package com.serenity.ui.presets

import java.nio.charset.StandardCharsets
import java.nio.channels.FileChannel
import java.nio.file.*
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.ReentrantLock

import cats.effect.IO
import com.serenity.io.AtomicFileWriter
import io.circe.*
import io.circe.generic.semiauto.deriveEncoder
import io.circe.parser.decode
import io.circe.syntax.*

final case class UiPresetIndex(presets: List[UiPreset], unknownFields: JsonObject = JsonObject.empty):

  def upsert(preset: UiPreset): UiPresetIndex =
    val existing = find(preset.name)
    val preserved =
      preset.copy(
        unknownFields = existing.fold(preset.unknownFields)(_.unknownFields.deepMerge(preset.unknownFields)),
        configUnknownFields = existing.fold(preset.configUnknownFields)(existing =>
          Json
            .fromJsonObject(existing.configUnknownFields)
            .deepMerge(Json.fromJsonObject(preset.configUnknownFields))
            .asObject
            .getOrElse(JsonObject.empty)
        )
      )
    copy(presets = presets.filterNot(item => UiPreset.nameKey(item.name) == UiPreset.nameKey(preset.name)) :+ preserved)

  def delete(name: String): UiPresetIndex =
    copy(presets = presets.filterNot(existing => UiPreset.nameKey(existing.name) == UiPreset.nameKey(name)))

  def rename(sourceName: String, targetName: String): UiPresetIndex =
    val normalizedTarget = targetName.trim
    find(sourceName)
      .filter(_ => normalizedTarget.nonEmpty)
      .map(preset => delete(sourceName).upsert(preset.copy(name = normalizedTarget)))
      .getOrElse(this)

  def duplicate(sourceName: String, targetName: String): UiPresetIndex =
    val normalizedTarget = targetName.trim
    find(sourceName)
      .filter(_ => normalizedTarget.nonEmpty)
      .map(preset => upsert(preset.copy(name = normalizedTarget)))
      .getOrElse(this)

  def find(name: String): Option[UiPreset] =
    presets.find(existing => UiPreset.nameKey(existing.name) == UiPreset.nameKey(name))

  def names: List[String] =
    presets.map(_.name)

object UiPresetIndex:
  val empty: UiPresetIndex = UiPresetIndex(Nil)

  private given rawUiPresetIndexEncoder: Encoder.AsObject[UiPresetIndex] = deriveEncoder

  given Encoder[UiPresetIndex] = Encoder.AsObject.instance { index =>
    index.unknownFields.deepMerge(rawUiPresetIndexEncoder.encodeObject(index).remove("unknownFields"))
  }

  given Decoder[UiPresetIndex] = Decoder.instance { cursor =>
    cursor.get[List[UiPreset]]("presets").map { presets =>
      val knownKeys = Set("presets")
      val unknown = cursor.value.asObject.fold(JsonObject.empty)(objectValue =>
        JsonObject.fromIterable(objectValue.toIterable.filterNot((key, _) => knownKeys.contains(key)))
      )
      UiPresetIndex(presets, unknown)
    }
  }

class UiPresetStore private (path: Path):
  import UiPresetIndex.given

  private val mutationLockPath = path.resolveSibling(s".${path.getFileName.toString}.lock")

  private def withExclusiveMutationLock[A](operation: IO[A]): IO[A] =
    val processLock = UiPresetStore.inProcessLock(mutationLockPath)
    IO.blocking(processLock.lock())
      .bracket { _ =>
        IO.blocking {
          Option(mutationLockPath.getParent).foreach(Files.createDirectories(_))
          FileChannel.open(mutationLockPath, StandardOpenOption.CREATE, StandardOpenOption.WRITE)
        }.bracket { channel =>
          IO.blocking(channel.lock()).bracket(_ => operation)(fileLock => IO.blocking(fileLock.release()))
        }(channel => IO.blocking(channel.close()))
      }(_ => IO.blocking(processLock.unlock()))

  def load(): IO[UiPresetIndex] =
    IO.blocking(Files.exists(path)).flatMap {
      case false => IO.pure(UiPresetIndex.empty)
      case true =>
        IO.blocking(Files.readString(path, StandardCharsets.UTF_8)).flatMap { json =>
          IO.fromEither(decode[UiPresetIndex](json))
        }
    }

  private def saveUnlocked(index: UiPresetIndex): IO[Unit] =
    IO.fromEither(validateIndex(index)).flatMap { validIndex =>
      AtomicFileWriter.writeString(path, validIndex.asJson.spaces2)
    }

  def save(index: UiPresetIndex): IO[Unit] =
    withExclusiveMutationLock(saveUnlocked(index))

  def upsert(preset: UiPreset): IO[Unit] =
    withExclusiveMutationLock {
      load().flatMap(index =>
        IO.fromEither(validateForUpsert(preset, index)).flatMap(valid => saveUnlocked(index.upsert(valid)))
      )
    }

  /** Creates a new custom preset and rejects any existing normalized name. */
  def create(preset: UiPreset): IO[Unit] =
    withExclusiveMutationLock {
      load().flatMap { index =>
        IO.raiseWhen(index.find(preset.name).nonEmpty)(
          new IllegalArgumentException(s"Preset name '${preset.name}' already exists")
        ) >>
          IO.fromEither(validateForUpsert(preset, index))
            .flatMap(valid => saveUnlocked(index.copy(presets = index.presets :+ valid)))
      }
    }

  def delete(name: String): IO[Unit] =
    withExclusiveMutationLock(load().flatMap(index => saveUnlocked(index.delete(name))))

  def rename(sourceName: String, targetName: String): IO[Unit] =
    withExclusiveMutationLock {
      load().flatMap { index =>
        for
          source <- IO.fromOption(index.find(sourceName))(
            new IllegalArgumentException(s"Preset '$sourceName' does not exist")
          )
          _ <- IO.raiseWhen(index.find(targetName).exists(_ != source))(
            new IllegalArgumentException(s"Preset name '$targetName' already exists")
          )
          renamed <- IO.fromEither(
            validateForUpsert(
              source.copy(name = targetName),
              index.copy(presets = index.presets.filterNot(_ == source))
            )
          )
          _ <- saveUnlocked(index.copy(presets = index.presets.filterNot(_ == source) :+ renamed))
        yield ()
      }
    }

  def duplicate(sourceName: String, targetName: String): IO[Unit] =
    withExclusiveMutationLock {
      load().flatMap { index =>
        for
          source <- IO.fromOption(index.find(sourceName))(
            new IllegalArgumentException(s"Preset '$sourceName' does not exist")
          )
          _ <- IO.raiseWhen(index.find(targetName).nonEmpty)(
            new IllegalArgumentException(s"Preset name '$targetName' already exists")
          )
          copy <- IO.fromEither(validateForUpsert(source.copy(name = targetName), index))
          _    <- saveUnlocked(index.copy(presets = index.presets :+ copy))
        yield ()
      }
    }

  def find(name: String): IO[Option[UiPreset]] =
    load().map(_.find(name))

  def list(): IO[List[UiPreset]] =
    load().map(_.presets)

  private def validateForUpsert(preset: UiPreset, index: UiPresetIndex): Either[IllegalArgumentException, UiPreset] =
    val name = UiPreset.normalizedName(preset.name)
    Either
      .cond(
        name.nonEmpty && !name.exists(ch => ch == '/' || ch == '\\' || ch == 0) &&
          name != "." && name != ".." && UiPreset.builtIn(name).isEmpty,
        preset.copy(name = name),
        new IllegalArgumentException("Preset name must be a non-built-in, non-path-like name")
      )
      .flatMap { valid =>
        index.find(valid.name) match
          case Some(existing) if existing.name != preset.name =>
            Left(new IllegalArgumentException(s"Preset name collides with existing preset '${existing.name}'"))
          case _ => Right(valid)
      }

  private def validateIndex(index: UiPresetIndex): Either[IllegalArgumentException, UiPresetIndex] =
    index.presets
      .foldLeft[Either[IllegalArgumentException, List[UiPreset]]](Right(Nil)) { (validated, preset) =>
        validated.flatMap(accepted => validateForUpsert(preset, UiPresetIndex(accepted)).map(accepted :+ _))
      }
      .map(presets => UiPresetIndex(presets, index.unknownFields))

object UiPresetStore:
  val defaultPath: Path = Paths.get(System.getProperty("user.home"), ".serenity", "ui-presets.json")

  private val inProcessLocks = new ConcurrentHashMap[Path, ReentrantLock]()

  private def inProcessLock(path: Path): ReentrantLock =
    inProcessLocks.computeIfAbsent(path.toAbsolutePath.normalize, _ => new ReentrantLock())

  def apply(path: Path): UiPresetStore =
    new UiPresetStore(path)

  def default: UiPresetStore =
    UiPresetStore(defaultPath)
