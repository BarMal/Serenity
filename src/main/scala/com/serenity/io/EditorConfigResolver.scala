package com.serenity.io

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.HexFormat
import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec
import scala.util.Try

import cats.effect.IO
import com.serenity.state.models.IndentationHint

/** Finds the `.editorconfig` files that speak for a path: those in its directory and every directory above it, up to
  * the first that says `root = true`. Each directory's file is parsed once and kept under a hash of its bytes; every
  * lookup reads it again and parses it only if the hash changed, so an edited file is seen the next time a document is
  * opened or reloaded, with no watcher involved. The hash, not the file's timestamp and size, is what says the file is
  * unchanged: a same-length edit saved within one timestamp tick, which a coarse filesystem clock makes ordinary,
  * leaves both of those as they were (#2348). These files are a few hundred bytes, so the read is cheap.
  */
final class EditorConfigResolver:

  final private case class Cached(contentHash: String, file: EditorConfigFile)

  private val cache = AtomicReference(Map.empty[Path, Cached])

  /** What the files say about `path`'s indentation. A file that cannot be read says nothing. */
  def indentationFor(path: Path): IO[IndentationHint] =
    IO.blocking {
      val absolute = path.toAbsolutePath.normalize
      EditorConfig.indentation(EditorConfig.propertiesFor(layersFor(absolute), absolute))
    }.handleError(_ => IndentationHint.Empty)

  // Farthest directory first, so that the nearer file is applied last.
  private def layersFor(path: Path): List[EditorConfig.Layer] =
    @tailrec def climb(directory: Option[Path], layers: List[EditorConfig.Layer]): List[EditorConfig.Layer] =
      directory match
        case None => layers
        case Some(current) =>
          val found = fileIn(current)
          val next  = found.fold(layers)(EditorConfig.Layer(current, _) :: layers)
          if found.exists(_.root) then next else climb(Option(current.getParent), next)
    climb(Option(path.getParent), Nil)

  private def fileIn(directory: Path): Option[EditorConfigFile] =
    val location = directory.resolve(EditorConfig.FileName)
    if !Files.isRegularFile(location) then
      val _ = cache.updateAndGet(_ - directory)
      None
    else
      Try(Files.readAllBytes(location)).toOption.flatMap { bytes =>
        val contentHash = hashOf(bytes)
        cache.get.get(directory).filter(_.contentHash == contentHash) match
          case Some(cached) => Some(cached.file)
          case None         =>
            // Strict, as `Files.readString` is: a file that is not valid UTF-8 says nothing.
            Try(StandardCharsets.UTF_8.newDecoder.decode(ByteBuffer.wrap(bytes)).toString).toOption.map { text =>
              val file = EditorConfig.parse(text)
              val _    = cache.updateAndGet(_.updated(directory, Cached(contentHash, file)))
              file
            }
      }

  private def hashOf(bytes: Array[Byte]): String =
    HexFormat.of.formatHex(MessageDigest.getInstance("SHA-256").digest(bytes))
