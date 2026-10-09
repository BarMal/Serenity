package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.config.{AppConfig, ConfigError, ConfigLoadResult, ConfigManager}
import com.serenity.io.FileStamp

/** The config file and the live config that are known to agree: `config` is what the file meant when `stamp` was taken,
  * and `encoded` is that config as it would be written, so "has anything to write" is one string comparison.
  */
final private[manager] case class ConfigOnDisk(config: AppConfig, encoded: String, stamp: Option[FileStamp])

private[manager] object ConfigOnDisk:
  def of(config: AppConfig, stamp: Option[FileStamp]): ConfigOnDisk =
    ConfigOnDisk(config, ConfigManager.configToString(config), stamp)

/** A write to the config file by someone other than this process. */
private[manager] enum ConfigFileChange:
  case Edited(loaded: ConfigLoadResult, stamp: FileStamp)
  case Unreadable(error: ConfigError, stamp: FileStamp)

/** Keeps the config file and the live config from fighting over each other.
  *
  * Writes are skipped when the encoded config is the one already on disk. Every write records the stamp it left behind,
  * so the watcher reporting this process's own write finds a stamp it already knows and ignores it; only a stamp nobody
  * here produced is an external edit. Both run on the config lane, which orders a reload after any write queued before
  * it.
  *
  * `known` is `None` while it is unknown what the file holds relative to the live config: before the first sync, and
  * after a write failed, when the live config holds changes the file never received. An external edit then wins
  * outright, since there is nothing here worth protecting from it.
  */
final private[manager] class ConfigFileSync private (
    val path: Path,
    known: Ref[IO, Option[ConfigOnDisk]],
    save: (AppConfig, Path) => IO[Either[ConfigError, Unit]],
    load: Path => IO[Either[ConfigError, ConfigLoadResult]],
    observe: Path => IO[Option[FileStamp.Observed]]
):

  def onDisk: IO[Option[ConfigOnDisk]] = known.get

  def writeIfChanged(config: AppConfig): IO[Either[ConfigError, Unit]] =
    known.get.flatMap { disk =>
      if disk.exists(_.encoded == ConfigManager.configToString(config)) then IO.pure(Right(()))
      else write(config)
    }

  /** Writes `config` whether or not it looks changed: an explicit save, or a file that was moved aside. */
  def write(config: AppConfig): IO[Either[ConfigError, Unit]] =
    save(config, path).flatMap {
      case Right(_) =>
        stampOfFile.flatMap(stamp => known.set(Some(ConfigOnDisk.of(config, stamp)))).as(Right(()))
      case failed => known.set(None).as(failed)
    }

  /** The file as it now is, if its stamp is not one this process left or already read. A file that is gone, or not a
    * regular file, is no change: the live settings stay and the next save writes the file again.
    */
  def externalChange: IO[Option[ConfigFileChange]] =
    (known.get, observe(path).handleError(_ => None)).flatMapN {
      case (_, None) => IO.pure(None)
      case (disk, Some(observed)) if disk.exists(_.stamp.contains(observed.stamp)) && observed.vouches =>
        IO.pure(None)
      case (_, Some(observed)) =>
        load(path).map {
          case Left(error)   => Some(ConfigFileChange.Unreadable(error, observed.stamp))
          case Right(loaded) => Some(ConfigFileChange.Edited(loaded, observed.stamp))
        }
    }

  /** Records that the file at `stamp` was read and matches the live config. */
  def adopt(config: AppConfig, stamp: FileStamp): IO[Unit] =
    known.set(Some(ConfigOnDisk.of(config, Some(stamp))))

  /** Records that the file at `stamp` was read and left alone, so it is not read again until it changes. */
  def noteSeen(stamp: FileStamp): IO[Unit] =
    known.update(_.map(_.copy(stamp = Some(stamp))))

  /** Makes the next write go through even if the config looks unchanged. */
  def forget: IO[Unit] = known.set(None)

  private def stampOfFile: IO[Option[FileStamp]] =
    observe(path).map(_.map(_.stamp)).handleError(_ => None)

private[manager] object ConfigFileSync:

  val defaultLoad: Path => IO[Either[ConfigError, ConfigLoadResult]] =
    file => ConfigManager.loadConfigResultIO(Some(file.toString))

  /** `startingConfig` is the config the file is believed to hold; `None` when that is not known. */
  def unsafe(
    path: Path,
    startingConfig: Option[AppConfig],
    save: (AppConfig, Path) => IO[Either[ConfigError, Unit]],
    load: Path => IO[Either[ConfigError, ConfigLoadResult]] = defaultLoad,
    observe: Path => IO[Option[FileStamp.Observed]] = file => FileStamp.observe(file)
  ): ConfigFileSync =
    new ConfigFileSync(
      path,
      Ref.unsafe[IO, Option[ConfigOnDisk]](startingConfig.map(ConfigOnDisk.of(_, None))),
      save,
      load,
      observe
    )
