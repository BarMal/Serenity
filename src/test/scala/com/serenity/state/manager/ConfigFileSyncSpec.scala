package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.config.AppConfigOps.*
import com.serenity.config.{AppConfig, ConfigError, ConfigLoadResult, ConfigMigrationReport}
import com.serenity.io.FileStamp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[ConfigFileSync]] over a file that is only a stamp and a result to load, so what counts as "our own write" and what
  * as "an edit" is decided by stamps alone, with no filesystem timestamp resolution in the way.
  */
class ConfigFileSyncSpec extends AnyFlatSpec with Matchers:

  private val path    = Path.of("/virtual/config.conf")
  private val initial = AppConfig.default

  final private class FakeFile(
      stamp: Ref[IO, Option[FileStamp]],
      contents: Ref[IO, Either[ConfigError, ConfigLoadResult]],
      val saved: Ref[IO, List[AppConfig]],
      val loads: Ref[IO, Int],
      val vouching: Ref[IO, Boolean],
      failNextSave: Ref[IO, Boolean],
      edits: Ref[IO, Long]
  ):
    private def stampOf(size: Long) = FileStamp(size, size * 1000L, None)

    def sync(startingConfig: Option[AppConfig]): ConfigFileSync =
      ConfigFileSync.unsafe(
        path,
        startingConfig,
        save = (config, _) =>
          failNextSave.getAndSet(false).flatMap { fails =>
            if fails then IO.pure(Left(ConfigError("save", path, "disk full")))
            else
              saved.update(_ :+ config) >> contents.set(Right(ConfigLoadResult(config, ConfigMigrationReport.empty))) >>
                touch.as(Right(()))
          },
        load = _ => loads.update(_ + 1) >> contents.get,
        observe = _ => (stamp.get, vouching.get).mapN((current, vouches) => current.map(FileStamp.Observed(_, vouches)))
      )

    private def touch: IO[Unit] = edits.updateAndGet(_ + 1).flatMap(count => stamp.set(Some(stampOf(count))))

    def editExternally(result: Either[ConfigError, ConfigLoadResult]): IO[Unit] = contents.set(result) >> touch

    def remove: IO[Unit] = stamp.set(None)

    def failNext: IO[Unit] = failNextSave.set(true)

  private def fakeFile: FakeFile =
    (
      Ref.of[IO, Option[FileStamp]](None),
      Ref.of[IO, Either[ConfigError, ConfigLoadResult]](Right(ConfigLoadResult(initial, ConfigMigrationReport.empty))),
      Ref.of[IO, List[AppConfig]](Nil),
      Ref.of[IO, Int](0),
      Ref.of[IO, Boolean](true),
      Ref.of[IO, Boolean](false),
      Ref.of[IO, Long](0L)
    ).mapN(new FakeFile(_, _, _, _, _, _, _)).unsafeRunSync()

  private def edited(config: AppConfig): Either[ConfigError, ConfigLoadResult] =
    Right(ConfigLoadResult(config, ConfigMigrationReport.empty))

  "Writing" should "skip a config whose encoding is the one the file already holds" in {
    val file = fakeFile
    val sync = file.sync(Some(initial))

    sync.writeIfChanged(initial).unsafeRunSync() shouldBe Right(())
    sync.writeIfChanged(initial.withWheelScrollLines(7)).unsafeRunSync() shouldBe Right(())
    sync.writeIfChanged(initial.withWheelScrollLines(7)).unsafeRunSync() shouldBe Right(())

    file.saved.get.unsafeRunSync().map(_.inputConfig.wheelScrollLines) shouldBe List(7)
  }

  it should "write when nothing is known about what the file holds" in {
    val file = fakeFile
    val sync = file.sync(None)

    sync.writeIfChanged(initial).unsafeRunSync()

    file.saved.get.unsafeRunSync() shouldBe List(initial)
  }

  it should "write an explicit save even when the config looks unchanged" in {
    val file = fakeFile
    val sync = file.sync(Some(initial))

    sync.write(initial).unsafeRunSync()

    file.saved.get.unsafeRunSync() shouldBe List(initial)
  }

  it should "try again after a failed write rather than assume the file is up to date" in {
    val file    = fakeFile
    val sync    = file.sync(Some(initial))
    val changed = initial.withWheelScrollLines(7)

    file.failNext.unsafeRunSync()
    sync.writeIfChanged(changed).unsafeRunSync().isLeft shouldBe true
    sync.writeIfChanged(changed).unsafeRunSync() shouldBe Right(())

    file.saved.get.unsafeRunSync() shouldBe List(changed)
  }

  "Watching" should "not report the stamp this process's own write left" in {
    val file = fakeFile
    val sync = file.sync(Some(initial))

    sync.writeIfChanged(initial.withWheelScrollLines(7)).unsafeRunSync()

    sync.externalChange.unsafeRunSync() shouldBe None
    file.loads.get.unsafeRunSync() shouldBe 0
  }

  it should "report a write nobody here made, loaded" in {
    val file    = fakeFile
    val sync    = file.sync(Some(initial))
    val changed = initial.withWheelScrollLines(7)

    sync.writeIfChanged(initial.withWheelScrollLines(3)).unsafeRunSync()
    file.editExternally(edited(changed)).unsafeRunSync()

    sync.externalChange.unsafeRunSync().map {
      case ConfigFileChange.Edited(loaded, _) => Some(loaded.config)
      case _                                  => None
    } shouldBe Some(Some(changed))
  }

  it should "not read a file again once it has been read and left as it was" in {
    val file = fakeFile
    val sync = file.sync(Some(initial))

    file.editExternally(edited(initial.withWheelScrollLines(7))).unsafeRunSync()
    sync.externalChange.unsafeRunSync().foreach {
      case ConfigFileChange.Edited(loaded, stamp) => sync.adopt(loaded.config, stamp).unsafeRunSync()
      case _                                      => ()
    }

    sync.externalChange.unsafeRunSync() shouldBe None
    file.loads.get.unsafeRunSync() shouldBe 1
  }

  it should "read a file whose stamp cannot vouch for its content, even if the stamp is the one already seen" in {
    val file = fakeFile
    val sync = file.sync(Some(initial))

    sync.writeIfChanged(initial.withWheelScrollLines(7)).unsafeRunSync()
    file.vouching.set(false).unsafeRunSync()

    sync.externalChange.unsafeRunSync().isDefined shouldBe true
  }

  it should "report an unreadable file so the settings in use can be kept" in {
    val file = fakeFile
    val sync = file.sync(Some(initial))

    file.editExternally(Left(ConfigError("load", path, "broken"))).unsafeRunSync()

    sync.externalChange.unsafeRunSync().map {
      case ConfigFileChange.Unreadable(error, _) => error.message
      case _                                     => ""
    } shouldBe Some("broken")
  }

  it should "report nothing for a file that is gone" in {
    val file = fakeFile
    val sync = file.sync(Some(initial))

    file.editExternally(edited(initial.withWheelScrollLines(7))).unsafeRunSync()
    file.remove.unsafeRunSync()

    sync.externalChange.unsafeRunSync() shouldBe None
  }

  "Forgetting what the file holds" should "make the next write go through" in {
    val file = fakeFile
    val sync = file.sync(Some(initial))

    sync.forget.unsafeRunSync()
    sync.writeIfChanged(initial).unsafeRunSync()

    file.saved.get.unsafeRunSync() shouldBe List(initial)
  }
