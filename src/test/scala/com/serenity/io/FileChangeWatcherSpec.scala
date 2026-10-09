package com.serenity.io

import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers `FileChangeWatcher` (#1623): the real `java.nio.file.WatchService`-backed directory watcher that
  * `AppRuntime`'s background external-change loop polls. `sync` keeps the watched directory set matching the caller's
  * current interest (open local buffers' parent directories); `pollChangedFiles` reports which watched files a
  * subsequent poll window saw change.
  */
class FileChangeWatcherSpec extends AnyFlatSpec with Matchers:

  private val PollWindow = 3.seconds

  "pollChangedFiles" should "report a file written into a synced directory" in {
    val directory = Files.createTempDirectory("file-change-watcher")
    val file      = directory.resolve("watched.txt")
    Files.writeString(file, "initial")

    FileChangeWatcher.create
      .use { watcher =>
        for
          _       <- watcher.sync(Set(directory))
          _       <- cats.effect.IO.blocking(Files.writeString(file, "changed"))
          changed <- watcher.pollChangedFiles(PollWindow)
        yield changed shouldBe Set(file)
      }
      .unsafeRunSync()
  }

  it should "report nothing when no synced directory changes within the poll window" in {
    val directory = Files.createTempDirectory("file-change-watcher-quiet")

    FileChangeWatcher.create
      .use { watcher =>
        for
          _       <- watcher.sync(Set(directory))
          changed <- watcher.pollChangedFiles(1.second)
        yield changed shouldBe Set.empty
      }
      .unsafeRunSync()
  }

  it should "stop reporting changes in a directory once it is dropped from the synced set" in {
    val directory = Files.createTempDirectory("file-change-watcher-drop")
    val file      = directory.resolve("watched.txt")
    Files.writeString(file, "initial")

    FileChangeWatcher.create
      .use { watcher =>
        for
          _       <- watcher.sync(Set(directory))
          _       <- watcher.sync(Set.empty)
          _       <- cats.effect.IO.blocking(Files.writeString(file, "changed"))
          changed <- watcher.pollChangedFiles(1.second)
        yield changed shouldBe Set.empty
      }
      .unsafeRunSync()
  }

  it should "track changes across multiple synced directories independently" in {
    val directoryA = Files.createTempDirectory("file-change-watcher-a")
    val directoryB = Files.createTempDirectory("file-change-watcher-b")
    val fileA      = directoryA.resolve("a.txt")
    val fileB      = directoryB.resolve("b.txt")
    Files.writeString(fileA, "initial")
    Files.writeString(fileB, "initial")

    FileChangeWatcher.create
      .use { watcher =>
        for
          _       <- watcher.sync(Set(directoryA, directoryB))
          _       <- cats.effect.IO.blocking(Files.writeString(fileB, "changed"))
          changed <- watcher.pollChangedFiles(PollWindow)
        yield changed shouldBe Set(fileB)
      }
      .unsafeRunSync()
  }

  "awaitChangedFiles" should "report a burst of writes within the settle window as one change (#1885)" in {
    val directory = Files.createTempDirectory("file-change-watcher-burst")
    val file      = directory.resolve("watched.txt")
    Files.writeString(file, "initial")

    val writeBurst =
      (1 to 5).toList.traverse_(n => IO.blocking(Files.writeString(file, s"write $n")) >> IO.sleep(20.millis))

    FileChangeWatcher.create
      .use { watcher =>
        for
          _       <- watcher.sync(Set(directory))
          _       <- writeBurst.start
          changed <- watcher.awaitChangedFiles(300.millis)
          after   <- watcher.pollChangedFiles(500.millis)
        yield (changed, after)
      }
      .unsafeRunTimed(10.seconds) shouldBe Some((Set(file), Set.empty))
  }

  "The polling fallback" should "report a file written into a synced directory" in {
    val directory = Files.createTempDirectory("file-change-watcher-polling")
    val file      = directory.resolve("watched.txt")
    Files.writeString(file, "initial")

    FileChangeWatcher
      .polling(100.millis)
      .use { watcher =>
        for
          _       <- watcher.sync(Set(directory))
          _       <- IO.blocking(Files.writeString(file, "changed, and longer"))
          changed <- watcher.awaitChangedFiles(50.millis)
        yield changed shouldBe Set(file)
      }
      .unsafeRunSync()
  }

  it should "report nothing when no synced directory changes" in {
    val directory = Files.createTempDirectory("file-change-watcher-polling-quiet")
    Files.writeString(directory.resolve("watched.txt"), "initial")

    FileChangeWatcher
      .polling(100.millis)
      .use(watcher => watcher.sync(Set(directory)) >> watcher.pollChangedFiles(500.millis))
      .unsafeRunSync() shouldBe Set.empty
  }

  it should "report a burst of writes within the settle window as one change (#1885)" in {
    val directory = Files.createTempDirectory("file-change-watcher-polling-burst")
    val file      = directory.resolve("watched.txt")
    Files.writeString(file, "initial")

    val writeBurst =
      (1 to 5).toList.traverse_(n => IO.blocking(Files.writeString(file, s"write $n")) >> IO.sleep(20.millis))

    FileChangeWatcher
      .polling(100.millis)
      .use { watcher =>
        for
          _       <- watcher.sync(Set(directory))
          _       <- writeBurst.start
          changed <- watcher.awaitChangedFiles(300.millis)
          after   <- watcher.pollChangedFiles(500.millis)
        yield (changed, after)
      }
      .unsafeRunTimed(10.seconds) shouldBe Some((Set(file), Set.empty))
  }

  "isJdkPollingService" should "recognise the JDK's built-in poller, which macOS gets instead of a native watcher" in {
    FileChangeWatcher.isJdkPollingService("sun.nio.fs.PollingWatchService") shouldBe true
  }

  it should "accept the native watchers" in {
    List("sun.nio.fs.LinuxWatchService", "sun.nio.fs.WindowsWatchService", "sun.nio.fs.BsdWatchService")
      .map(FileChangeWatcher.isJdkPollingService) shouldBe List(false, false, false)
  }

  "DefaultPollInterval" should "stay well under the JDK poller's 10 seconds it replaces" in {
    FileChangeWatcher.DefaultPollInterval should be <= 2.seconds
  }

  "sync" should "be idempotent when called repeatedly with the same directory" in {
    val directory = Files.createTempDirectory("file-change-watcher-idempotent")
    val file      = directory.resolve("watched.txt")
    Files.writeString(file, "initial")

    FileChangeWatcher.create
      .use { watcher =>
        for
          _       <- watcher.sync(Set(directory))
          _       <- watcher.sync(Set(directory))
          _       <- cats.effect.IO.blocking(Files.writeString(file, "changed"))
          changed <- watcher.pollChangedFiles(PollWindow)
        yield changed shouldBe Set(file)
      }
      .unsafeRunSync()
  }
