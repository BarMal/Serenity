package com.serenity.io

import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers `FileChangeWatcher` (#1623): the real `java.nio.file.WatchService`-backed directory watcher that
  * `AppRuntime`'s background external-change loop polls. `sync` keeps the watched directory set matching the
  * caller's current interest (open local buffers' parent directories); `pollChangedFiles` reports which watched
  * files a subsequent poll window saw change.
  */
class FileChangeWatcherSpec extends AnyFlatSpec with Matchers:

  private val PollWindow = 3.seconds

  "pollChangedFiles" should "report a file written into a synced directory" in {
    val directory = Files.createTempDirectory("file-change-watcher")
    val file      = directory.resolve("watched.txt")
    Files.writeString(file, "initial")

    FileChangeWatcher.create.use { watcher =>
      for
        _       <- watcher.sync(Set(directory))
        _       <- cats.effect.IO.blocking(Files.writeString(file, "changed"))
        changed <- watcher.pollChangedFiles(PollWindow)
      yield changed shouldBe Set(file)
    }.unsafeRunSync()
  }

  it should "report nothing when no synced directory changes within the poll window" in {
    val directory = Files.createTempDirectory("file-change-watcher-quiet")

    FileChangeWatcher.create.use { watcher =>
      for
        _       <- watcher.sync(Set(directory))
        changed <- watcher.pollChangedFiles(1.second)
      yield changed shouldBe Set.empty
    }.unsafeRunSync()
  }

  it should "stop reporting changes in a directory once it is dropped from the synced set" in {
    val directory = Files.createTempDirectory("file-change-watcher-drop")
    val file      = directory.resolve("watched.txt")
    Files.writeString(file, "initial")

    FileChangeWatcher.create.use { watcher =>
      for
        _       <- watcher.sync(Set(directory))
        _       <- watcher.sync(Set.empty)
        _       <- cats.effect.IO.blocking(Files.writeString(file, "changed"))
        changed <- watcher.pollChangedFiles(1.second)
      yield changed shouldBe Set.empty
    }.unsafeRunSync()
  }

  it should "track changes across multiple synced directories independently" in {
    val directoryA = Files.createTempDirectory("file-change-watcher-a")
    val directoryB = Files.createTempDirectory("file-change-watcher-b")
    val fileA      = directoryA.resolve("a.txt")
    val fileB      = directoryB.resolve("b.txt")
    Files.writeString(fileA, "initial")
    Files.writeString(fileB, "initial")

    FileChangeWatcher.create.use { watcher =>
      for
        _       <- watcher.sync(Set(directoryA, directoryB))
        _       <- cats.effect.IO.blocking(Files.writeString(fileB, "changed"))
        changed <- watcher.pollChangedFiles(PollWindow)
      yield changed shouldBe Set(fileB)
    }.unsafeRunSync()
  }

  "sync" should "be idempotent when called repeatedly with the same directory" in {
    val directory = Files.createTempDirectory("file-change-watcher-idempotent")
    val file      = directory.resolve("watched.txt")
    Files.writeString(file, "initial")

    FileChangeWatcher.create.use { watcher =>
      for
        _       <- watcher.sync(Set(directory))
        _       <- watcher.sync(Set(directory))
        _       <- cats.effect.IO.blocking(Files.writeString(file, "changed"))
        changed <- watcher.pollChangedFiles(PollWindow)
      yield changed shouldBe Set(file)
    }.unsafeRunSync()
  }
