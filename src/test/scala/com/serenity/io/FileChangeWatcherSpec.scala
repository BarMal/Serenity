package com.serenity.io

import java.nio.file.StandardWatchEventKinds.{ENTRY_MODIFY, OVERFLOW}
import java.nio.file.{Files, Path, WatchEvent, WatchKey, Watchable}
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.TestTemp
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
    val directory = TestTemp.directory("file-change-watcher")
    val file      = directory.resolve("watched.txt")
    Files.writeString(file, "initial")

    FileChangeWatcher.create
      .use { watcher =>
        for
          _       <- watcher.sync(Set(directory))
          _       <- cats.effect.IO.blocking(Files.writeString(file, "changed"))
          changed <- watcher.pollChangedFiles(PollWindow)
        yield changed shouldBe WatchedChanges.files(Set(file))
      }
      .unsafeRunSync()
  }

  it should "report nothing when no synced directory changes within the poll window" in {
    val directory = TestTemp.directory("file-change-watcher-quiet")

    FileChangeWatcher.create
      .use { watcher =>
        for
          _       <- watcher.sync(Set(directory))
          changed <- watcher.pollChangedFiles(1.second)
        yield changed shouldBe WatchedChanges.none
      }
      .unsafeRunSync()
  }

  it should "stop reporting changes in a directory once it is dropped from the synced set" in {
    val directory = TestTemp.directory("file-change-watcher-drop")
    val file      = directory.resolve("watched.txt")
    Files.writeString(file, "initial")

    FileChangeWatcher.create
      .use { watcher =>
        for
          _       <- watcher.sync(Set(directory))
          _       <- watcher.sync(Set.empty)
          _       <- cats.effect.IO.blocking(Files.writeString(file, "changed"))
          changed <- watcher.pollChangedFiles(1.second)
        yield changed shouldBe WatchedChanges.none
      }
      .unsafeRunSync()
  }

  it should "track changes across multiple synced directories independently" in {
    val directoryA = TestTemp.directory("file-change-watcher-a")
    val directoryB = TestTemp.directory("file-change-watcher-b")
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
        yield changed shouldBe WatchedChanges.files(Set(fileB))
      }
      .unsafeRunSync()
  }

  "awaitChangedFiles" should "report a burst of writes within the settle window as one change (#1885)" in {
    val directory = TestTemp.directory("file-change-watcher-burst")
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
      .unsafeRunTimed(10.seconds) shouldBe Some((WatchedChanges.files(Set(file)), WatchedChanges.none))
  }

  "The polling fallback" should "report a file written into a synced directory" in {
    val directory = TestTemp.directory("file-change-watcher-polling")
    val file      = directory.resolve("watched.txt")
    Files.writeString(file, "initial")

    FileChangeWatcher
      .polling(100.millis)
      .use { watcher =>
        for
          _       <- watcher.sync(Set(directory))
          _       <- IO.blocking(Files.writeString(file, "changed, and longer"))
          changed <- watcher.awaitChangedFiles(50.millis)
        yield changed shouldBe WatchedChanges.files(Set(file))
      }
      .unsafeRunSync()
  }

  it should "report nothing when no synced directory changes" in {
    val directory = TestTemp.directory("file-change-watcher-polling-quiet")
    Files.writeString(directory.resolve("watched.txt"), "initial")

    FileChangeWatcher
      .polling(100.millis)
      .use(watcher => watcher.sync(Set(directory)) >> watcher.pollChangedFiles(500.millis))
      .unsafeRunSync() shouldBe WatchedChanges.none
  }

  it should "report a burst of writes within the settle window as one change (#1885)" in {
    val directory = TestTemp.directory("file-change-watcher-polling-burst")
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
      .unsafeRunTimed(10.seconds) shouldBe Some((WatchedChanges.files(Set(file)), WatchedChanges.none))
  }

  it should "account for every file of a burst too large for the platform's event queue" in {
    // The JDK keeps at most 512 pending events per watched directory and replaces them with one OVERFLOW (#2531 W3);
    // a polling backend sees every file instead. Either way no file may go unreported.
    val directory = TestTemp.directory("file-change-watcher-overflow")
    val burst     = (1 to 2000).map(n => directory.resolve(s"burst-$n.txt")).toSet

    FileChangeWatcher.create
      .use { watcher =>
        for
          _       <- watcher.sync(Set(directory))
          _       <- IO.blocking(burst.foreach(Files.createFile(_)))
          changed <- watcher.pollChangedFiles(PollWindow)
        yield burst.filterNot(file => changed.files(file) || changed.overflowed(file.getParent)) shouldBe empty
      }
      .unsafeRunSync()
  }

  "drainKey" should "report the directory whose events overflowed, which the JDK reports with no file name" in {
    val directory = Path.of("project", "src")
    val key       = FakeWatchKey(directory, List(FakeEvent(ENTRY_MODIFY, Path.of("a.txt")), FakeEvent(OVERFLOW, null)))

    FileChangeWatcher.drainKey(key).unsafeRunSync() shouldBe
      WatchedChanges(files = Set(directory.resolve("a.txt")), overflowed = Set(directory))
    key.resets shouldBe 1
  }

  "WatchedChanges" should "combine the files and the overflowed directories of two reports" in {
    val a = Path.of("a")
    val b = Path.of("b")
    WatchedChanges.files(Set(a)) ++ WatchedChanges(Set.empty, Set(b)) shouldBe WatchedChanges(Set(a), Set(b))
    WatchedChanges.none.isEmpty shouldBe true
    WatchedChanges(Set.empty, Set(b)).isEmpty shouldBe false
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
    val directory = TestTemp.directory("file-change-watcher-idempotent")
    val file      = directory.resolve("watched.txt")
    Files.writeString(file, "initial")

    FileChangeWatcher.create
      .use { watcher =>
        for
          _       <- watcher.sync(Set(directory))
          _       <- watcher.sync(Set(directory))
          _       <- cats.effect.IO.blocking(Files.writeString(file, "changed"))
          changed <- watcher.pollChangedFiles(PollWindow)
        yield changed shouldBe WatchedChanges.files(Set(file))
      }
      .unsafeRunSync()
  }

// The JDK's OVERFLOW event carries a null context, so a fake event must be able to as well.
final private class FakeEvent[A](kindOf: WatchEvent.Kind[A], contextOf: A) extends WatchEvent[A]:
  def kind: WatchEvent.Kind[A] = kindOf
  def count: Int               = 1
  def context: A               = contextOf

final private class FakeWatchKey(directory: Path, events: List[WatchEvent[?]]) extends WatchKey:
  private val resetCount                        = AtomicInteger(0)
  def resets: Int                               = resetCount.get
  def isValid: Boolean                          = true
  def pollEvents: java.util.List[WatchEvent[?]] = events.asJava
  def reset: Boolean                            = resetCount.incrementAndGet() > 0
  def cancel(): Unit                            = ()
  def watchable: Watchable                      = directory
