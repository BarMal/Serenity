package com.serenity

import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.AppRuntime
import com.serenity.io.FileChangeWatcher
import com.serenity.state.models.BufferId
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers `AppRuntime.externalChangeWatchLoop` (#1623): the background half of external-change detection, driven by a
  * real `FileChangeWatcher` rather than a fake, so this exercises the same directory-sync-then-poll-then-check cycle
  * the running app uses. `FileChangeWatcherSpec` covers the watcher's own sync/poll contract directly; this spec covers
  * the loop wiring that turns "a watched file changed" into "check this specific buffer."
  */
class AppRuntimeExternalChangeWatchSpec extends AnyFlatSpec with Matchers:

  "externalChangeWatchLoop" should "check the buffer whose watched file a poll cycle saw change" in {
    val file     = Files.createTempFile("external-change-watch", ".md")
    val bufferId = BufferId(1)

    val program = for
      checkedBuffers <- Ref.of[IO, List[BufferId]](Nil)
      _ <- FileChangeWatcher.create.use { watcher =>
        // Registering the directory before writing (rather than racing the loop's own first-cycle sync against
        // the write on a timer) makes this deterministic: a write that lands before registration would not be
        // captured as a WatchService event, regardless of how generous a sleep tried to avoid that ordering.
        for
          _ <- watcher.sync(Set(file.getParent))
          _ <- IO.blocking(Files.writeString(file, "changed externally"))
          _ <- AppRuntime
            .externalChangeWatchLoop(
              watcher,
              openBufferPaths = IO.pure(Map(file -> bufferId)),
              checkBufferForExternalChanges = id => checkedBuffers.update(_ :+ id),
              pollInterval = 5.seconds
            )
            .take(1)
            .compile
            .drain
        yield ()
      }
      checked <- checkedBuffers.get
    yield checked

    program.unsafeRunTimed(15.seconds) shouldBe Some(List(bufferId))
  }

  it should "not check any buffer when nothing changes within the poll window" in {
    val file     = Files.createTempFile("external-change-watch-quiet", ".md")
    val bufferId = BufferId(1)

    val program = for
      checkedBuffers <- Ref.of[IO, List[BufferId]](Nil)
      _ <- FileChangeWatcher.create.use { watcher =>
        AppRuntime
          .externalChangeWatchLoop(
            watcher,
            openBufferPaths = IO.pure(Map(file -> bufferId)),
            checkBufferForExternalChanges = id => checkedBuffers.update(_ :+ id),
            pollInterval = 1.second
          )
          .take(1)
          .compile
          .drain
      }
      checked <- checkedBuffers.get
    yield checked

    program.unsafeRunTimed(10.seconds) shouldBe Some(Nil)
  }

  it should "call refreshDictionaryFingerprints when a poll cycle sees a change under a watched dictionary directory" in {
    // #1691: a dictionary directory is watched the same way an open buffer's parent directory is -- via
    // FileChangeWatcher.sync/pollChangedFiles -- with no open buffers at all, proving the invalidation path doesn't
    // depend on any buffer being open (unlike the pre-existing focus-in-only backstop).
    val dictionaryDirectory = Files.createTempDirectory("external-change-watch-dictionary")

    val program = for
      refreshCount <- Ref.of[IO, Int](0)
      _ <- FileChangeWatcher.create.use { watcher =>
        for
          _ <- watcher.sync(Set(dictionaryDirectory))
          _ <- IO.blocking(Files.writeString(dictionaryDirectory.resolve("en.dic"), "changed externally"))
          _ <- AppRuntime
            .externalChangeWatchLoop(
              watcher,
              openBufferPaths = IO.pure(Map.empty),
              checkBufferForExternalChanges = _ => IO.unit,
              dictionaryWatchDirectories = IO.pure(Set(dictionaryDirectory)),
              refreshDictionaryFingerprints = refreshCount.update(_ + 1),
              pollInterval = 5.seconds
            )
            .take(1)
            .compile
            .drain
        yield ()
      }
      count <- refreshCount.get
    yield count

    program.unsafeRunTimed(15.seconds) shouldBe Some(1)
  }

  it should "not call refreshDictionaryFingerprints when nothing changes under the watched dictionary directory" in {
    val dictionaryDirectory = Files.createTempDirectory("external-change-watch-dictionary-quiet")

    val program = for
      refreshCount <- Ref.of[IO, Int](0)
      _ <- FileChangeWatcher.create.use { watcher =>
        AppRuntime
          .externalChangeWatchLoop(
            watcher,
            openBufferPaths = IO.pure(Map.empty),
            checkBufferForExternalChanges = _ => IO.unit,
            dictionaryWatchDirectories = IO.pure(Set(dictionaryDirectory)),
            refreshDictionaryFingerprints = refreshCount.update(_ + 1),
            pollInterval = 1.second
          )
          .take(1)
          .compile
          .drain
      }
      count <- refreshCount.get
    yield count

    program.unsafeRunTimed(10.seconds) shouldBe Some(0)
  }

  it should "stay virtual-time-compatible when spell-check is disabled and dictionaryWatchDirectories is empty" in {
    // Mirrors the buffer-less case below: SpellCheckConfig.dictionaryWatchDirectories returns Set.empty when
    // disabled (the default), so a real IO.blocking WatchService.poll must still never run here either.
    val checked = FileChangeWatcher.create.use { watcher =>
      for
        refreshCount <- Ref.of[IO, Int](0)
        _ <- AppRuntime
          .externalChangeWatchLoop(
            watcher,
            openBufferPaths = IO.pure(Map.empty),
            checkBufferForExternalChanges = _ => IO.unit,
            dictionaryWatchDirectories = IO.pure(Set.empty),
            refreshDictionaryFingerprints = refreshCount.update(_ + 1),
            pollInterval = 2.seconds
          )
          .take(3)
          .compile
          .drain
        count <- refreshCount.get
      yield count
    }

    runVirtual(checked) shouldBe 0
  }

  it should "stay virtual-time-compatible when there is nothing to watch, instead of blocking on WatchService.poll" in {
    // Regression test: with no open buffers, the loop must sleep rather than call the real, genuinely-blocking
    // WatchService.poll -- otherwise any virtual-time test harness driving AppRuntime.run (VirtualTime.runVirtual's
    // TestControl treats IO.blocking as non-terminating) hangs the instant this loop starts, even though the test
    // itself never opens a file.
    val checked = FileChangeWatcher.create.use { watcher =>
      for
        checkedBuffers <- Ref.of[IO, List[BufferId]](Nil)
        _ <- AppRuntime
          .externalChangeWatchLoop(
            watcher,
            openBufferPaths = IO.pure(Map.empty),
            checkBufferForExternalChanges = id => checkedBuffers.update(_ :+ id),
            pollInterval = 2.seconds
          )
          .take(3)
          .compile
          .drain
        checked <- checkedBuffers.get
      yield checked
    }

    runVirtual(checked) shouldBe Nil
  }
