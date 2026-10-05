package com.serenity

import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.app.AppRuntime
import com.serenity.io.FileChangeWatcher
import com.serenity.rope.Balance
import com.serenity.state.models.BufferId
import com.serenity.testkit.VirtualTime.runVirtual
import fs2.concurrent.SignallingRef
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers `AppRuntime.externalChangeWatchLoop` (#1623): the background half of external-change detection, driven by a
  * real `FileChangeWatcher` rather than a fake, so this exercises the same directory-sync-then-poll-then-check cycle
  * the running app uses. `FileChangeWatcherSpec` covers the watcher's own sync/poll contract directly; this spec covers
  * the loop wiring that turns "a watched file changed" into "check this specific buffer."
  */
class AppRuntimeExternalChangeWatchSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

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
              checkBufferForExternalChanges = id => checkedBuffers.update(_ :+ id)
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
            checkBufferForExternalChanges = id => checkedBuffers.update(_ :+ id)
          )
          .interruptAfter(1.second)
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
              refreshDictionaryFingerprints = refreshCount.update(_ + 1)
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
            refreshDictionaryFingerprints = refreshCount.update(_ + 1)
          )
          .interruptAfter(1.second)
          .compile
          .drain
      }
      count <- refreshCount.get
    yield count

    program.unsafeRunTimed(10.seconds) shouldBe Some(0)
  }

  it should "stay virtual-time-compatible when spell-check is disabled and dictionaryWatchDirectories is empty" in {
    // Mirrors the buffer-less case below: SpellCheckConfig.dictionaryWatchDirectories returns Set.empty when
    // disabled (the default), so a real blocking WatchService call must still never run here either.
    val checked = FileChangeWatcher.create.use { watcher =>
      for
        refreshCount <- Ref.of[IO, Int](0)
        _ <- AppRuntime
          .externalChangeWatchLoop(
            watcher,
            openBufferPaths = IO.pure(Map.empty),
            checkBufferForExternalChanges = _ => IO.unit,
            dictionaryWatchDirectories = IO.pure(Set.empty),
            refreshDictionaryFingerprints = refreshCount.update(_ + 1)
          )
          .interruptAfter(1.minute)
          .compile
          .drain
        count <- refreshCount.get
      yield count
    }

    runVirtual(checked) shouldBe 0
  }

  it should "stay virtual-time-compatible when there is nothing to watch, instead of blocking on WatchService.poll" in {
    // Regression test: with no open buffers, the loop must wait for the watched set to change rather than call the
    // real, genuinely-blocking WatchService -- otherwise any virtual-time test harness driving AppRuntime.run (VirtualTime.runVirtual's
    // TestControl treats IO.blocking as non-terminating) hangs the instant this loop starts, even though the test
    // itself never opens a file.
    val checked = FileChangeWatcher.create.use { watcher =>
      for
        checkedBuffers <- Ref.of[IO, List[BufferId]](Nil)
        _ <- AppRuntime
          .externalChangeWatchLoop(
            watcher,
            openBufferPaths = IO.pure(Map.empty),
            checkBufferForExternalChanges = id => checkedBuffers.update(_ :+ id)
          )
          .interruptAfter(1.minute)
          .compile
          .drain
        checked <- checkedBuffers.get
      yield checked
    }

    runVirtual(checked) shouldBe Nil
  }

  it should "mark an explorer directory stale when a file is added to or removed from it" in {
    val directory = Files.createTempDirectory("external-change-watch-explorer")
    val added     = directory.resolve("new.md")

    val program = for
      staleBatches <- Ref.of[IO, List[Set[java.nio.file.Path]]](Nil)
      _ <- FileChangeWatcher.create.use { watcher =>
        for
          _ <- watcher.sync(Set(directory))
          _ <- IO.blocking(Files.writeString(added, "created externally"))
          _ <- AppRuntime
            .externalChangeWatchLoop(
              watcher,
              openBufferPaths = IO.pure(Map.empty),
              checkBufferForExternalChanges = _ => IO.unit,
              explorerWatchDirectories = IO.pure(Set(directory)),
              markExplorerDirectoriesStale = directories => staleBatches.update(_ :+ directories)
            )
            .take(1)
            .compile
            .drain
        yield ()
      }
      stale <- staleBatches.get
    yield stale

    try program.unsafeRunTimed(15.seconds) shouldBe Some(List(Set(directory)))
    finally
      Files.deleteIfExists(added)
      Files.deleteIfExists(directory)
  }

  it should "check a file once for a burst of writes that land within the settle window (#1885)" in {
    val file     = Files.createTempFile("external-change-watch-burst", ".md")
    val bufferId = BufferId(1)

    val program = for
      checkedBuffers <- Ref.of[IO, List[BufferId]](Nil)
      _ <- FileChangeWatcher.create.use { watcher =>
        val loop = AppRuntime.externalChangeWatchLoop(
          watcher,
          openBufferPaths = IO.pure(Map(file -> bufferId)),
          checkBufferForExternalChanges = id => checkedBuffers.update(_ :+ id)
        )
        val burst =
          (1 to 5).toList.traverse_(n => IO.blocking(Files.writeString(file, s"write $n")) >> IO.sleep(20.millis))
        loop.compile.drain.background.surround(IO.sleep(500.millis) >> burst >> IO.sleep(1.second))
      }
      checked <- checkedBuffers.get
    yield checked

    program.unsafeRunTimed(15.seconds) shouldBe Some(List(bufferId))
  }

  it should "not wake while there is nothing to watch (#1938)" in {
    val wakeups = FileChangeWatcher.create.use { watcher =>
      for
        derivations <- Ref.of[IO, Int](0)
        _ <- AppRuntime
          .externalChangeWatchLoop(
            watcher,
            openBufferPaths = derivations.update(_ + 1).as(Map.empty),
            checkBufferForExternalChanges = _ => IO.unit
          )
          .interruptAfter(1.minute)
          .compile
          .drain
        count <- derivations.get
      yield count
    }

    runVirtual(wakeups) should be <= 1
  }

  "watchInputsChanged" should "announce an opened file but not an edit to one already open (#1938)" in {
    val file    = Files.createTempFile("external-change-watch-inputs", ".md")
    val initial = com.serenity.state.models.AppState.initial
    val opened = initial.copy(persisted =
      initial.persisted.copy(buffers =
        Map(BufferId(1) -> com.serenity.state.models.Buffer.fromFile(BufferId(1), file, "text"))
      )
    )
    val edited = opened.copy(persisted =
      opened.persisted.copy(buffers =
        Map(BufferId(1) -> com.serenity.state.models.Buffer.fromFile(BufferId(1), file, "text, edited"))
      )
    )

    AppRuntime.watchInputsChanged(initial, opened) shouldBe true
    AppRuntime.watchInputsChanged(opened, edited) shouldBe false
  }

  "externalChangeWatchLoop" should "start watching a file once a change to the watched set announces it (#1938)" in {
    // Its own directory: the shared temp directory sees every other process's files, and any of them would count as
    // the first change.
    val file     = Files.createFile(Files.createTempDirectory("external-change-watch-opened").resolve("notes.md"))
    val bufferId = BufferId(1)

    val program = for
      open           <- Ref.of[IO, Map[java.nio.file.Path, BufferId]](Map.empty)
      watchedSet     <- SignallingRef.of[IO, Long](0L)
      checkedBuffers <- Ref.of[IO, List[BufferId]](Nil)
      openThenEdit =
        IO.sleep(300.millis) >> open.set(Map(file -> bufferId)) >> watchedSet.update(_ + 1) >>
          IO.sleep(300.millis) >> IO.blocking(Files.writeString(file, "changed externally"))
      _ <- FileChangeWatcher.create.use { watcher =>
        openThenEdit.background.surround(
          AppRuntime
            .externalChangeWatchLoop(
              watcher,
              openBufferPaths = open.get,
              checkBufferForExternalChanges = id => checkedBuffers.update(_ :+ id),
              watchedSetChanges = watchedSet.discrete.void
            )
            .take(1)
            .compile
            .drain
        )
      }
      checked <- checkedBuffers.get
    yield checked

    program.unsafeRunTimed(15.seconds) shouldBe Some(List(bufferId))
  }
