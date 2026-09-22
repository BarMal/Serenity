package com.serenity

import java.nio.file.Files

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.AppRuntime
import com.serenity.io.FileChangeWatcher
import com.serenity.state.models.BufferId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers `AppRuntime.externalChangeWatchLoop` (#1623): the background half of external-change detection, driven by
  * a real `FileChangeWatcher` rather than a fake, so this exercises the same directory-sync-then-poll-then-check
  * cycle the running app uses. `FileChangeWatcherSpec` covers the watcher's own sync/poll contract directly; this
  * spec covers the loop wiring that turns "a watched file changed" into "check this specific buffer."
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
