package com.serenity

import java.util.concurrent.CountDownLatch

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.app.AppRuntime
import com.serenity.ui.terminal.SwingWindow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The GUI shutdown gate: `AppRuntime.run` composes `SwingWindow.awaitClose` as its external-quit signal, so the run IO
  * completes (and CE3's `IOApp` then calls `System.exit`) only when either the window closes or the in-app quit signal
  * fires. `coordinateExternalQuit` races the two; the trap is that the close side blocks a real thread on a
  * `CountDownLatch`, and if that block is uninterruptible the losing side of the race can never be cancelled, so the
  * run IO hangs forever on the in-app Quit path while still exiting cleanly via the chrome close control.
  */
class SwingWindowCloseGateSpec extends AnyFlatSpec with Matchers:

  "SwingWindow.awaitClose" should "return once the close latch is counted down (the chrome close control)" in {
    val latch = new CountDownLatch(1)
    val program = for
      fiber <- SwingWindow.awaitCloseLatch(latch).start
      _     <- IO.blocking(latch.countDown())
      _     <- fiber.joinWithNever
    yield ()

    program.unsafeRunTimed(5.seconds) shouldBe defined
  }

  it should "let the runtime quit coordinator complete when quit fires and the window never closes" in {
    // The exact GUI composition: awaitClose (blocked on a latch that is never counted down) is the external-quit signal,
    // and the in-app quit signal fires instead. The run IO must complete rather than block forever on the close latch.
    val latch = new CountDownLatch(1)
    val program = for
      quit   <- Deferred[IO, Unit]
      forced <- Ref.of[IO, Boolean](false)
      coordinator <- AppRuntime
        .coordinateExternalQuit(SwingWindow.awaitCloseLatch(latch), forced.set(true), quit.get)
        .start
      _ <- quit.complete(())
      _ <- coordinator.joinWithNever
    yield ()

    program.unsafeRunTimed(5.seconds) shouldBe defined
  }
