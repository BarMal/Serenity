package com.serenity

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.*

import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import com.serenity.app.CallbackBridges
import com.serenity.state.manager.QuitOutcome
import com.serenity.ui.terminal.CloseRequestRouting
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Closing the window must ask the same close workflow as Ctrl+Q (so unsaved buffers are asked about) and must keep
  * working after the user answers Cancel, which a one-shot latch cannot.
  */
class WindowCloseRequestSpec extends AnyFlatSpec with Matchers:

  "CloseRequestRouting.route" should "hand the request to the registered callback and leave the latch alone" in {
    val latch    = new CountDownLatch(1)
    val requests = new AtomicInteger(0)

    CloseRequestRouting.route(Some(() => requests.incrementAndGet()), latch)
    CloseRequestRouting.route(Some(() => requests.incrementAndGet()), latch)

    requests.get shouldBe 2
    latch.getCount shouldBe 1L
  }

  it should "count the latch down when nothing is listening, as the forced-quit paths expect" in {
    val latch = new CountDownLatch(1)

    CloseRequestRouting.route(None, latch)

    latch.getCount shouldBe 0L
  }

  "CallbackBridges.closeRequest" should "ask the orderly quit once while an answer is pending, and again afterwards" in {
    val program = Dispatcher.parallel[IO].use { dispatcher =>
      for
        asked    <- IO.ref(0)
        answered <- Deferred[IO, Unit]
        quit = asked.update(_ + 1) >> answered.get.as(QuitOutcome.Abandoned)
        callback     <- CallbackBridges.closeRequest(quit, dispatcher)
        _            <- IO(callback())
        _            <- IO.sleep(100.millis)
        _            <- IO(callback())
        _            <- IO.sleep(100.millis)
        whilePending <- asked.get
        _            <- answered.complete(())
        _            <- IO.sleep(100.millis)
        _            <- IO(callback())
        _            <- IO.sleep(100.millis)
        afterwards   <- asked.get
      yield (whilePending, afterwards)
    }

    program.unsafeRunTimed(10.seconds) shouldBe Some((1, 2))
  }
