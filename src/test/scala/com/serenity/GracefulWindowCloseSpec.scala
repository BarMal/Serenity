package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.parallel.*
import com.serenity.keystroke.events.Quit
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

import scala.concurrent.duration.*

class GracefulWindowCloseSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager() =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

  "Window close" should "trigger the quit path and signal awaitQuit" in {
    val sm = makeStateManager()
    sm.createBuffer("").unsafeRunSync()
    sm.createPane().unsafeRunSync()

    // Simulate window close: fires Quit event, same as Ctrl+Q
    val program = (
      sm.awaitQuit,
      IO.sleep(50.millis) >> sm.applyEvent(Quit)
    ).parMapN((_, _) => ())

    // Should complete without timeout
    program.unsafeRunTimed(2.seconds) shouldBe defined
  }

  it should "go through the close workflow when there are dirty buffers" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello").unsafeRunSync()
    sm.createPane().unsafeRunSync()
    sm.updateBuffer(bufferId, "modified").unsafeRunSync()

    sm.applyEvent(Quit).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    // Dirty buffer means close workflow is shown, app is NOT quit yet
    state.modalSurface shouldBe defined
    state.buffers.get(bufferId).exists(_.isDirty) shouldBe true
  }

  it should "allow an external close to terminate immediately even with dirty buffers" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello").unsafeRunSync()
    sm.createPane().unsafeRunSync()
    sm.updateBuffer(bufferId, "modified").unsafeRunSync()

    val program = (
      sm.awaitQuit,
      IO.sleep(50.millis) >> sm.forceQuit()
    ).parMapN((_, _) => ())

    program.unsafeRunTimed(2.seconds) shouldBe defined
    sm.getCurrentState.unsafeRunSync().buffers.get(bufferId).exists(_.isDirty) shouldBe true
  }
