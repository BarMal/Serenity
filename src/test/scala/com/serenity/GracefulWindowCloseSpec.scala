package com.serenity

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import cats.syntax.parallel.*
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.Quit
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GracefulWindowCloseSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private val ciTimeout = 15.seconds

  private def makeStateManager(): StateManager =
    createStateManager("GracefulWindowCloseSpec", initialConfig = AppConfig.default)

  "Window close" should "trigger the quit path and signal awaitQuit" in {
    val sm = makeStateManager()
    sm.createBuffer("", None).unsafeRunSync()
    sm.createPane(None).unsafeRunSync()
    val program = for
      awaiting <- Deferred[IO, Unit]
      _ <- (
        awaiting.complete(()) >> sm.runtimeLifecycle.awaitQuit,
        awaiting.get >> sm.applyEvent(Quit)
      ).parMapN((_, _) => ())
    yield ()

    // Should complete without timeout
    program.unsafeRunTimed(ciTimeout).shouldBe(defined)
  }

  it should "go through the close workflow when there are dirty buffers" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello", None).unsafeRunSync()
    sm.createPane(None).unsafeRunSync()
    sm.updateBuffer(bufferId, "modified").unsafeRunSync()
    sm.applyEvent(Quit).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    // Dirty buffer means close workflow is shown, app is NOT quit yet
    state.topModal shouldBe defined
    state.persisted.buffers.get(bufferId).exists(_.document.isDirty) shouldBe true
  }

  it should "allow an external close to terminate immediately even with dirty buffers" in {
    val sm       = makeStateManager()
    val bufferId = sm.createBuffer("hello", None).unsafeRunSync()
    sm.createPane(None).unsafeRunSync()
    sm.updateBuffer(bufferId, "modified").unsafeRunSync()
    val program = for
      awaiting <- Deferred[IO, Unit]
      _ <- (
        awaiting.complete(()) >> sm.runtimeLifecycle.awaitQuit,
        awaiting.get >> sm.runtimeLifecycle.forceQuit
      ).parMapN((_, _) => ())
    yield ()

    program.unsafeRunTimed(ciTimeout).shouldBe(defined)
    sm.getCurrentState.unsafeRunSync().persisted.buffers.get(bufferId).exists(_.document.isDirty) shouldBe true
  }
