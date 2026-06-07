package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.RenderController
import fs2.concurrent.SignallingRef
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Verifies that detecting a terminal resize triggers a full render cycle (fast mode), not just a cursor-only render.
  * The root cause of the bug: checkResize in Main.scala applied the resize event but never set fastMode = true, so the
  * idle phase kept doing cursor-only renders at 500ms intervals until the next keypress.
  */
class ResizeRenderTriggerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager(): IO[StateManager] =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager.apply(logger)

  "RenderController.handleResize" should "invoke onResized callback when a resize is detected" in {
    val result = for
      sm       <- makeStateManager()
      fastMode <- SignallingRef.of[IO, Boolean](false)
      _        <- RenderController.handleResize(Some(ViewportSize(120, 40)), sm, fastMode.set(true))
      flag     <- fastMode.get
    yield flag
    result.unsafeRunSync() shouldBe true
  }

  it should "not invoke onResized callback when no resize occurred" in {
    val result = for
      sm       <- makeStateManager()
      fastMode <- SignallingRef.of[IO, Boolean](false)
      _        <- RenderController.handleResize(None, sm, fastMode.set(true))
      flag     <- fastMode.get
    yield flag
    result.unsafeRunSync() shouldBe false
  }

  it should "update state with the new terminal size when a resize is detected" in {
    val newSize = ViewportSize(100, 35)
    val result = for
      sm    <- makeStateManager()
      _     <- RenderController.handleResize(Some(newSize), sm, IO.unit)
      state <- sm.getCurrentState
    yield state.viewportSize
    result.unsafeRunSync() shouldBe Some(newSize)
  }

  it should "leave state terminal size unchanged when no resize occurred" in {
    val result = for
      sm    <- makeStateManager()
      _     <- RenderController.handleResize(None, sm, IO.unit)
      state <- sm.getCurrentState
    yield state.viewportSize
    result.unsafeRunSync() shouldBe None
  }
