package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.Renderer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class RenderCursorOnlySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager.apply(logger).unsafeRunSync()

  "Renderer.render" should "complete without error given an AppState with no active pane" in {
    val surface = new MockRenderSurface(80, 24)
    val state   = AppState.empty
    noException should be thrownBy Renderer.render(state, cursorVisible = true, surface, ViewportSize(80, 24))
  }

  it should "complete without error given a pane with an empty buffer" in {
    val sm      = makeStateManager()
    val surface = new MockRenderSurface(80, 24)

    val bufferId = sm.createNewEmptyBuffer().unsafeRunSync()
    val state    = sm.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    sm.setBufferForPane(paneId, bufferId).unsafeRunSync()

    val finalState = sm.getCurrentState.unsafeRunSync()
    noException should be thrownBy Renderer.render(finalState, cursorVisible = false, surface, ViewportSize(80, 24))
  }

  it should "complete without error given a pane with text and a cursor in the middle" in {
    val sm      = makeStateManager()
    val surface = new MockRenderSurface(80, 24)

    val bufferId = sm.createBuffer("Hello, World!").unsafeRunSync()
    val state    = sm.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    sm.setBufferForPane(paneId, bufferId).unsafeRunSync()
    sm.setCursorPosition(paneId, 0, 6).unsafeRunSync()

    val finalState = sm.getCurrentState.unsafeRunSync()
    Renderer.render(finalState, cursorVisible = true, surface, ViewportSize(80, 24))

    (0 until surface.height).map(surface.getRow).mkString("\n") should include("Hello, World!")
  }
