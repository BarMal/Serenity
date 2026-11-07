package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.RendererEntryPoints
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RenderCursorOnlySpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def makeStateManager(): StateManager =
    createStateManager("RenderCursorOnlySpec", initialConfig = AppConfig.default)

  "RendererEntryPoints.render" should "complete without error given an AppState with no active pane" in {
    val surface = new MockRenderSurface(80, 24)
    val state   = AppState.empty
    noException should be thrownBy RendererEntryPoints.render(
      state,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      com.serenity.state.manager.RenderCaches.create()
    )
  }

  it should "complete without error given a pane with an empty buffer" in {
    val sm      = makeStateManager()
    val surface = new MockRenderSurface(80, 24)

    val bufferId = sm.createNewEmptyBuffer.unsafeRunSync()
    val state    = sm.getCurrentState.unsafeRunSync()
    val paneId   = state.persisted.layout.editorPanes.keys.head
    sm.setBufferForPane(paneId, bufferId).unsafeRunSync()

    val finalState = sm.getCurrentState.unsafeRunSync()
    noException should be thrownBy RendererEntryPoints.render(
      finalState,
      cursorVisible = false,
      surface,
      ViewportSize(80, 24),
      com.serenity.state.manager.RenderCaches.create()
    )
  }

  it should "complete without error given a pane with text and a cursor in the middle" in {
    val sm      = makeStateManager()
    val surface = new MockRenderSurface(80, 24)

    val bufferId = sm.createBuffer("Hello, World!", None).unsafeRunSync()
    val state    = sm.getCurrentState.unsafeRunSync()
    val paneId   = state.persisted.layout.editorPanes.keys.head
    sm.setBufferForPane(paneId, bufferId).unsafeRunSync()
    sm.setCursorPosition(paneId, 0, 6).unsafeRunSync()

    val finalState = sm.getCurrentState.unsafeRunSync()
    RendererEntryPoints.render(
      finalState,
      cursorVisible = true,
      surface,
      ViewportSize(80, 24),
      com.serenity.state.manager.RenderCaches.create()
    )

    (0 until surface.height).map(surface.getRow).mkString("\n") should include("Hello, World!")
  }
