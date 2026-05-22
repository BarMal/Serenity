package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.googlecode.lanterna.screen.{Screen, TerminalScreen}
import com.googlecode.lanterna.terminal.virtual.DefaultVirtualTerminal
import com.googlecode.lanterna.{TerminalSize => LanternaSize}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.renderer.Renderer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class RenderCursorOnlySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeTestScreen(): Screen =
    val terminal = new DefaultVirtualTerminal(new LanternaSize(80, 24))
    val screen   = new TerminalScreen(terminal)
    screen.startScreen()
    screen

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager.apply(logger).unsafeRunSync()

  "Renderer.renderCursorOnly" should "complete without error given an AppState with no active pane" in {
    val screen = makeTestScreen()
    val state  = AppState.empty
    noException should be thrownBy Renderer.renderCursorOnly(state, cursorVisible = true, screen)
    screen.stopScreen()
  }

  it should "complete without error given a pane with an empty buffer" in {
    val sm     = makeStateManager()
    val screen = makeTestScreen()

    val bufferId = sm.createNewEmptyBuffer().unsafeRunSync()
    val state    = sm.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    sm.setBufferForPane(paneId, bufferId).unsafeRunSync()

    val finalState = sm.getCurrentState.unsafeRunSync()
    noException should be thrownBy Renderer.renderCursorOnly(finalState, cursorVisible = false, screen)
    screen.stopScreen()
  }

  it should "complete without error given a pane with text and a cursor in the middle" in {
    val sm     = makeStateManager()
    val screen = makeTestScreen()

    val bufferId = sm.createBuffer("Hello, World!").unsafeRunSync()
    val state    = sm.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    sm.setBufferForPane(paneId, bufferId).unsafeRunSync()
    sm.setCursorPosition(paneId, 0, 6).unsafeRunSync()

    val finalState = sm.getCurrentState.unsafeRunSync()
    noException should be thrownBy Renderer.renderCursorOnly(finalState, cursorVisible = true, screen)
    screen.stopScreen()
  }
