package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class ActualStartupFlowSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  behavior of "Actual Startup Flow (mimicking Main.scala)"

  it should "start with startup page focused when following Main.scala flow" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      // This mimics what Main.scala does
      logger <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))

      stateManager       <- StateManager.apply(logger)
      stateAfterCreation <- stateManager.getCurrentState
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)
      initialState  <- AppStartup.initializeState(stateManager, theme, viewportSize)
      _             <- stateManager.applyEvent(MoveDown)
      stateAfterNav <- stateManager.getCurrentState
      startPage = stateAfterNav.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
    yield
      stateAfterCreation.persisted.layout.editorPanes should not be empty
      stateAfterCreation.persisted.buffers should not be empty

      initialState.persisted.focus shouldBe Focus.Surface(SurfaceId("surface-0"))
      initialState.startPageSurface should be(defined)
      initialState.persisted.layout.editorPanes shouldBe empty
      initialState.persisted.buffers shouldBe empty

      startPage.selectedIndex shouldBe 1

    program.unsafeRunSync()
  }
