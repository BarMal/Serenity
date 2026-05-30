package com.serenity

import com.serenity.state.manager.StateManager
import com.serenity.app.AppStartup
import com.serenity.state.models.{Focus, SurfaceContent}
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}
import cats.effect.IO
import cats.effect.unsafe.implicits.global

class MainStartupSpec extends AnyFlatSpec with Matchers:

  behavior of "Main Application Startup"

  it should "initialize startup state with the current terminal size and a focused start page" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val logger = LoggerFactory[IO].getLogger(using LoggerName("Main"))
    val initialViewportSize = ViewportSize(200, 40)

    val result = for {
      themeManager <- IO.pure(AppThemeManager.create)
      defaultTheme <- themeManager.initializeWithTheme()
      stateManager <- StateManager.apply(logger)
      finalState <- AppStartup.initializeState(
        stateManager,
        defaultTheme,
        initialViewportSize
      )
    } yield finalState

    val finalState = result.unsafeRunSync()

    finalState.viewportSize.shouldBe(Some(initialViewportSize))
    finalState.theme.should(not.be(com.serenity.ui.theme.Theme.default))
    finalState.buffers.shouldBe(Map.empty)
    finalState.layout.editorPanes.shouldBe(Map.empty)
    finalState.startPageSurface.shouldBe(defined)
    finalState.focus.shouldBe(Focus.Surface(finalState.startPageSurface.get.id))
  }

  it should "render the expected startup choices on the dedicated start page" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val logger = LoggerFactory[IO].getLogger(using LoggerName("Main"))
    val wideViewportSize = ViewportSize(200, 40)

    val result = for {
      themeManager <- IO.pure(AppThemeManager.create)
      defaultTheme <- themeManager.initializeWithTheme()
      stateManager <- StateManager.apply(logger)
      finalState <- AppStartup.initializeState(
        stateManager,
        defaultTheme,
        wideViewportSize
      )
    } yield finalState

    val finalState = result.unsafeRunSync()

    finalState.startPageSurface.map(_.content) match {
      case Some(SurfaceContent.StartPage(startPage)) =>
        startPage.title shouldBe "What would you like to do?"
        startPage.options should contain allElementsOf List(
          "1. Start a new session",
          "2. Restore an existing session", 
          "3. Open an existing file or directory"
        )
        // Status message should be either None (session exists) or Some("No previous session found")
        startPage.statusMessage should (be(None) or be(Some("No previous session found")))
      case other => fail(s"Expected StartPage surface content, got: $other")
    }
  }
