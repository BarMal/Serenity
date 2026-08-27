package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{Focus, SurfaceContent}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class MainStartupSpec extends AnyFlatSpec with Matchers:

  behavior of "Main Application Startup"

  it should "initialize startup state with the current terminal size and a focused start page" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO]         = Slf4jFactory.create[IO]

    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Main"))
    val initialViewportSize = ViewportSize(200, 40)

    val result = for
      themeManager <- IO.pure(AppThemeManager.create)
      defaultTheme <- themeManager.initializeWithTheme()
      stateManager <- StateManager.apply(logger)
      finalState <- AppStartup.initializeState(
        stateManager,
        defaultTheme,
        initialViewportSize
      )
    yield finalState

    val finalState = result.unsafeRunSync()

    finalState.runtime.viewportSize.shouldBe(Some(initialViewportSize))
    finalState.persisted.theme.should(not.be(com.serenity.ui.theme.Theme.default))
    finalState.persisted.buffers.shouldBe(Map.empty)
    finalState.persisted.layout.editorPanes.shouldBe(Map.empty)
    finalState.startPageSurface.shouldBe(defined)
    finalState.persisted.focus.shouldBe(Focus.Surface(finalState.startPageSurface.get.id))
  }

  it should "render the expected startup choices on the dedicated start page" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO]         = Slf4jFactory.create[IO]

    val logger           = LoggerFactory[IO].getLogger(using LoggerName("Main"))
    val wideViewportSize = ViewportSize(200, 40)

    val result = for
      themeManager <- IO.pure(AppThemeManager.create)
      defaultTheme <- themeManager.initializeWithTheme()
      stateManager <- StateManager.apply(logger)
      finalState <- AppStartup.initializeState(
        stateManager,
        defaultTheme,
        wideViewportSize
      )
    yield finalState

    val finalState = result.unsafeRunSync()

    finalState.startPageSurface.map(_.content) match
      case Some(SurfaceContent.StartPage(startPage)) =>
        startPage.title shouldBe "Welcome to Serenity"
        startPage.actions.map(_.id) should contain allOf ("new-session", "open-file", "workflow-writing")
        startPage.actions.map(_.id) should not contain "restore-session"
        startPage.statusMessage shouldBe Some("No previous session found")
      case other => fail(s"Expected StartPage surface content, got: $other")
  }

  it should "open a launch path instead of showing the startup page" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO]         = Slf4jFactory.create[IO]

    val logger              = LoggerFactory[IO].getLogger(using LoggerName("MainLaunchPathSpec"))
    val selectedFile        = Files.createTempFile("serenity-launch-open", ".txt")
    val initialViewportSize = ViewportSize(120, 30)

    try
      Files.writeString(selectedFile, "opened from launch option")

      val result = for
        themeManager <- IO.pure(AppThemeManager.create)
        defaultTheme <- themeManager.initializeWithTheme()
        stateManager <- StateManager.apply(logger)
        finalState <- AppStartup.initializeState(
          stateManager,
          defaultTheme,
          initialViewportSize,
          openPath = Some(selectedFile)
        )
      yield finalState

      val finalState = result.unsafeRunSync()

      finalState.startPageSurface shouldBe None
      finalState.persisted.buffers.size shouldBe 1
      finalState.persisted.layout.editorPanes.size shouldBe 1
      finalState.persisted.bufferOrder.size shouldBe 1
      finalState.persisted.buffers.values
        .find(_.document.filePath.contains(selectedFile))
        .map(_.document.content.collect()) shouldBe Some(
        "opened from launch option"
      )
      val paneId = finalState.persisted.layout.activeEditorPaneId.getOrElse(fail("Expected an active editor pane"))
      finalState.persisted.focus shouldBe Focus.EditorPane(paneId)
      val activeBufferId =
        finalState.persisted.layout.editorPanes
          .get(paneId)
          .flatMap(_.bufferId)
          .getOrElse(fail("Expected active pane buffer"))
      finalState.persisted.buffers(activeBufferId).document.filePath shouldBe Some(selectedFile)

      val surface     = new MockRenderSurface(initialViewportSize.width, initialViewportSize.height)
      val font        = FontLoader.previewCodeFont(FontConfig(fontSize = 12.0f))
      val cellMetrics = CellMetrics.fromFont(font)

      Renderer.render(finalState, cursorVisible = true, surface, initialViewportSize, font, font, cellMetrics, None)

      (0 until initialViewportSize.height).map(surface.getRow).mkString("\n") should include(
        "opened from launch option"
      )
    finally Files.deleteIfExists(selectedFile)
  }

  it should "use the current session theme for startup instead of defaulting to dark" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    given LoggerFactory[IO]         = Slf4jFactory.create[IO]

    val logger              = LoggerFactory[IO].getLogger(using LoggerName("MainStartupThemeSpec"))
    val sessionRoot         = Files.createTempDirectory("serenity-startup-theme")
    val initialViewportSize = ViewportSize(120, 30)

    val program = for
      firstManager <- StateManager.apply(logger, sessionRootOverride = Some(sessionRoot))
      _ <- firstManager.updateState(state => state.copy(persisted = state.persisted.copy(theme = Theme.light)))
      _ <- firstManager.saveSession()
      secondManager <- StateManager.apply(
        logger,
        sessionRootOverride = Some(sessionRoot)
      )
      startupTheme <- AppStartup.startupTheme(secondManager, AppThemeManager.create)
      finalState <- AppStartup.initializeState(
        secondManager,
        startupTheme,
        initialViewportSize
      )
    yield finalState

    val finalState = program.unsafeRunSync()

    finalState.persisted.theme.name shouldBe Theme.light.name
  }
