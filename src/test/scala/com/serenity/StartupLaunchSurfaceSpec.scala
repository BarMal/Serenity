package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.command.{CommandIntent, FileIntent, SessionIntent}
import com.serenity.keystroke.events.*
import com.serenity.state.components.{ComponentResult, StartupPageComponent}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StartupLaunchSurfaceSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def stateFor(page: StartupPage): AppState =
    val surfaceId = SurfaceId("startup")
    AppState.empty.copy(
      persisted = AppState.empty.persisted.copy(focus = Focus.Surface(surfaceId)),
      runtime = AppState.empty.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            surfaceId,
            SurfaceContent.StartPage(page),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  "Startup launch surface" should "omit Restore when no session is available and keep its visible shortcuts executable" in {
    val page = AppStartup.createStartPage(sessionExists = false, recentFiles = Nil)

    page.actions.map(_.id) shouldBe List("new-session", "open-file")
    page.actions.flatMap(_.shortcut) shouldBe List('1', '2')
    page.workflows.map(_.id) shouldBe List("workflow-writing", "workflow-code", "workflow-compact")
    page.workflows.flatMap(_.shortcut) shouldBe List('W', 'C', 'M')

    val result = StartupPageComponent().processEvent(InsertChar('2'), stateFor(page))

    result should matchPattern {
      case ComponentResult.ExecuteCommand(command)
          if command.intent == CommandIntent.Session(SessionIntent.StartupOpenFile) =>
    }
  }

  it should "activate a selected recent file through a path-carrying command" in {
    val recent = Files.createTempFile("serenity-recent-document", ".md")
    val page   = AppStartup.createStartPage(sessionExists = true, recentFiles = List(recent))
    val index  = page.actions.indexWhere(_.id == s"recent:${recent.toString}")

    index should be >= 0

    val result = StartupPageComponent().processEvent(StartupPageSelect(index), stateFor(page))

    result match
      case ComponentResult.ExecuteCommand(command) =>
        command.intent shouldBe CommandIntent.File(FileIntent.OpenRecentFile(recent))
      case other =>
        fail(s"Expected an executable recent-file command, got $other")
  }

  it should "use unambiguous recent labels and discard missing paths before rendering" in {
    val root    = Files.createTempDirectory("serenity-startup-recents")
    val first   = Files.createFile(root.resolve("one").resolveSibling("notes.md"))
    val second  = Files.createFile(root.resolve("second-notes.md"))
    val missing = root.resolve("missing.md")

    val readableRecentFiles =
      List(first, second, missing).filter(path => Files.isRegularFile(path) && Files.isReadable(path))
    val page = AppStartup.createStartPage(sessionExists = true, recentFiles = readableRecentFiles)

    val recents = page.actions.filter(_.id.startsWith("recent:"))
    recents.map(_.label) shouldBe List(first.toAbsolutePath.toString, second.toAbsolutePath.toString)
    recents.map(_.label) should not contain missing.toAbsolutePath.toString
  }

  it should "keep workflow presets and quick-resume out of the navigable list, as bottom shortcut hints" in {
    val page = AppStartup.createStartPage(sessionExists = true, recentFiles = Nil, resumeIdentifier = Some("notes.md"))

    page.launchActions.map(_.id) shouldBe List("new-session", "open-file")
    page.renderLines should not contain "Workflows"
    page.workflows.map(_.id) shouldBe List("workflow-writing", "workflow-code", "workflow-compact")
    page.resume.map(_.identifier) shouldBe Some("notes.md")
    page.actionLineIndices shouldBe List(2, 3)
  }

  it should "activate a workflow preset by its shortcut letter, case-insensitively" in {
    val page   = AppStartup.createStartPage(sessionExists = false, recentFiles = Nil)
    val result = StartupPageComponent().processEvent(InsertChar('c'), stateFor(page))

    result should matchPattern {
      case ComponentResult.ExecuteCommand(command)
          if command.intent == CommandIntent.UiPresets(com.serenity.command.UiPresetsIntent.ApplyUiPreset("Code")) =>
    }
  }

  it should "quick-resume the previous session with Tab" in {
    val page   = AppStartup.createStartPage(sessionExists = true, recentFiles = Nil, resumeIdentifier = Some("notes.md"))
    val result = StartupPageComponent().processEvent(TabKey, stateFor(page))

    result should matchPattern {
      case ComponentResult.ExecuteCommand(command)
          if command.intent == CommandIntent.Session(SessionIntent.StartupRestoreSession) =>
    }
  }

  it should "activate only the rendered launch action bounds with taller UI metrics" in {
    val stateManager = createStateManager("StartupLaunchSurfaceSpec-mouse")
    val viewport     = ViewportSize(80, 24)
    val codeMetrics  = CellMetrics(charWidth = 8, lineHeight = 12, ascent = 9)
    val uiMetrics    = CellMetrics(charWidth = 11, lineHeight = 24, ascent = 18)

    AppStartup
      .initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        viewport
      )
      .unsafeRunSync()
    val page = stateManager.getCurrentState
      .unsafeRunSync()
      .startPageSurface
      .flatMap {
        _.content match
          case SurfaceContent.StartPage(value) => Some(value)
          case _                               => None
      }
      .getOrElse(fail("expected startup page"))
    val bounds =
      page.actionBounds(viewport, codeMetrics, uiMetrics).headOption.getOrElse(fail("expected New document bounds"))

    stateManager
      .applyEvent(
        MouseClick(
          col = codeMetrics.toCol(bounds.xPx + 1),
          row = codeMetrics.toRow(bounds.yPx + 1),
          pixelX = Some(bounds.xPx + 1),
          pixelY = Some(bounds.yPx + 1),
          renderMetrics = Some(MouseRenderMetrics(codeMetrics, uiMetrics))
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().startPageSurface shouldBe None
  }

  it should "ignore clicks outside a compact launch action width" in {
    val stateManager = createStateManager("StartupLaunchSurfaceSpec-mouse-miss")
    val viewport     = ViewportSize(80, 24)
    val codeMetrics  = CellMetrics(charWidth = 8, lineHeight = 12, ascent = 9)
    val uiMetrics    = CellMetrics(charWidth = 11, lineHeight = 24, ascent = 18)

    AppStartup
      .initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        viewport
      )
      .unsafeRunSync()
    val page = stateManager.getCurrentState
      .unsafeRunSync()
      .startPageSurface
      .flatMap {
        _.content match
          case SurfaceContent.StartPage(value) => Some(value)
          case _                               => None
      }
      .getOrElse(fail("expected startup page"))
    val bounds =
      page.actionBounds(viewport, codeMetrics, uiMetrics).headOption.getOrElse(fail("expected New document bounds"))

    stateManager
      .applyEvent(
        MouseClick(
          col = 0,
          row = codeMetrics.toRow(bounds.yPx + 1),
          pixelX = Some(bounds.xPx - 1),
          pixelY = Some(bounds.yPx + 1),
          renderMetrics = Some(MouseRenderMetrics(codeMetrics, uiMetrics))
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().startPageSurface should not be None
  }

  // A config file that cannot be read resets every setting for the session. The only report of that was a line on
  // stderr, which the TUI redirects to nowhere -- so the start page, which is on screen at exactly that moment, says
  // it instead.

  it should "show a configuration notice on the start page, in place of the session status" in {
    val page = AppStartup.createStartPage(
      sessionExists = false,
      recentFiles = Nil,
      configNotice = Some("Configuration could not be read; using defaults")
    )

    page.statusMessage shouldBe Some("Configuration could not be read; using defaults")
  }

  it should "keep the session status when there is no configuration notice" in {
    AppStartup.createStartPage(sessionExists = false, recentFiles = Nil).statusMessage shouldBe
      Some("No previous session found")
  }
end StartupLaunchSurfaceSpec
