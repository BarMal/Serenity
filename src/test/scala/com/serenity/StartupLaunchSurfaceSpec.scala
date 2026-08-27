package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.command.CommandIntent
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

    page.actions
      .map(_.id) shouldBe List("new-session", "open-file", "workflow-writing", "workflow-code", "workflow-compact")
    page.actions.flatMap(_.shortcut) shouldBe List('1', '2')

    val result = StartupPageComponent().processEvent(InsertChar('2'), stateFor(page))

    result should matchPattern {
      case ComponentResult.ExecuteCommand(command) if command.intent == CommandIntent.StartupOpenFile =>
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
        command.intent shouldBe CommandIntent.OpenRecentFile(recent)
      case other =>
        fail(s"Expected an executable recent-file command, got $other")
  }

  it should "use unambiguous recent labels and discard missing paths before rendering" in {
    val root    = Files.createTempDirectory("serenity-startup-recents")
    val first   = Files.createFile(root.resolve("one").resolveSibling("notes.md"))
    val second  = Files.createFile(root.resolve("second-notes.md"))
    val missing = root.resolve("missing.md")

    val page = AppStartup.createStartPage(sessionExists = true, recentFiles = List(first, second, missing))

    val recents = page.actions.filter(_.id.startsWith("recent:"))
    recents.map(_.label) shouldBe List(first.toAbsolutePath.toString, second.toAbsolutePath.toString)
    recents.map(_.label) should not contain missing.toAbsolutePath.toString
  }

  it should "separate workflow presets from session actions in the rendered layout" in {
    val page = AppStartup.createStartPage(sessionExists = false, recentFiles = Nil)

    page.renderLines.slice(0, 7) shouldBe List(
      "Welcome to Serenity",
      "Choose a starting point",
      "",
      page.actions(0).renderedLabel,
      page.actions(1).renderedLabel,
      "",
      "Workflows"
    )
    page.actionLineIndices shouldBe List(3, 4, 7, 8, 9)
  }

  it should "activate only the rendered launch action bounds with taller UI metrics" in {
    val stateManager = createStateManager("StartupLaunchSurfaceSpec-mouse")
    val viewport     = ViewportSize(80, 24)
    val codeMetrics  = CellMetrics(charWidth = 8, lineHeight = 12, ascent = 9)
    val uiMetrics    = CellMetrics(charWidth = 11, lineHeight = 24, ascent = 18)

    AppStartup.initializeState(stateManager, Theme.default, viewport).unsafeRunSync()
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

    AppStartup.initializeState(stateManager, Theme.default, viewport).unsafeRunSync()
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
end StartupLaunchSurfaceSpec
