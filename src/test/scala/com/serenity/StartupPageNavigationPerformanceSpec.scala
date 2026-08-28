package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.command.Command
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Diagnostic coverage for #892: arrow-key navigation on the pure startup screen (no documents open) must not force a
  * full editor-scene rebuild on the next mouse-hit-testing call. Before the fix, `SurfaceGeometryKey.from` used the
  * startup page's full content -- including `selectedIndex` -- as part of the cache key gating
  * `AuthoritativeUiScene.forState`'s expensive `LayoutEngine.calculateLayoutWithUI` rebuild, so every arrow-key press
  * defeated the cache for whichever mouse-move/click/press/drag event came next
  * (`MouseHitTestGeometry.isInsideFloatingSurface` and `EditorMouseTargeting.resolveMouseTarget` both build that scene
  * unconditionally). The actual, deterministic regression guard for this lives in `MouseTargetCacheSpec`
  * (object-identity checks on the cached scene, independent of timing); this spec logs the real wall-clock cost of a
  * nav+hover cycle for human review, but does not assert on it -- see `CommandRunnerRenderPerformanceSpec` for why
  * wall-clock assertions are unreliable on shared CI hardware.
  */
class StartupPageNavigationPerformanceSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "navigating the startup page while hovering with the mouse" should "report its per-navigation cost" in {
    val driver = UiScenarioDriver.create("startup-page-navigation-perf").unsafeRunSync()

    val newSessionCommand = Command.typed(
      "startup.new-session",
      "Start a new session",
      com.serenity.command.CommandIntent.Session(com.serenity.command.SessionIntent.StartupNewSession)
    )
    val openFileCommand = Command.typed(
      "startup.open-file",
      "Open an existing file or directory",
      com.serenity.command.CommandIntent.Session(com.serenity.command.SessionIntent.StartupOpenFile)
    )
    val actions = List(
      StartupAction("new-session", "Start a new session", newSessionCommand),
      StartupAction("open-file", "Open a file", openFileCommand),
      StartupAction("restore-session", "Restore an existing session", newSessionCommand)
    )

    driver
      .updateState { state =>
        val surface = UiSurface(
          SurfaceId("surface-0"),
          SurfaceContent.StartPage(StartupPage(title = "Welcome", actions = actions)),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
        state.copy(
          persisted = state.persisted.copy(focus = Focus.Surface(surface.id)),
          runtime = state.runtime.copy(uiSurfaces = List(surface))
        )
      }
      .unsafeRunSync()

    val timings = (1 to 20).map { i =>
      val start = System.nanoTime()
      driver.dispatch(StartupPageMoveDown).unsafeRunSync()
      driver.dispatch(MouseMove(col = 10, row = 10 + (i % 3))).unsafeRunSync()
      (System.nanoTime() - start) / 1000000L
    }

    info(s"per-navigation average: ${timings.sum / timings.length}ms, max: ${timings.max}ms")
  }
