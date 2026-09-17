package com.serenity.ui.layout

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.CommandRunnerReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for issue #1548 ("command runner appears to double-navigate up/down keys"). The reducer's own
  * `selectedIndex` movement was never the problem -- `CommandRunner.moveSelection`/`CommandPaletteState.moveSelection`
  * always advance by exactly `delta` (see `CommandRunnerReducerNavigationSpec`). The bug was in the rendered viewport:
  * `SurfaceFrameLayout.itemWindow`'s `reservedContentRows` (the selected item's own expand-in-place group preview,
  * issue #1059) shrank the sibling row budget by however many children the *currently selected* group happened to have.
  * Two settings-root groups with very different child counts sitting next to each other made that budget -- and the
  * `half` centering value derived from it -- swing wildly between two adjacent selections, so a single keypress could
  * scroll the visible window by two, three, or more rows even though `selectedIndex` moved by exactly one. That looked
  * indistinguishable from the key having fired twice.
  */
class CommandRunnerViewportNavigationSpec extends AnyFlatSpec with Matchers:

  private def resolvedAbsoluteIndices(runner: CommandRunner, frame: LayoutRect): List[Int] =
    CommandRunnerSurfaceComposition
      .forRunner(runner, frame, itemGapRows = 0.0, itemTargetRows = 1, showKeyHints = false)
      .paintBoxes
      .flatMap(_.focusId)
      .flatMap(CommandRunnerSurfaceComposition.absoluteIndexOf)

  private def settingsRootRunner(selectedIndex: Int): CommandRunner =
    CommandRunner.empty
      .activate(CommandRegistry.default, AppConfig.default)
      .copy(surface = CommandRunnerSurface.Settings(root = CommandPaletteState(selectedIndex = selectedIndex)))

  // Small enough that the real settings-root groups' varying child counts (real production data, not a fixture)
  // previously produced multi-row jumps at this width -- see the class doc.
  private val narrowFrame = LayoutRect(0, 0, 60, 10)

  "the settings-root visible window" should "advance its start by at most one row per single-index selection move" in {
    val itemCount = settingsRootRunner(0).settingsSurfaceItems.size
    itemCount should be > 1

    val offsets = (0 until itemCount).map { index =>
      resolvedAbsoluteIndices(settingsRootRunner(index), narrowFrame).minOption.getOrElse(0)
    }

    offsets.sliding(2).foreach {
      case Seq(before, after) => math.abs(after - before) should be <= 1
      case _                  => ()
    }
  }

  it should "always keep the newly selected row inside the visible window after a single Up/Down press" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner            = CommandRunner.empty.activate(registry, AppConfig.default).openSettings
    val itemCount         = runner.settingsSurfaceItems.size
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val initialState = AppState(
      persisted = Persisted(layout = Layout.empty, buffers = Map.empty, focus = Focus.Surface(surface.id)),
      runtime = Runtime(uiSurfaces = List(surface))
    )

    def runnerOf(state: AppState): CommandRunner =
      state.commandRunnerSurface
        .flatMap {
          _.content match
            case SurfaceContent.CommandPalette(r) => Some(r)
            case _                                => None
        }
        .getOrElse(fail("Expected command runner surface"))

    val initialIndex   = runnerOf(initialState).settingsSurfaceSelectedIndex
    val initialVisible = resolvedAbsoluteIndices(runnerOf(initialState), narrowFrame)

    (0 until (itemCount - 1)).foldLeft((initialState, initialIndex, initialVisible)) {
      case ((state, previousIndex, previousVisible), _) =>
        val result      = CommandRunnerReducer.reduce(RunnerNavigate(Direction.Down), state, registry)
        val nextState   = result.state
        val nextRunner  = runnerOf(nextState)
        val nextIndex   = nextRunner.settingsSurfaceSelectedIndex
        val nextVisible = resolvedAbsoluteIndices(nextRunner, narrowFrame)

        // The reducer's own index bookkeeping: exactly one step per keypress.
        nextIndex shouldBe (previousIndex + 1)
        // The rendered window: the newly selected item is always visible after that one keypress.
        nextVisible should contain(nextIndex)
        // The window's own start never jumps by more than one row for that single keypress.
        math.abs(nextVisible.min - previousVisible.min) should be <= 1

        (nextState, nextIndex, nextVisible)
    }
  }
