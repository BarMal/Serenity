package com.serenity.state.reducers

import com.serenity.command.*
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `CommandRunnerReducer.submitToggle` -- the "flip a `ToggleItem` in place" reducer behavior a `ToggleItem` row
  * dispatches to on `RunnerSubmit` (mirroring `submitSubmenuOption`'s dispatch shape for `OptionItem`; see the
  * `reduceActive`/`submitRootSelection`/`submitSubmenu` wiring in `CommandRunnerReducer`). Tested directly against
  * this `private[reducers]` function -- like `invalidInputMessage` below it -- since (unlike `OptionItem`, which is
  * always backed by a real config-driven settings row) no settings group yet builds a `ToggleItem`: it is a generic
  * primitive with no current production call site.
  */
class CommandRunnerReducerToggleSpec extends AnyFlatSpec with Matchers:

  private val toggleItem = CommandSurfaceItem.ToggleItem(
    id = "diff-toggle",
    label = "Enable widget",
    checked = false,
    category = CommandCategory.Settings
  )

  private def stateWithRunner(runner: CommandRunner): AppState =
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    AppState(
      persisted = Persisted(layout = Layout.empty, buffers = Map.empty, focus = Focus.Surface(surface.id)),
      runtime = Runtime(uiSurfaces = List(surface))
    )

  private def runnerFrom(state: AppState): CommandRunner =
    state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }
      .getOrElse(fail("Expected command runner surface"))

  "submitToggle" should "flip an unchecked ToggleItem to checked, with no effects" in {
    val state = stateWithRunner(CommandRunner.empty.copy(isActive = true))

    val result = CommandRunnerReducer.submitToggle(state, toggleItem)

    result.effects shouldBe Nil
    runnerFrom(result.state).effectiveChecked(toggleItem) shouldBe true
  }

  it should "flip back to unchecked on a second submit" in {
    val state = stateWithRunner(CommandRunner.empty.copy(isActive = true))

    val onceFlipped  = CommandRunnerReducer.submitToggle(state, toggleItem)
    val twiceFlipped = CommandRunnerReducer.submitToggle(onceFlipped.state, toggleItem)

    runnerFrom(twiceFlipped.state).effectiveChecked(toggleItem) shouldBe false
  }

  it should "cycle checked -> unchecked -> checked across three submits" in {
    val state = stateWithRunner(CommandRunner.empty.copy(isActive = true))

    val first  = CommandRunnerReducer.submitToggle(state, toggleItem)
    val second = CommandRunnerReducer.submitToggle(first.state, toggleItem)
    val third  = CommandRunnerReducer.submitToggle(second.state, toggleItem)

    runnerFrom(first.state).effectiveChecked(toggleItem) shouldBe true
    runnerFrom(second.state).effectiveChecked(toggleItem) shouldBe false
    runnerFrom(third.state).effectiveChecked(toggleItem) shouldBe true
  }

  it should "leave the rest of the CommandRunner's state untouched" in {
    val runner = CommandRunner.empty.copy(isActive = true, editingText = "unrelated")
    val state  = stateWithRunner(runner)

    val result = CommandRunnerReducer.submitToggle(state, toggleItem)

    runnerFrom(result.state).editingText shouldBe "unrelated"
  }
