package com.serenity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.components.ThemePickerComponent
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ThemeEventReducer}
import com.serenity.ui.layout.Layout

class ThemePickerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  // ── ThemePickerState ──────────────────────────────────────────────────────

  "ThemePickerState" should "return the selected theme by index" in {
    val state = ThemePickerState(List("dark", "light", "mocha"), selectedIndex = 1, originalTheme = "dark")
    state.selectedTheme shouldBe Some("light")
  }

  it should "move selection forward and wrap around" in {
    val state = ThemePickerState(List("dark", "light", "mocha"), selectedIndex = 2, originalTheme = "dark")
    state.moveSelection(1).selectedIndex shouldBe 0
  }

  it should "move selection backward and wrap around" in {
    val state = ThemePickerState(List("dark", "light", "mocha"), selectedIndex = 0, originalTheme = "dark")
    state.moveSelection(-1).selectedIndex shouldBe 2
  }

  it should "handle an empty theme list without throwing" in {
    val state = ThemePickerState(Nil, selectedIndex = 0, originalTheme = "dark")
    state.moveSelection(1).selectedIndex shouldBe 0
    state.selectedTheme shouldBe None
  }

  // ── ThemeEventReducer ─────────────────────────────────────────────────────

  "ThemeEventReducer" should "emit OpenThemePicker for ListAvailableThemes" in {
    val result = ThemeEventReducer.reduce(ListAvailableThemes, AppState.empty)
    result.effects shouldBe List(AppEffect.OpenThemePicker())
    result.state shouldBe AppState.empty
  }

  // ── ThemePickerComponent ──────────────────────────────────────────────────

  private val themes = List("dark", "light", "mocha")

  private def stateWithPickerAndRunner(selectedIndex: Int = 1): (AppState, SurfaceId, SurfaceId) =
    val base = AppState.empty.copy(
      layout = Layout(
        editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), BufferId(0))),
        activeEditorPaneId = Some(PaneId(0))
      )
    )
    val runner         = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val (s1, runnerId) = base.allocateSurfaceId
    val runnerSurface = UiSurface(
      runnerId,
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val pickerState    = ThemePickerState(themes, selectedIndex, originalTheme = "dark")
    val (s2, pickerId) = s1.allocateSurfaceId
    val pickerSurface = UiSurface(
      pickerId,
      SurfaceContent.ThemePicker(pickerState),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val finalState = s2.copy(
      uiSurfaces = List(runnerSurface, pickerSurface),
      focus = Focus.Surface(pickerId)
    )
    (finalState, runnerId, pickerId)

  private val component = new ThemePickerComponent()

  "ThemePickerComponent" should "navigate down and emit SwitchTheme for the newly selected theme" in {
    val (state, _, _) = stateWithPickerAndRunner(selectedIndex = 0)

    val result = component.processEvent(MoveDown, state)

    val reducerResult = result match
      case com.serenity.state.components.ComponentResult.ReducerUpdate(r) => r
      case other                                                          => fail(s"Expected ReducerUpdate, got $other")

    reducerResult.effects shouldBe List(AppEffect.SwitchTheme("light"))
    reducerResult.state.themePickerSurface.map(_.content) shouldBe
      Some(SurfaceContent.ThemePicker(ThemePickerState(themes, 1, "dark")))
  }

  it should "navigate up and wrap to last theme from index 0" in {
    val (state, _, _) = stateWithPickerAndRunner(selectedIndex = 0)

    val result = component.processEvent(MoveUp, state)

    val reducerResult = result match
      case com.serenity.state.components.ComponentResult.ReducerUpdate(r) => r
      case other                                                          => fail(s"Expected ReducerUpdate, got $other")

    reducerResult.effects shouldBe List(AppEffect.SwitchTheme("mocha"))
    reducerResult.state.themePickerSurface.map(_.content) shouldBe
      Some(SurfaceContent.ThemePicker(ThemePickerState(themes, 2, "dark")))
  }

  it should "dismiss picker and focus command runner on Enter without emitting SwitchTheme" in {
    val (state, runnerId, _) = stateWithPickerAndRunner(selectedIndex = 1)

    val result = component.processEvent(Enter, state)

    val newState = result match
      case com.serenity.state.components.ComponentResult.StateChange(f) => f(state)
      case other                                                        => fail(s"Expected StateChange, got $other")

    newState.themePickerSurface shouldBe None
    newState.focus shouldBe Focus.Surface(runnerId)
  }

  it should "dismiss picker and emit SwitchTheme(originalTheme) on ESC" in {
    val (state, runnerId, _) = stateWithPickerAndRunner(selectedIndex = 1)

    val result = component.processEvent(Escape, state)

    val reducerResult = result match
      case com.serenity.state.components.ComponentResult.ReducerUpdate(r) => r
      case other                                                          => fail(s"Expected ReducerUpdate, got $other")

    reducerResult.effects shouldBe List(AppEffect.SwitchTheme("dark"))
    reducerResult.state.themePickerSurface shouldBe None
    reducerResult.state.focus shouldBe Focus.Surface(runnerId)
  }

  it should "do nothing for unrecognised events" in {
    val (state, _, _) = stateWithPickerAndRunner(selectedIndex = 0)
    val result        = component.processEvent(InsertChar('x'), state)
    result shouldBe com.serenity.state.components.ComponentResult.NoChange
  }
