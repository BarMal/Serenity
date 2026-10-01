package com.serenity

import com.serenity.command.ThemeCommands
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{
  AppEffect,
  ModalEventReducer,
  PopupSurfaceReducer,
  ReducerResult,
  SurfaceEffect,
  ThemeEventReducer
}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The theme chooser: a filterable [[ListPicker]] over the theme names, opened on the current theme, that previews each
  * theme it moves onto, applies the picked one, and restores the original on Escape.
  */
class ThemePickerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val themes = List("light", "dark", "mocha")

  private def opened(names: List[String] = themes, theme: Theme = Theme.dark): ReducerResult =
    val state = AppState.initial.copy(persisted = AppState.initial.persisted.copy(theme = theme))
    PopupSurfaceReducer.openThemePicker(names, state).getOrElse(fail("expected the theme chooser to open"))

  private def shown(state: AppState): ListPicker =
    state.modalSurface
      .map(_.content)
      .collect { case SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)) => picker }
      .getOrElse(fail("expected the theme chooser to be shown"))

  private def press(event: Event, state: AppState): ReducerResult =
    ModalEventReducer.reduce(ModalType.ListPicker, event, state)

  private def labels(picker: ListPicker): List[String] =
    picker.items.toOption.toList.flatMap(_.items.map(_.label))

  "ThemeEventReducer" should "emit OpenThemePicker for ListAvailableThemes" in {
    val result = ThemeEventReducer.reduce(ListAvailableThemes, AppState.empty)
    result.effects shouldBe List(AppEffect.Surface(SurfaceEffect.OpenThemePicker))
    result.state shouldBe AppState.empty
  }

  "The theme chooser" should "list every theme, marking and highlighting the current one, without previewing" in {
    val result = opened()
    val picker = shown(result.state)

    picker.title shouldBe "Theme"
    picker.items.toOption.toList.flatMap(_.items) shouldBe List(
      ListChoice("light", None, ThemeCommands.applyTheme("light"), preview = Some(ThemeCommands.applyTheme("light"))),
      ListChoice(
        "dark",
        Some("current"),
        ThemeCommands.applyTheme("dark"),
        preview = Some(ThemeCommands.applyTheme("dark"))
      ),
      ListChoice("mocha", None, ThemeCommands.applyTheme("mocha"), preview = Some(ThemeCommands.applyTheme("mocha")))
    )
    picker.selectedChoice.map(_.label) shouldBe Some("dark")
    picker.onDismiss shouldBe Some(ThemeCommands.applyTheme("dark"))
    result.effects shouldBe Nil
  }

  it should "preview each theme it moves onto, without recording it as a use" in {
    val down = press(MoveDown, opened().state)
    down.effects shouldBe List(AppEffect.ExecuteCommandUnrecorded(ThemeCommands.applyTheme("mocha")))

    val wrapped = press(MoveDown, down.state)
    wrapped.effects shouldBe List(AppEffect.ExecuteCommandUnrecorded(ThemeCommands.applyTheme("light")))
    shown(wrapped.state).selectedChoice.map(_.label) shouldBe Some("light")
  }

  it should "narrow the themes to those matching what is typed, previewing the new top match" in {
    val typed = press(InsertChar('m'), opened().state)

    labels(shown(typed.state)) shouldBe List("mocha")
    typed.effects shouldBe List(AppEffect.ExecuteCommandUnrecorded(ThemeCommands.applyTheme("mocha")))
  }

  it should "apply the highlighted theme as a recorded command on Enter and close" in {
    val onMocha = press(MoveDown, opened().state).state
    val picked  = press(Enter, onMocha)

    picked.effects shouldBe List(AppEffect.ExecuteCommand(ThemeCommands.applyTheme("mocha")))
    picked.state.modalSurface shouldBe None
  }

  it should "restore the theme it opened on, unrecorded, on Escape and close" in {
    val onMocha   = press(MoveDown, opened().state).state
    val dismissed = press(Escape, onMocha)

    dismissed.effects shouldBe List(AppEffect.ExecuteCommandUnrecorded(ThemeCommands.applyTheme("dark")))
    dismissed.state.modalSurface shouldBe None
  }
