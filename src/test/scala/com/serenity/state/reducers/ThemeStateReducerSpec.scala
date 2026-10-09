package com.serenity.state.reducers

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ThemeStateReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def withTheme(theme: Theme): AppState =
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(theme = theme))

  private def valid(result: ReducerResult): Boolean = AppStateValidation.validated(result.state).isRight

  "ThemeStateReducer.toggleTarget" should "pair each built-in theme with its opposite" in {
    ThemeStateReducer.toggleTarget(withTheme(Theme.dark)) shouldBe "light"
    ThemeStateReducer.toggleTarget(withTheme(Theme.light)) shouldBe "dark"
    ThemeStateReducer.toggleTarget(withTheme(Theme.dark.copy(name = "default-dark"))) shouldBe "default-light"
    ThemeStateReducer.toggleTarget(withTheme(Theme.dark.copy(name = "default-light"))) shouldBe "default-dark"
  }

  it should "fall back to the default of the opposite brightness for a custom theme" in {
    ThemeStateReducer.toggleTarget(withTheme(Theme.dark.copy(name = "Solarized-Light"))) shouldBe "default-dark"
    ThemeStateReducer.toggleTarget(withTheme(Theme.dark.copy(name = "dracula"))) shouldBe "default-light"
  }

  "ThemeStateReducer.applyTheme" should "switch the theme instantly" in {
    val result = ThemeStateReducer.applyTheme(Theme.light, withTheme(Theme.dark))

    result.state.persisted.theme shouldBe Theme.light
    result.effects shouldBe Nil
    valid(result) shouldBe true
  }

  "ThemeStateReducer.withAvailableThemeNames" should "record the listed theme names" in {
    val result = ThemeStateReducer.withAvailableThemeNames(List("dark", "light"), AppState.initial)

    result.state.runtime.themeDiscovery.availableThemeNames shouldBe List("dark", "light")
    valid(result) shouldBe true
  }

  private def chooser(result: ReducerResult): Option[ListPicker] =
    result.state.modalSurface.map(_.content).collect {
      case SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)) =>
        picker
    }

  "PopupSurfaceReducer.openThemePicker" should "open a focused theme list highlighting the current theme" in {
    val result = PopupSurfaceReducer.openThemePicker(List("light", "dark"), withTheme(Theme.dark))

    result.flatMap(chooser).flatMap(_.selectedChoice).map(_.label) shouldBe Some("dark")
    result.map(r => r.state.persisted.focus) shouldBe result.flatMap(_.state.modalSurface.map(s => Focus.Surface(s.id)))
    result.map(valid) shouldBe Some(true)
  }

  it should "select the first theme when the current one isn't listed" in {
    PopupSurfaceReducer
      .openThemePicker(List("a", "b"), withTheme(Theme.dark))
      .flatMap(chooser)
      .flatMap(_.selectedChoice)
      .map(_.label) shouldBe Some("a")
  }

  it should "decline to open before any theme names are known" in {
    PopupSurfaceReducer.openThemePicker(Nil, AppState.initial) shouldBe None
  }

  "PopupSurfaceReducer.openThemeCreator" should "open a single focused creator, replacing any existing one" in {
    val once  = PopupSurfaceReducer.openThemeCreator(AppState.initial).state
    val twice = PopupSurfaceReducer.openThemeCreator(once)

    val creators = twice.state.runtime.uiSurfaces.filter {
      _.content match
        case SurfaceContent.ThemeCreator(_) => true
        case _                              => false
    }
    creators should have size 1
    twice.state.persisted.focus shouldBe Focus.Surface(creators.head.id)
    valid(twice) shouldBe true
  }
