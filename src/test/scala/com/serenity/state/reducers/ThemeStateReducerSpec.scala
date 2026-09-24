package com.serenity.state.reducers

import com.serenity.config.AppConfigMotionOps.*
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

  "ThemeStateReducer.applyTheme" should "switch the theme and start a transition from the previous one" in {
    val state  = withTheme(Theme.dark)
    val result = ThemeStateReducer.applyTheme(Theme.light, state)

    result.state.persisted.theme shouldBe Theme.light
    result.state.runtime.themeTransition shouldBe
      state.persisted.config.scaledUiAnimation.map(config => ThemeTransition(Theme.dark, 0, config.steps))
    result.effects shouldBe Nil
    valid(result) shouldBe true
  }

  it should "clear any transition when re-applying the current theme" in {
    val state = withTheme(Theme.dark).copy(runtime =
      AppState.initial.runtime.copy(themeTransition = Some(ThemeTransition(Theme.light, 1, 5)))
    )

    ThemeStateReducer.applyTheme(Theme.dark, state).state.runtime.themeTransition shouldBe None
  }

  "ThemeStateReducer.replaceTheme" should "swap the theme without starting a transition" in {
    val result = ThemeStateReducer.replaceTheme(Theme.light, withTheme(Theme.dark))

    result.state.persisted.theme shouldBe Theme.light
    result.state.runtime.themeTransition shouldBe AppState.initial.runtime.themeTransition
    valid(result) shouldBe true
  }

  "ThemeStateReducer.withAvailableThemeNames" should "record the listed theme names" in {
    val result = ThemeStateReducer.withAvailableThemeNames(List("dark", "light"), AppState.initial)

    result.state.runtime.availableThemeNames shouldBe List("dark", "light")
    valid(result) shouldBe true
  }

  "PopupSurfaceReducer.openThemePicker" should "open a focused picker seeded at the current theme" in {
    val result = PopupSurfaceReducer.openThemePicker(List("light", "dark"), withTheme(Theme.dark))

    result.map(_.state.runtime.uiSurfaces.map(_.content)) shouldBe
      Some(List(SurfaceContent.ThemePicker(ThemePickerState(List("light", "dark"), 1, "dark"))))
    result.map(r => r.state.persisted.focus) shouldBe result.map(r => Focus.Surface(r.state.runtime.uiSurfaces.head.id))
    result.map(valid) shouldBe Some(true)
  }

  it should "select the first theme when the current one isn't listed" in {
    PopupSurfaceReducer
      .openThemePicker(List("a", "b"), withTheme(Theme.dark))
      .map(_.state.runtime.uiSurfaces.map(_.content)) shouldBe
      Some(List(SurfaceContent.ThemePicker(ThemePickerState(List("a", "b"), 0, "dark"))))
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

  "PopupSurfaceReducer.openFileSearch" should "open a focused, empty file-search overlay" in {
    val result = PopupSurfaceReducer.openFileSearch(AppState.initial)

    result.state.runtime.uiSurfaces.map(_.content) shouldBe List(SurfaceContent.FileSearch(FileSearchState("", Nil, 0)))
    result.state.persisted.focus shouldBe Focus.Surface(result.state.runtime.uiSurfaces.head.id)
    valid(result) shouldBe true
  }
