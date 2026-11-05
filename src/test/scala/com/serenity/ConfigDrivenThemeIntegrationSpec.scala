package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.SwitchTheme
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConfigDrivenThemeIntegrationSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def createStateManager(): StateManager =
    createStateManager("ConfigDrivenThemeIntegrationSpec", initialConfig = AppConfig.default)

  "Config-driven theming integration" should "load dark theme and update AppState" in {
    val themeManager = AppThemeManager.create

    // Initialize with dark theme
    val darkTheme = themeManager.initializeWithTheme("dark").unsafeRunSync()
    darkTheme.name shouldBe "dark"

    // Create an initial app state
    val initialState = AppState.empty.copy(persisted = AppState.empty.persisted.copy(theme = darkTheme))
    initialState.persisted.theme.name shouldBe "dark"

    // Verify theme properties
    darkTheme.foregroundColor should not be darkTheme.backgroundColor
  }

  it should "handle missing theme gracefully" in {
    val stateManager = createStateManager()
    val initialState = stateManager.getCurrentState.unsafeRunSync()

    noException should be thrownBy stateManager.applyEvent(SwitchTheme("nonexistent-theme")).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.persisted.theme shouldBe initialState.persisted.theme
  }
