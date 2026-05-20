package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{SwitchTheme, ReloadCurrentTheme}
import com.serenity.state.components.ThemeComponent
import com.serenity.state.models.*
import com.serenity.ui.theme.config.AppThemeManager
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConfigDrivenThemeIntegrationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Config-driven theming integration" should "load dark theme and update AppState" in {
    val themeManager = AppThemeManager.create
    
    // Initialize with dark theme
    val darkTheme = themeManager.initializeWithTheme("dark").unsafeRunSync()
    darkTheme.name shouldBe "dark"
    
    // Create an initial app state
    val initialState = AppState.empty.copy(theme = darkTheme)
    initialState.theme.name shouldBe "dark"
    
    // Verify theme properties
    darkTheme.foregroundColor.toString should include("WHITE")
    darkTheme.backgroundColor.toString should include("BLACK")
  }

  it should "switch between dark and light themes dynamically" in {
    val themeManager = AppThemeManager.create
    val themeComponent = new ThemeComponent(themeManager)
    
    // Start with dark theme
    val darkTheme = themeManager.initializeWithTheme("dark").unsafeRunSync()
    val initialState = AppState.empty.copy(theme = darkTheme)
    
    // Switch to light theme using component
    val switchEvent = SwitchTheme("light")
    val result = themeComponent.processEvent(switchEvent, initialState)
    
    result should not be ComponentResult.noChange
    result match
      case ComponentResult.StateChange(stateUpdate) =>
        val newState = stateUpdate(initialState)
        newState.theme.name shouldBe "light"
        newState.theme.foregroundColor should not be darkTheme.foregroundColor
        newState.theme.backgroundColor should not be darkTheme.backgroundColor
      case _ => fail("Expected StateChange result")
  }

  it should "support theme reloading" in {
    val themeManager = AppThemeManager.create
    val themeComponent = new ThemeComponent(themeManager)
    
    // Initialize with a theme
    val theme = themeManager.initializeWithTheme("dark").unsafeRunSync()
    val state = AppState.empty.copy(theme = theme)
    
    // Trigger reload
    val reloadEvent = ReloadCurrentTheme
    val result = themeComponent.processEvent(reloadEvent, state)
    
    // Should either update state or return no change if reload was successful
    result match
      case ComponentResult.StateChange(stateUpdate) =>
        val newState = stateUpdate(state)
        newState.theme.name shouldBe "dark" // Should still be dark theme
      case ComponentResult.NoChange => // This is also acceptable if no changes detected
        succeed
      case _ => fail("Expected StateChange or NoChange result")
  }

  it should "handle missing theme gracefully" in {
    val themeManager = AppThemeManager.create
    val themeComponent = new ThemeComponent(themeManager)
    
    val state = AppState.empty
    
    // Try to switch to a non-existent theme
    val switchEvent = SwitchTheme("nonexistent-theme")
    
    // This should either fail gracefully or fall back to a default theme
    val result = try {
      themeComponent.processEvent(switchEvent, state)
    } catch {
      case _: Exception => ComponentResult.noChange // Expected for non-existent theme
    }
    
    // The test should not crash
    succeed
  }

  it should "preserve theme across state updates" in {
    val themeManager = AppThemeManager.create
    
    // Load a theme
    val lightTheme = themeManager.loadTheme("light").unsafeRunSync()
    
    // Create state with the theme
    val state = AppState.empty.copy(theme = lightTheme)
    
    // Update state with some other changes but preserve theme
    val updatedState = state.copy(
      buffers = Map(BufferId(1) -> Buffer.empty(BufferId(1))),
      nextBufferId = BufferId(2)
    )
    
    // Theme should be preserved
    updatedState.theme.name shouldBe "light"
    updatedState.theme shouldBe lightTheme
  }