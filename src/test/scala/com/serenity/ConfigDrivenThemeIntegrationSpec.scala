package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{ReloadCurrentTheme, SwitchTheme}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class ConfigDrivenThemeIntegrationSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("ConfigDrivenThemeIntegrationSpec"))
    StateManager.apply(logger).unsafeRunSync()

  "Config-driven theming integration" should "load dark theme and update AppState" in {
    val themeManager = AppThemeManager.create

    // Initialize with dark theme
    val darkTheme = themeManager.initializeWithTheme("dark").unsafeRunSync()
    darkTheme.name shouldBe "dark"

    // Create an initial app state
    val initialState = AppState.empty.copy(theme = darkTheme)
    initialState.theme.name shouldBe "dark"

    // Verify theme properties
    darkTheme.foregroundColor shouldBe a[java.awt.Color]
    darkTheme.backgroundColor shouldBe a[java.awt.Color]
  }

  it should "switch between dark and light themes dynamically" in {
    val stateManager = createStateManager()

    stateManager
      .updateState(_.copy(theme = AppThemeManager.create.initializeWithTheme("dark").unsafeRunSync()))
      .unsafeRunSync()
    val initialState = stateManager.getCurrentState.unsafeRunSync()
    val darkTheme    = initialState.theme

    stateManager.applyEvent(SwitchTheme("light")).unsafeRunSync()
    val updatedState = stateManager.getCurrentState.unsafeRunSync()

    updatedState.theme.name shouldBe "light"
    updatedState.theme.foregroundColor should not be darkTheme.foregroundColor
    updatedState.theme.backgroundColor should not be darkTheme.backgroundColor
  }

  it should "support theme reloading" in {
    val stateManager = createStateManager()
    stateManager
      .updateState(_.copy(theme = AppThemeManager.create.initializeWithTheme("dark").unsafeRunSync()))
      .unsafeRunSync()

    stateManager.applyEvent(ReloadCurrentTheme).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.theme.name shouldBe "dark"
  }

  it should "handle missing theme gracefully" in {
    val stateManager = createStateManager()
    val initialState = stateManager.getCurrentState.unsafeRunSync()

    noException should be thrownBy stateManager.applyEvent(SwitchTheme("nonexistent-theme")).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.theme shouldBe initialState.theme
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
