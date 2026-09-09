package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.SwitchTheme
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
    val initialState = AppState.empty.copy(persisted = AppState.empty.persisted.copy(theme = darkTheme))
    initialState.persisted.theme.name shouldBe "dark"

    // Verify theme properties
    darkTheme.foregroundColor shouldBe a[java.awt.Color]
    darkTheme.backgroundColor shouldBe a[java.awt.Color]
  }

  it should "handle missing theme gracefully" in {
    val stateManager = createStateManager()
    val initialState = stateManager.getCurrentState.unsafeRunSync()

    noException should be thrownBy stateManager.applyEvent(SwitchTheme("nonexistent-theme")).unsafeRunSync()

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.persisted.theme shouldBe initialState.persisted.theme
  }
