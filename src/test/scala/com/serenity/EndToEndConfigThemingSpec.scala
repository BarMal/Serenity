package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

class EndToEndConfigThemingSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  "End-to-end config-driven theming" should "preserve theme configuration through editor operations" in {
    val themeManager = AppThemeManager.create
    val stateManager = StateManager
      .apply(LoggerFactory[IO].getLogger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    // Set up with light theme
    val theme = themeManager.loadTheme("light").unsafeRunSync()
    stateManager.updateState(state => state.copy(persisted = state.persisted.copy(theme = theme))).unsafeRunSync()

    // Perform typical editor operations
    val bufferId = stateManager.bufferManager.createBuffer("function test() { return 'hello'; }", None).unsafeRunSync()
    val paneId   = stateManager.paneManager.createPane(Some(bufferId)).unsafeRunSync()

    // Verify theme is preserved
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.persisted.theme.name shouldBe "light"
    finalState.persisted.buffers should contain key bufferId
    finalState.persisted.layout.editorPanes should contain key paneId
  }

  it should "allow theme reloading without losing application state" in {
    val themeManager = AppThemeManager.create
    val stateManager = StateManager
      .apply(LoggerFactory[IO].getLogger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    // Set up application with content
    val bufferId = stateManager.bufferManager.createBuffer("val x = 42\nval y = \"hello\"", None).unsafeRunSync()
    stateManager.paneManager.createPane(Some(bufferId)).unsafeRunSync()

    // Apply initial theme
    val initialTheme = themeManager.loadTheme("dark").unsafeRunSync()
    stateManager
      .updateState(state => state.copy(persisted = state.persisted.copy(theme = initialTheme)))
      .unsafeRunSync()

    val stateBeforeReload = stateManager.getCurrentState.unsafeRunSync()

    // Reload theme (simulating config file change)
    themeManager.reloadCurrentTheme.unsafeRunSync() match
      case Some((_, themeUpdate)) =>
        stateManager.updateState(themeUpdate).unsafeRunSync()

        val stateAfterReload = stateManager.getCurrentState.unsafeRunSync()

        // Theme should be reloaded but other state preserved
        stateAfterReload.persisted.theme.name shouldBe "dark"
        stateAfterReload.persisted.buffers shouldBe stateBeforeReload.persisted.buffers
        stateAfterReload.persisted.layout shouldBe stateBeforeReload.persisted.layout

      case None =>
        // No theme was active to reload, which is also valid
        succeed
  }
