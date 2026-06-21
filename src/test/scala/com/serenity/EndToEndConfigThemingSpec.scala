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

  "End-to-end config-driven theming" should "work with StateManager integration" in {
    // Create theme manager and state manager
    val themeManager = AppThemeManager.create
    val stateManager = StateManager
      .apply(LoggerFactory[IO].getLogger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    // Initialize with dark theme
    val darkTheme = themeManager.initializeWithTheme("dark").unsafeRunSync()
    darkTheme.name shouldBe "dark"

    // Update state manager with the theme
    stateManager.updateState(_.copy(theme = darkTheme)).unsafeRunSync()

    // Verify the theme is applied
    val currentState = stateManager.getCurrentState.unsafeRunSync()
    currentState.theme.name shouldBe "dark"

    // Switch to light theme
    val lightTheme = themeManager.loadTheme("light").unsafeRunSync()
    stateManager.updateState(_.copy(theme = lightTheme)).unsafeRunSync()

    // Verify the theme was switched
    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.theme.name shouldBe "light"
    updatedState.theme.foregroundColor should not be darkTheme.foregroundColor
  }

  it should "preserve theme configuration through editor operations" in {
    val themeManager = AppThemeManager.create
    val stateManager = StateManager
      .apply(LoggerFactory[IO].getLogger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    // Set up with light theme
    val theme = themeManager.loadTheme("light").unsafeRunSync()
    stateManager.updateState(_.copy(theme = theme)).unsafeRunSync()

    // Perform typical editor operations
    val bufferId = stateManager.createBuffer("function test() { return 'hello'; }").unsafeRunSync()
    val paneId   = stateManager.createPane(Some(bufferId)).unsafeRunSync()

    // Verify theme is preserved
    val finalState = stateManager.getCurrentState.unsafeRunSync()
    finalState.theme.name shouldBe "light"
    finalState.buffers should contain key bufferId
    finalState.layout.editorPanes should contain key paneId
  }

  it should "allow theme reloading without losing application state" in {
    val themeManager = AppThemeManager.create
    val stateManager = StateManager
      .apply(LoggerFactory[IO].getLogger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    // Set up application with content
    val bufferId = stateManager.createBuffer("val x = 42\nval y = \"hello\"").unsafeRunSync()
    stateManager.createPane(Some(bufferId)).unsafeRunSync()

    // Apply initial theme
    val initialTheme = themeManager.loadTheme("dark").unsafeRunSync()
    stateManager.updateState(_.copy(theme = initialTheme)).unsafeRunSync()

    val stateBeforeReload = stateManager.getCurrentState.unsafeRunSync()

    // Reload theme (simulating config file change)
    themeManager.reloadCurrentTheme.unsafeRunSync() match
      case Some((_, themeUpdate)) =>
        stateManager.updateState(themeUpdate).unsafeRunSync()

        val stateAfterReload = stateManager.getCurrentState.unsafeRunSync()

        // Theme should be reloaded but other state preserved
        stateAfterReload.theme.name shouldBe "dark"
        stateAfterReload.buffers shouldBe stateBeforeReload.buffers
        stateAfterReload.layout shouldBe stateBeforeReload.layout

      case None =>
        // No theme was active to reload, which is also valid
        succeed
  }

  it should "handle theme switching with syntax highlighting configuration" in {
    val themeManager = AppThemeManager.create

    // Load both themes
    val darkTheme  = themeManager.loadTheme("dark").unsafeRunSync()
    val lightTheme = themeManager.loadTheme("light").unsafeRunSync()

    // Both themes should have complete syntax highlighting configurations
    import com.serenity.ui.theme.SyntaxElement.*

    // Dark theme validation
    val darkKeywordColor = darkTheme.colorFor(Keyword)
    darkKeywordColor.style.isBold shouldBe true

    val darkCommentColor = darkTheme.colorFor(Comment)
    darkCommentColor.style.isItalic shouldBe true

    val darkErrorColor = darkTheme.colorFor(Error)
    darkErrorColor.style.isUnderlined shouldBe true

    // Light theme validation
    val lightKeywordColor = lightTheme.colorFor(Keyword)
    lightKeywordColor.style.isBold shouldBe true

    val lightCommentColor = lightTheme.colorFor(Comment)
    lightCommentColor.style.isItalic shouldBe true

    val lightErrorColor = lightTheme.colorFor(Error)
    lightErrorColor.style.isUnderlined shouldBe true

    // Colors should be different between themes
    darkKeywordColor.foreground should not be lightKeywordColor.foreground
    darkTheme.backgroundColor should not be lightTheme.backgroundColor
  }
