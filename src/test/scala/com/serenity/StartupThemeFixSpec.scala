package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.ui.theme.DefaultThemes
import com.serenity.ui.theme.config.{AppThemeManager, ThemeRegistry}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StartupThemeFixSpec extends AnyFlatSpec with Matchers:

  "Default internal themes" should "always be available" in {
    val themeManager = AppThemeManager.create

    // Should not crash and should return a valid theme
    val theme = themeManager.initializeWithTheme().unsafeRunSync()

    theme.name shouldBe "dark"
    theme should not be null
    theme.syntaxColors should not be empty
  }

  "Theme discovery" should "include internal themes" in {
    val themes = ThemeRegistry.getAvailableThemeNames.unsafeRunSync()

    themes should contain("default-dark")
    themes should contain("default-light")
  }

  "Theme discovery" should "work even with no config files" in {
    val themesBySource = ThemeRegistry.getThemesBySource.unsafeRunSync()

    // Internal themes should always be available
    themesBySource.internal should contain("default-dark")
    themesBySource.internal should contain("default-light")
    themesBySource.internal should not be empty
  }

  "AppThemeManager" should "fall back to internal default on failure" in {
    val themeManager = AppThemeManager.create

    // Even if we try to load a non-existent theme, it should fall back
    val theme = themeManager.initializeWithTheme("non-existent-theme").unsafeRunSync()

    // Should fall back to internal default
    theme shouldBe DefaultThemes.default
  }

  "Theme names" should "not include file extensions" in {
    val themes = ThemeRegistry.getAvailableThemeNames.unsafeRunSync()

    // No theme name should contain .conf or .hocon
    themes.foreach { themeName =>
      themeName should not include ".conf"
      themeName should not include ".hocon"
    }
  }
