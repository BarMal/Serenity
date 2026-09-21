package com.serenity.ui.theme.config

import java.awt.Color

import cats.effect.unsafe.implicits.global
import com.serenity.ui.theme.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers issue #3 of the interaction-state/elevation task: theme files may optionally override these tokens, and every
  * bundled theme file must keep parsing unchanged when they don't.
  */
class VisualStateThemeConfigSpec extends AnyFlatSpec with Matchers:

  private val loader = new ThemeConfigLoader()

  "themes/dark.conf and themes/light.conf" should "still parse with no interaction-state or elevation keys" in {
    val darkConfig  = loader.loadThemeFromResource("themes/dark.conf").unsafeRunSync()
    val lightConfig = loader.loadThemeFromResource("themes/light.conf").unsafeRunSync()

    darkConfig.interactionStates shouldBe None
    darkConfig.elevation shouldBe None
    lightConfig.interactionStates shouldBe None
    lightConfig.elevation shouldBe None
  }

  they should "convert to a Theme whose tokens match that same theme's own derived defaults" in {
    val darkConfig  = loader.loadThemeFromResource("themes/dark.conf").unsafeRunSync()
    val lightConfig = loader.loadThemeFromResource("themes/light.conf").unsafeRunSync()

    val darkTheme  = ConfigurableThemeManager.configToTheme(darkConfig).toOption.get
    val lightTheme = ConfigurableThemeManager.configToTheme(lightConfig).toOption.get

    darkTheme.interactionStates shouldBe Theme.dark.interactionStates
    darkTheme.elevation shouldBe Theme.dark.elevation
    lightTheme.interactionStates shouldBe Theme.light.interactionStates
    lightTheme.elevation shouldBe Theme.light.elevation
  }

  "A theme config" should "let interaction-state and elevation tokens be overridden field-by-field" in {
    val overridden =
      """theme {
        |  name = "override-test"
        |  ui {
        |    foreground = "#FFFFFF"
        |    background = "#000000"
        |    cursor = "#FF0000"
        |    border = "#2F3B4A"
        |    muted = "#7B8794"
        |    placeholder = "#52606D"
        |    highlighted { foreground = "#000000", background = "#5DADE2", style { bold = false, italic = false, underline = false } }
        |    menu-item { foreground = "#FFFFFF", background = "#1F2933", style { bold = false, italic = false, underline = false } }
        |    panel { foreground = "#FFFFFF", background = "#111821", style { bold = false, italic = false, underline = false } }
        |    error { foreground = "#FF6B6B", background = "#2B1215", style { bold = false, italic = false, underline = false } }
        |  }
        |  interaction-states {
        |    hover { foreground = "#FFFFFF", background = "#334455", style { bold = false, italic = false, underline = false } }
        |  }
        |  elevation {
        |    modal {
        |      shadow-opacity = 0.9
        |      surface-tint = "#112233"
        |    }
        |  }
        |  syntax {
        |    keyword { foreground = "#0066CC", style { bold = true, italic = false, underline = false } }
        |    string { foreground = "#00AA00", style { bold = false, italic = false, underline = false } }
        |    comment { foreground = "#666666", style { bold = false, italic = true, underline = false } }
        |    number { foreground = "#FF6600", style { bold = false, italic = false, underline = false } }
        |    operator { foreground = "#AA0000", style { bold = false, italic = false, underline = false } }
        |    identifier { foreground = "#000000", style { bold = false, italic = false, underline = false } }
        |  }
        |}
        |""".stripMargin

    val config                   = loader.loadThemeFromString(overridden).unsafeRunSync()
    val theme                    = ConfigurableThemeManager.configToTheme(config).toOption.get
    val derivedInteractionStates = InteractionStates.derive(theme.menuItem)
    val derivedElevation         = ElevationLevels.derive(theme.foreground, theme.background)

    theme.interactionStates.hover.foreground shouldBe Color.WHITE
    theme.interactionStates.hover.background shouldBe new Color(0x33, 0x44, 0x55)
    // Not overridden -- still the derived default.
    theme.interactionStates.pressed shouldBe derivedInteractionStates.pressed
    theme.interactionStates.disabled shouldBe derivedInteractionStates.disabled

    theme.elevation.forLevel(ElevationLevel.Modal).shadowOpacity shouldBe NormalizedAlpha(0.9)
    theme.elevation.forLevel(ElevationLevel.Modal).surfaceTint shouldBe Some(new Color(0x11, 0x22, 0x33))
    // Not overridden -- still the derived defaults.
    theme.elevation.base shouldBe derivedElevation.base
    theme.elevation.raised shouldBe derivedElevation.raised
    theme.elevation.floating shouldBe derivedElevation.floating
  }
end VisualStateThemeConfigSpec
