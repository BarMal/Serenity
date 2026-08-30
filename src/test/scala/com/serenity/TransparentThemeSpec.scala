package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.ui.theme.DefaultThemes
import com.serenity.ui.theme.config.{ConfigurableThemeManager, ThemeConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The built-in "Transparent" theme (#1240): a concrete theme whose background opts into the alpha-0 sentinel, so TUI
  * mode shows a compositor's own transparency (e.g. kitty's `background_opacity`) through the editor, and GUI mode
  * shows the desktop through the window where per-pixel translucency is available.
  */
class TransparentThemeSpec extends AnyFlatSpec with Matchers:

  "DefaultThemes.transparent" should "have a fully transparent background" in {
    DefaultThemes.transparent.background.getAlpha shouldBe 0
  }

  it should "be registered as an always-available internal theme named \"transparent\"" in {
    DefaultThemes.allInternal should contain key "transparent"
    DefaultThemes.allInternal("transparent") shouldBe DefaultThemes.transparent
  }

  it should "be loadable by name through ConfigurableThemeManager, the same path the theme picker uses" in {
    val manager = new ConfigurableThemeManager(new com.serenity.ui.theme.config.ThemeConfigLoader())
    val theme   = manager.loadThemeByName("transparent").unsafeRunSync()

    theme.background.getAlpha shouldBe 0
    theme.name shouldBe "transparent"
  }

  it should "keep every non-background chrome surface (panel, menu, highlighted) fully opaque" in {
    // Only the application background itself is transparent -- panels, menus, and selection highlights keep their
    // own solid backgrounds so chrome stays legible regardless of what shows through behind the editor.
    val theme = DefaultThemes.transparent
    theme.panel.background.getAlpha shouldBe 255
    theme.menuItem.background.getAlpha shouldBe 255
    theme.highlighted.background.getAlpha shouldBe 255
  }

  it should "build successfully from ThemeConfig.transparent via configToTheme" in {
    val result = ConfigurableThemeManager.configToTheme(ThemeConfig.transparent)
    result shouldBe a[Right[?, ?]]
    result.toOption.get.background.getAlpha shouldBe 0
  }
end TransparentThemeSpec
