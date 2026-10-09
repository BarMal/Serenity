package com.serenity.ui.terminal

import com.serenity.ui.color.RenderColor
import com.serenity.ui.theme.{InteractionStates, Theme, ThemeColor}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MenuBarPaletteSpec extends AnyFlatSpec with Matchers:

  private def rgb(hex: Int): RenderColor = RenderColor.fromRgba((hex >> 16) & 0xff, (hex >> 8) & 0xff, hex & 0xff)

  /** The values of `themes/high-contrast.conf` from #2088, which is not on this branch. */
  private val highContrast: Theme =
    val white = rgb(0xffffff)
    val black = rgb(0x000000)
    val plain = ThemeColor(white, black)
    Theme.dark.copy(
      name = "high-contrast",
      foreground = white,
      background = black,
      border = rgb(0xffff00),
      panelBorder = rgb(0x00ffff),
      highlighted = ThemeColor(black, rgb(0x00ffff)),
      menuItem = plain,
      panel = plain,
      interactionStates = InteractionStates.derive(plain)
    )

  private val bundled = List("dark" -> Theme.dark, "light" -> Theme.light)

  private def textPairs(palette: MenuBarPalette): List[(String, RenderColor, RenderColor)] =
    List(
      ("bar", palette.barForeground, palette.barBackground),
      ("popup", palette.popupForeground, palette.popupBackground),
      ("selection", palette.selectionForeground, palette.selectionBackground),
      ("hover", palette.hoverForeground, palette.hoverBackground),
      ("accelerator", palette.acceleratorForeground, palette.popupBackground)
    )

  "MenuBarPalette.fromTheme" should "take bar colours from the panel and popup colours from the menu item" in
    bundled.foreach: (_, theme) =>
      val palette = MenuBarPalette.fromTheme(theme)
      palette.barBackground shouldBe theme.panel.background
      palette.barForeground shouldBe theme.panel.foreground
      palette.popupBackground shouldBe theme.menuItem.background
      palette.popupForeground shouldBe theme.menuItem.foreground

  it should "take selection from the highlighted pair and hover and disabled from the interaction states" in
    bundled.foreach: (_, theme) =>
      val palette = MenuBarPalette.fromTheme(theme)
      palette.selectionBackground shouldBe theme.highlighted.background
      palette.selectionForeground shouldBe theme.highlighted.foreground
      palette.hoverBackground shouldBe theme.interactionStates.hover.background
      palette.hoverForeground shouldBe theme.interactionStates.hover.foreground
      palette.disabledForeground shouldBe theme.interactionStates.disabled.foreground

  it should "differ between the light and dark themes" in {
    MenuBarPalette.fromTheme(Theme.light) should not be MenuBarPalette.fromTheme(Theme.dark)
  }

  it should "give every text pair at least 4.5:1 on the bundled light and dark themes" in {
    for
      (name, theme)         <- bundled
      (role, text, surface) <- textPairs(MenuBarPalette.fromTheme(theme))
    do withClue(s"$name $role: ")(Theme.contrastRatio(text, surface) should be >= MenuBarPalette.TextContrastFloor)
  }

  it should "give every text pair at least 7:1 on the high-contrast theme" in
    textPairs(MenuBarPalette.fromTheme(highContrast)).foreach: (role, text, surface) =>
      withClue(s"$role: ")(Theme.contrastRatio(text, surface) should be >= MenuBarPalette.EnhancedTextContrast)

  it should "give border and separator at least 3:1 against the popup on every theme" in
    (bundled :+ ("high-contrast" -> highContrast)).foreach: (name, theme) =>
      val palette = MenuBarPalette.fromTheme(theme)
      withClue(s"$name border: ")(
        Theme.contrastRatio(palette.border, palette.popupBackground) should be >= MenuBarPalette.NonTextContrastFloor
      )
      withClue(s"$name separator: ")(
        Theme.contrastRatio(palette.separator, palette.popupBackground) should be >= MenuBarPalette.NonTextContrastFloor
      )

  it should "replace a border too faint to see with the foreground" in {
    val faint   = Theme.dark.copy(panelBorder = Theme.dark.menuItem.background)
    val palette = MenuBarPalette.fromTheme(faint)
    palette.border shouldBe faint.menuItem.foreground
    palette.separator shouldBe faint.menuItem.foreground
  }
