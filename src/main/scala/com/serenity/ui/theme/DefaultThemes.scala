package com.serenity.ui.theme

import scala.util.Failure

import com.serenity.ui.theme.config.{ConfigurableThemeManager, ThemeConfig}

object DefaultThemes:

  /** Bundled theme configs (ThemeConfig.defaultDark/defaultLight) are fixed source, not user input -- an invalid one
    * here is a genuine programming error, so this fails fast at class-init time, the outermost boundary for this
    * module. Failure(...).get raises instead of a literal `throw`.
    */
  private def load(config: ThemeConfig): Theme =
    ConfigurableThemeManager.configToTheme(config) match
      case Right(theme) => theme
      case Left(error)  => Failure[Theme](new IllegalStateException(s"Invalid bundled theme config: $error")).get

  /** Internal default dark theme - always available, no external config files required */
  val defaultDark: Theme = load(ThemeConfig.defaultDark)

  /** Internal default light theme - always available, no external config files required */
  val defaultLight: Theme = load(ThemeConfig.defaultLight)

  /** Internal transparent theme - background alpha 0, for use with a terminal or window compositor's own transparency
    * (e.g. kitty's `background_opacity`). See `ThemeConfig.transparent`'s doc comment for the palette rationale, and
    * `TerminalAnsiDiff.sgr`/`Java2DRenderSurface`/`SwingWindow` for how the alpha-0 sentinel is actually rendered in
    * TUI and GUI mode respectively.
    */
  val transparent: Theme = load(ThemeConfig.transparent)

  /** Get all internal themes */
  val allInternal: Map[String, Theme] = Map(
    "default-dark"  -> defaultDark,
    "default-light" -> defaultLight,
    "transparent"   -> transparent
  )

  /** Get default theme (fallback) */
  val default: Theme = defaultDark
