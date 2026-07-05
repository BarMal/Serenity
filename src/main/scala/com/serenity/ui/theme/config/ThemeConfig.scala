package com.serenity.ui.theme.config

import pureconfig.*
import pureconfig.generic.derivation.default.*

/** Configuration representation of a theme that can be loaded from config files */
case class ThemeConfig(
    name: String,
    ui: UiColors,
    syntax: SyntaxColors
) derives ConfigReader

/** Semantic UI colors for the theme */
case class UiColors(
    foreground: String,
    background: String,
    cursor: String,
    highlighted: UiTokenConfig,
    menuItem: UiTokenConfig,
    panel: UiTokenConfig,
    error: UiTokenConfig,
    warning: Option[UiTokenConfig] = None,
    border: String,
    panelBorder: Option[String] = None,
    margin: Option[String] = None,
    muted: String,
    placeholder: String
) derives ConfigReader

case class UiTokenConfig(
    foreground: String,
    background: String,
    alpha: Option[Double] = None,
    style: StyleConfig = StyleConfig()
) derives ConfigReader

/** Syntax highlighting colors for different code elements */
case class SyntaxColors(
    keyword: SyntaxElementConfig,
    string: SyntaxElementConfig,
    comment: SyntaxElementConfig,
    number: SyntaxElementConfig,
    operator: SyntaxElementConfig,
    identifier: SyntaxElementConfig,
    typ: Option[SyntaxElementConfig] = None,
    delimiter: Option[SyntaxElementConfig] = None,
    whitespace: Option[SyntaxElementConfig] = None,
    error: Option[SyntaxElementConfig] = None,
    normal: Option[SyntaxElementConfig] = None
) derives ConfigReader

/** Configuration for a specific syntax element (color + style) */
case class SyntaxElementConfig(
    foreground: String,
    background: Option[String] = None,
    style: StyleConfig = StyleConfig()
) derives ConfigReader

/** Text styling configuration (bold, italic, underline) */
case class StyleConfig(
    bold: Boolean = false,
    italic: Boolean = false,
    underline: Boolean = false
) derives ConfigReader

object ThemeConfig:

  val defaultDark: ThemeConfig =
    ThemeConfig(
      name = "default-dark",
      ui = UiColors(
        foreground = "#F5F7FA",
        background = "#0B0F14",
        cursor = "#F4D03F",
        highlighted = UiTokenConfig(foreground = "#0B0F14", background = "#5DADE2"),
        menuItem = UiTokenConfig(foreground = "#F5F7FA", background = "#1F2933"),
        panel = UiTokenConfig(foreground = "#F5F7FA", background = "#111821", alpha = Some(0.90)),
        error = UiTokenConfig(foreground = "#FF6B6B", background = "#2B1215"),
        warning = Some(UiTokenConfig(foreground = "#F0B429", background = "#2B2000")),
        border = "#2F3B4A",
        panelBorder = Some("#2F3B4A"),
        margin = Some("#0B0F14"),
        muted = "#7B8794",
        placeholder = "#52606D"
      ),
      syntax = SyntaxColors(
        keyword = SyntaxElementConfig("#5DADE2", Some("#0B0F14"), StyleConfig(bold = true)),
        string = SyntaxElementConfig("#58D68D", Some("#0B0F14")),
        comment = SyntaxElementConfig("#808B96", Some("#0B0F14"), StyleConfig(italic = true)),
        number = SyntaxElementConfig("#F5B041", Some("#0B0F14")),
        operator = SyntaxElementConfig("#EC7063", Some("#0B0F14")),
        identifier = SyntaxElementConfig("#F5F7FA", Some("#0B0F14")),
        typ = Some(SyntaxElementConfig("#AF7AC5", Some("#0B0F14"), StyleConfig(bold = true))),
        delimiter = Some(SyntaxElementConfig("#D5D8DC", Some("#0B0F14"))),
        whitespace = Some(SyntaxElementConfig("#0B0F14", Some("#0B0F14"))),
        error = Some(SyntaxElementConfig("#FF6B6B", Some("#2B1215"), StyleConfig(bold = true, underline = true))),
        normal = Some(SyntaxElementConfig("#F5F7FA", Some("#0B0F14")))
      )
    )

  val defaultLight: ThemeConfig =
    ThemeConfig(
      name = "default-light",
      ui = UiColors(
        foreground = "#102A43",
        background = "#FDFDFD",
        cursor = "#0066CC",
        highlighted = UiTokenConfig(foreground = "#FDFDFD", background = "#0066CC"),
        menuItem = UiTokenConfig(foreground = "#102A43", background = "#D9E2EC"),
        panel = UiTokenConfig(foreground = "#102A43", background = "#EFF3F8", alpha = Some(0.90)),
        error = UiTokenConfig(foreground = "#B00020", background = "#FDECEC"),
        warning = Some(UiTokenConfig(foreground = "#945802", background = "#FFFAEC")),
        border = "#BCCCDC",
        panelBorder = Some("#BCCCDC"),
        margin = Some("#FDFDFD"),
        muted = "#61758A",
        placeholder = "#829AB1"
      ),
      syntax = SyntaxColors(
        keyword = SyntaxElementConfig("#0033CC", Some("#FDFDFD"), StyleConfig(bold = true)),
        string = SyntaxElementConfig("#00875A", Some("#FDFDFD")),
        comment = SyntaxElementConfig("#66788A", Some("#FDFDFD"), StyleConfig(italic = true)),
        number = SyntaxElementConfig("#CC6600", Some("#FDFDFD")),
        operator = SyntaxElementConfig("#CC0000", Some("#FDFDFD")),
        identifier = SyntaxElementConfig("#102A43", Some("#FDFDFD")),
        typ = Some(SyntaxElementConfig("#7A0099", Some("#FDFDFD"), StyleConfig(bold = true))),
        delimiter = Some(SyntaxElementConfig("#334E68", Some("#FDFDFD"))),
        whitespace = Some(SyntaxElementConfig("#FDFDFD", Some("#FDFDFD"))),
        error = Some(SyntaxElementConfig("#B00020", Some("#FDECEC"), StyleConfig(bold = true, underline = true))),
        normal = Some(SyntaxElementConfig("#102A43", Some("#FDFDFD")))
      )
    )
