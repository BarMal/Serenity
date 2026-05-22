package com.serenity.ui.theme.config

import pureconfig.*
import pureconfig.generic.derivation.default.*

/** Configuration representation of a theme that can be loaded from config files */
case class ThemeConfig(
    name: String,
    colors: BaseColors,
    syntax: SyntaxColors
) derives ConfigReader

/** Base colors for the theme (background, foreground, cursor) */
case class BaseColors(
    foreground: String,
    background: String,
    cursor: String
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
