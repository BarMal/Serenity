package com.serenity.ui.theme.config

import com.serenity.ui.theme.SyntaxElement

/** One optional `SyntaxColors` field: which slot it reads/writes, its HOCON path and creator-UI label, and the
  * fallback value used when the field is absent from config.
  */
final case class SyntaxFieldSchema(
    element: SyntaxElement,
    path: String,
    label: String,
    select: SyntaxColors => Option[SyntaxElementConfig],
    replace: (SyntaxColors, SyntaxElementConfig) => SyntaxColors,
    default: SyntaxElementConfig
)

/** Canonical list of `SyntaxColors`' five optional fields (`typ`, `delimiter`, `whitespace`, `error`, `normal`),
  * with the fallback each one takes when a theme config omits it.
  *
  * `ConfigurableThemeManager` (config -> domain), `ThemeConfigWriter` (domain -> config -> text) and
  * `ThemeCreatorState` (creator UI rows) each independently re-declared this same five-element list and its
  * defaults, and those copies had drifted out of sync with each other (issue #1410) -- e.g. the `normal` fallback
  * was `"#F5F7FA"` in one place and the theme's own `ui.foreground` in another. This is now the one place that
  * list is declared; the defaults below are `ConfigurableThemeManager`'s previous values, since that module is
  * what actually determines the color rendered for a config missing one of these fields, so keeping them exactly
  * as they were changes duplication, not behavior.
  */
object ThemeFieldSchema:

  /** Fallback for `UiColors.warning` when a theme config omits it -- also previously redeclared identically in
    * `ConfigurableThemeManager`, `ThemeConfigWriter` and `ThemeCreatorState`.
    */
  val warningDefault: UiTokenConfig = UiTokenConfig(foreground = "#F0B429", background = "#2B2000")

  val typeField: SyntaxFieldSchema = SyntaxFieldSchema(
    element = SyntaxElement.Type,
    path = "syntax.type.foreground",
    label = "Type",
    select = _.typ,
    replace = (syntax, value) => syntax.copy(typ = Some(value)),
    default = SyntaxElementConfig("#AF7AC5", None, StyleConfig(bold = true))
  )

  val delimiterField: SyntaxFieldSchema = SyntaxFieldSchema(
    element = SyntaxElement.Delimiter,
    path = "syntax.delimiter.foreground",
    label = "Delimiter",
    select = _.delimiter,
    replace = (syntax, value) => syntax.copy(delimiter = Some(value)),
    default = SyntaxElementConfig("#F5F7FA", None, StyleConfig())
  )

  val whitespaceField: SyntaxFieldSchema = SyntaxFieldSchema(
    element = SyntaxElement.Whitespace,
    path = "syntax.whitespace.foreground",
    label = "Whitespace",
    select = _.whitespace,
    replace = (syntax, value) => syntax.copy(whitespace = Some(value)),
    default = SyntaxElementConfig("#000000", None, StyleConfig())
  )

  val errorField: SyntaxFieldSchema = SyntaxFieldSchema(
    element = SyntaxElement.Error,
    path = "syntax.error.foreground",
    label = "Syntax Error",
    select = _.error,
    replace = (syntax, value) => syntax.copy(error = Some(value)),
    default = SyntaxElementConfig("#FF6B6B", None, StyleConfig(underline = true))
  )

  val normalField: SyntaxFieldSchema = SyntaxFieldSchema(
    element = SyntaxElement.Normal,
    path = "syntax.normal.foreground",
    label = "Normal Text",
    select = _.normal,
    replace = (syntax, value) => syntax.copy(normal = Some(value)),
    default = SyntaxElementConfig("#F5F7FA", None, StyleConfig())
  )

  val syntaxFields: List[SyntaxFieldSchema] =
    List(typeField, delimiterField, whitespaceField, errorField, normalField)
