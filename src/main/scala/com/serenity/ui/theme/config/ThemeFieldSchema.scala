package com.serenity.ui.theme.config

import java.awt.Color

import com.serenity.ui.theme.{SyntaxElement, Theme, ThemeColor}

/** One optional `SyntaxColors` field: which slot it reads/writes, its HOCON path and creator-UI label, and the fallback
  * value used when the field is absent from config.
  */
final case class SyntaxFieldSchema(
    element: SyntaxElement,
    path: String,
    label: String,
    select: SyntaxColors => Option[SyntaxElementConfig],
    replace: (SyntaxColors, SyntaxElementConfig) => SyntaxColors,
    default: SyntaxElementConfig
)

/** Canonical list of `SyntaxColors`' five optional fields (`typ`, `delimiter`, `whitespace`, `error`, `normal`), with
  * the fallback each one takes when a theme config omits it.
  *
  * `ConfigurableThemeManager` (config -> domain), `ThemeConfigWriter` (domain -> config -> text) and
  * `ThemeCreatorState` (creator UI rows) each independently re-declared this same five-element list and its defaults,
  * and those copies had drifted out of sync with each other (issue #1410) -- e.g. the `normal` fallback was `"#F5F7FA"`
  * in one place and the theme's own `ui.foreground` in another. This is now the one place that list is declared; the
  * defaults below are `ConfigurableThemeManager`'s previous values, since that module is what actually determines the
  * color rendered for a config missing one of these fields, so keeping them exactly as they were changes duplication,
  * not behavior.
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

  // ── Mandatory fields ─────────────────────────────────────────────────────
  //
  // Unlike the optional fields above, these are always present on both `ThemeConfig` and `Theme` -- there is no
  // fallback-default concept for them, so each descriptor instead carries a `themeValue` accessor for the
  // domain -> config direction (`ThemeConfigWriter.themeToConfig`) alongside the config-side `select`/`replace` pair
  // used by `ConfigurableThemeManager`, `ThemeConfigWriter.render` and `ThemeCreatorState`.

  /** One mandatory `UiColors` field stored as a plain hex-color string (`foreground`, `background`, `cursor`, `border`,
    * `muted`, `placeholder`).
    */
  final case class UiScalarFieldSchema(
      path: String,
      hoconKey: String,
      label: String,
      select: UiColors => String,
      replace: (UiColors, String) => UiColors,
      themeValue: Theme => Color
  )

  val uiForegroundField: UiScalarFieldSchema =
    UiScalarFieldSchema(
      "ui.foreground",
      "foreground",
      "Foreground",
      _.foreground,
      (ui, v) => ui.copy(foreground = v),
      _.foreground
    )

  val uiBackgroundField: UiScalarFieldSchema =
    UiScalarFieldSchema(
      "ui.background",
      "background",
      "Background",
      _.background,
      (ui, v) => ui.copy(background = v),
      _.background
    )

  val uiCursorField: UiScalarFieldSchema =
    UiScalarFieldSchema("ui.cursor", "cursor", "Cursor", _.cursor, (ui, v) => ui.copy(cursor = v), _.cursor)

  val uiBorderField: UiScalarFieldSchema =
    UiScalarFieldSchema("ui.border", "border", "Border", _.border, (ui, v) => ui.copy(border = v), _.border)

  val uiMutedField: UiScalarFieldSchema =
    UiScalarFieldSchema("ui.muted", "muted", "Muted Text", _.muted, (ui, v) => ui.copy(muted = v), _.muted)

  val uiPlaceholderField: UiScalarFieldSchema =
    UiScalarFieldSchema(
      "ui.placeholder",
      "placeholder",
      "Placeholder",
      _.placeholder,
      (ui, v) => ui.copy(placeholder = v),
      _.placeholder
    )

  val uiScalarFields: List[UiScalarFieldSchema] =
    List(uiForegroundField, uiBackgroundField, uiCursorField, uiBorderField, uiMutedField, uiPlaceholderField)

  /** One mandatory `UiColors` field stored as a `UiTokenConfig` (`highlighted`, `menuItem`, `panel`, `error`). `path`
    * and `label` are prefixes -- consumers that expose separate foreground/background rows (`ThemeCreatorState`) suffix
    * them with `.foreground`/`.background` and `" Foreground"`/`" Background"`.
    */
  final case class UiTokenFieldSchema(
      path: String,
      hoconKey: String,
      label: String,
      select: UiColors => UiTokenConfig,
      replace: (UiColors, UiTokenConfig) => UiColors,
      themeValue: Theme => ThemeColor
  )

  val uiHighlightedField: UiTokenFieldSchema =
    UiTokenFieldSchema(
      "ui.highlighted",
      "highlighted",
      "Highlight",
      _.highlighted,
      (ui, v) => ui.copy(highlighted = v),
      _.highlighted
    )

  val uiMenuItemField: UiTokenFieldSchema =
    UiTokenFieldSchema("ui.menu-item", "menu-item", "Menu", _.menuItem, (ui, v) => ui.copy(menuItem = v), _.menuItem)

  val uiPanelField: UiTokenFieldSchema =
    UiTokenFieldSchema("ui.panel", "panel", "Panel", _.panel, (ui, v) => ui.copy(panel = v), _.panel)

  val uiErrorField: UiTokenFieldSchema =
    UiTokenFieldSchema("ui.error", "error", "Error", _.error, (ui, v) => ui.copy(error = v), _.error)

  val uiTokenFields: List[UiTokenFieldSchema] =
    List(uiHighlightedField, uiMenuItemField, uiPanelField, uiErrorField)

  /** One mandatory `SyntaxColors` element (`keyword`, `string`, `comment`, `number`, `operator`, `identifier`). See
    * `SyntaxFieldSchema` for its five optional counterparts; no `themeValue` accessor is needed here since the domain
    * side is already keyed generically by `SyntaxElement` via `Theme.colorFor`/`Theme.syntaxColors`.
    */
  final case class MandatorySyntaxFieldSchema(
      element: SyntaxElement,
      path: String,
      hoconKey: String,
      label: String,
      select: SyntaxColors => SyntaxElementConfig,
      replace: (SyntaxColors, SyntaxElementConfig) => SyntaxColors
  )

  val keywordField: MandatorySyntaxFieldSchema = MandatorySyntaxFieldSchema(
    SyntaxElement.Keyword,
    "syntax.keyword.foreground",
    "keyword",
    "Keyword",
    _.keyword,
    (syntax, v) => syntax.copy(keyword = v)
  )

  val stringField: MandatorySyntaxFieldSchema = MandatorySyntaxFieldSchema(
    SyntaxElement.String,
    "syntax.string.foreground",
    "string",
    "String",
    _.string,
    (syntax, v) => syntax.copy(string = v)
  )

  val commentField: MandatorySyntaxFieldSchema = MandatorySyntaxFieldSchema(
    SyntaxElement.Comment,
    "syntax.comment.foreground",
    "comment",
    "Comment",
    _.comment,
    (syntax, v) => syntax.copy(comment = v)
  )

  val numberField: MandatorySyntaxFieldSchema = MandatorySyntaxFieldSchema(
    SyntaxElement.Number,
    "syntax.number.foreground",
    "number",
    "Number",
    _.number,
    (syntax, v) => syntax.copy(number = v)
  )

  val operatorField: MandatorySyntaxFieldSchema = MandatorySyntaxFieldSchema(
    SyntaxElement.Operator,
    "syntax.operator.foreground",
    "operator",
    "Operator",
    _.operator,
    (syntax, v) => syntax.copy(operator = v)
  )

  val identifierField: MandatorySyntaxFieldSchema = MandatorySyntaxFieldSchema(
    SyntaxElement.Identifier,
    "syntax.identifier.foreground",
    "identifier",
    "Identifier",
    _.identifier,
    (syntax, v) => syntax.copy(identifier = v)
  )

  val mandatorySyntaxFields: List[MandatorySyntaxFieldSchema] =
    List(keywordField, stringField, commentField, numberField, operatorField, identifierField)
