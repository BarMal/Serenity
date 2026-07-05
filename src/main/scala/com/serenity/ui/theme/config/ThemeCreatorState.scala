package com.serenity.ui.theme.config

import java.awt.Color

import com.serenity.ui.theme.Theme

case class ThemeCreatorRow(
    path: String,
    label: String,
    value: String,
    valid: Boolean,
    previewColor: Option[Color]
)

case class ThemeCreatorState(
    originalTheme: Theme,
    draftConfig: ThemeConfig,
    selectedIndex: Int,
    statusMessage: Option[String] = None
):

  def rows: List[ThemeCreatorRow] =
    ThemeCreatorState.descriptors.map { descriptor =>
      val value = descriptor.read(draftConfig)
      val preview =
        if descriptor.isColor then ColorParser.parseColor(value).toOption
        else None
      ThemeCreatorRow(
        path = descriptor.path,
        label = descriptor.label,
        value = value,
        valid = !descriptor.isColor || preview.nonEmpty,
        previewColor = preview
      )
    }

  def selectedRow: Option[ThemeCreatorRow] =
    rows.lift(selectedIndex)

  def moveSelection(delta: Int): ThemeCreatorState =
    if rows.isEmpty then this
    else
      val rawIndex     = (selectedIndex + delta) % rows.length
      val wrappedIndex = if rawIndex < 0 then rows.length + rawIndex else rawIndex
      copy(selectedIndex = wrappedIndex, statusMessage = None)

  def selectPath(path: String): ThemeCreatorState =
    val index = ThemeCreatorState.descriptors.indexWhere(_.path == path)
    if index < 0 then this else copy(selectedIndex = index, statusMessage = None)

  def replaceSelectedValue(value: String): ThemeCreatorState =
    selectedDescriptor
      .map(descriptor =>
        copy(
          draftConfig = descriptor.write(draftConfig, value),
          statusMessage = None
        )
      )
      .getOrElse(this)

  def insertChar(char: Char): ThemeCreatorState =
    selectedRow.map(row => replaceSelectedValue(row.value + char)).getOrElse(this)

  def deleteBackward: ThemeCreatorState =
    selectedRow.map(row => replaceSelectedValue(row.value.dropRight(1))).getOrElse(this)

  def previewTheme: Either[String, Theme] =
    ConfigurableThemeManager.configToTheme(draftConfig)

  def validConfig: Either[String, ThemeConfig] =
    previewTheme.map(_ => draftConfig)

  def withStatus(message: String): ThemeCreatorState =
    copy(statusMessage = Some(message))

  private def selectedDescriptor: Option[ThemeCreatorState.Descriptor] =
    ThemeCreatorState.descriptors.lift(selectedIndex)

object ThemeCreatorState:

  def fromTheme(theme: Theme): ThemeCreatorState =
    ThemeCreatorState(
      originalTheme = theme,
      draftConfig = ThemeConfigWriter.themeToConfig(theme),
      selectedIndex = 0
    )

  private[config] case class Descriptor(
      path: String,
      label: String,
      read: ThemeConfig => String,
      write: (ThemeConfig, String) => ThemeConfig,
      isColor: Boolean = true
  )

  val descriptors: List[Descriptor] =
    List(
      Descriptor("theme.name", "Theme Name", _.name, (config, value) => config.copy(name = value), isColor = false),
      Descriptor("ui.foreground", "Foreground", _.ui.foreground, updateUi((ui, value) => ui.copy(foreground = value))),
      Descriptor("ui.background", "Background", _.ui.background, updateUi((ui, value) => ui.copy(background = value))),
      Descriptor("ui.cursor", "Cursor", _.ui.cursor, updateUi((ui, value) => ui.copy(cursor = value))),
      Descriptor("ui.border", "Border", _.ui.border, updateUi((ui, value) => ui.copy(border = value))),
      Descriptor(
        "ui.panel-border",
        "Panel Border",
        config => config.ui.panelBorder.getOrElse(config.ui.border),
        updateUi((ui, value) => ui.copy(panelBorder = Some(value)))
      ),
      Descriptor(
        "ui.margin",
        "Margin",
        config => config.ui.margin.getOrElse(config.ui.background),
        updateUi((ui, value) => ui.copy(margin = Some(value)))
      ),
      Descriptor("ui.muted", "Muted Text", _.ui.muted, updateUi((ui, value) => ui.copy(muted = value))),
      Descriptor(
        "ui.placeholder",
        "Placeholder",
        _.ui.placeholder,
        updateUi((ui, value) => ui.copy(placeholder = value))
      ),
      uiToken(
        "ui.highlighted.foreground",
        "Highlight Foreground",
        _.highlighted,
        (ui, token) => ui.copy(highlighted = token),
        foreground = true
      ),
      uiToken(
        "ui.highlighted.background",
        "Highlight Background",
        _.highlighted,
        (ui, token) => ui.copy(highlighted = token),
        foreground = false
      ),
      uiToken(
        "ui.menu-item.foreground",
        "Menu Foreground",
        _.menuItem,
        (ui, token) => ui.copy(menuItem = token),
        foreground = true
      ),
      uiToken(
        "ui.menu-item.background",
        "Menu Background",
        _.menuItem,
        (ui, token) => ui.copy(menuItem = token),
        foreground = false
      ),
      uiToken(
        "ui.panel.foreground",
        "Panel Foreground",
        _.panel,
        (ui, token) => ui.copy(panel = token),
        foreground = true
      ),
      uiToken(
        "ui.panel.background",
        "Panel Background",
        _.panel,
        (ui, token) => ui.copy(panel = token),
        foreground = false
      ),
      uiToken(
        "ui.error.foreground",
        "Error Foreground",
        _.error,
        (ui, token) => ui.copy(error = token),
        foreground = true
      ),
      uiToken(
        "ui.error.background",
        "Error Background",
        _.error,
        (ui, token) => ui.copy(error = token),
        foreground = false
      ),
      uiToken(
        "ui.warning.foreground",
        "Warning Foreground",
        ui => ui.warning.getOrElse(UiTokenConfig("#F0B429", "#2B2000")),
        (ui, token) => ui.copy(warning = Some(token)),
        foreground = true
      ),
      uiToken(
        "ui.warning.background",
        "Warning Background",
        ui => ui.warning.getOrElse(UiTokenConfig("#F0B429", "#2B2000")),
        (ui, token) => ui.copy(warning = Some(token)),
        foreground = false
      ),
      syntax("syntax.keyword.foreground", "Keyword", _.keyword, (syntax, value) => syntax.copy(keyword = value)),
      syntax("syntax.string.foreground", "String", _.string, (syntax, value) => syntax.copy(string = value)),
      syntax("syntax.comment.foreground", "Comment", _.comment, (syntax, value) => syntax.copy(comment = value)),
      syntax("syntax.number.foreground", "Number", _.number, (syntax, value) => syntax.copy(number = value)),
      syntax("syntax.operator.foreground", "Operator", _.operator, (syntax, value) => syntax.copy(operator = value)),
      syntax(
        "syntax.identifier.foreground",
        "Identifier",
        _.identifier,
        (syntax, value) => syntax.copy(identifier = value)
      ),
      optionalSyntax("syntax.type.foreground", "Type", _.typ, (syntax, value) => syntax.copy(typ = Some(value))),
      optionalSyntax(
        "syntax.delimiter.foreground",
        "Delimiter",
        _.delimiter,
        (syntax, value) => syntax.copy(delimiter = Some(value))
      ),
      optionalSyntax(
        "syntax.error.foreground",
        "Syntax Error",
        _.error,
        (syntax, value) => syntax.copy(error = Some(value))
      ),
      optionalSyntax(
        "syntax.normal.foreground",
        "Normal Text",
        _.normal,
        (syntax, value) => syntax.copy(normal = Some(value))
      )
    )

  private def updateUi(update: (UiColors, String) => UiColors): (ThemeConfig, String) => ThemeConfig =
    (config, value) => config.copy(ui = update(config.ui, normalizeColorInput(value)))

  private def uiToken(
    path: String,
    label: String,
    select: UiColors => UiTokenConfig,
    replace: (UiColors, UiTokenConfig) => UiColors,
    foreground: Boolean
  ): Descriptor =
    Descriptor(
      path,
      label,
      config =>
        val token = select(config.ui)
        if foreground then token.foreground else token.background
      ,
      (config, value) =>
        val token = select(config.ui)
        val updated =
          if foreground then token.copy(foreground = normalizeColorInput(value))
          else token.copy(background = normalizeColorInput(value))
        config.copy(ui = replace(config.ui, updated))
    )

  private def syntax(
    path: String,
    label: String,
    select: SyntaxColors => SyntaxElementConfig,
    replace: (SyntaxColors, SyntaxElementConfig) => SyntaxColors
  ): Descriptor =
    Descriptor(
      path,
      label,
      config => select(config.syntax).foreground,
      (config, value) =>
        val updatedElement = select(config.syntax).copy(foreground = normalizeColorInput(value))
        config.copy(syntax = replace(config.syntax, updatedElement))
    )

  private def optionalSyntax(
    path: String,
    label: String,
    select: SyntaxColors => Option[SyntaxElementConfig],
    replace: (SyntaxColors, SyntaxElementConfig) => SyntaxColors
  ): Descriptor =
    Descriptor(
      path,
      label,
      config => select(config.syntax).map(_.foreground).getOrElse(config.ui.foreground),
      (config, value) =>
        val updatedElement = select(config.syntax)
          .getOrElse(SyntaxElementConfig(config.ui.foreground, Some(config.ui.background)))
          .copy(foreground = normalizeColorInput(value))
        config.copy(syntax = replace(config.syntax, updatedElement))
    )

  private def normalizeColorInput(value: String): String =
    value.trim
