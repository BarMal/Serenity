package com.serenity.ui.theme.config

import java.awt.Color

import com.serenity.ui.theme.Theme

final case class ThemeCreatorRow(
    path: String,
    label: String,
    value: String,
    valid: Boolean,
    previewColor: Option[Color]
)

final case class ThemeCreatorState(
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

  final private case class Descriptor(
      path: String,
      label: String,
      read: ThemeConfig => String,
      write: (ThemeConfig, String) => ThemeConfig,
      isColor: Boolean = true
  )

  private val descriptors: List[Descriptor] =
    List(
      Descriptor("theme.name", "Theme Name", _.name, (config, value) => config.copy(name = value), isColor = false)
    ) ++
      ThemeFieldSchema.uiScalarFields.map(uiScalarDescriptor) ++
      List(
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
        )
      ) ++
      ThemeFieldSchema.uiTokenFields.flatMap(uiTokenDescriptors) ++
      List(
        uiToken(
          "ui.warning.foreground",
          "Warning Foreground",
          ui => ui.warning.getOrElse(ThemeFieldSchema.warningDefault),
          (ui, token) => ui.copy(warning = Some(token)),
          foreground = true
        ),
        uiToken(
          "ui.warning.background",
          "Warning Background",
          ui => ui.warning.getOrElse(ThemeFieldSchema.warningDefault),
          (ui, token) => ui.copy(warning = Some(token)),
          foreground = false
        )
      ) ++
      ThemeFieldSchema.mandatorySyntaxFields.map(field =>
        syntax(field.path, field.label, field.select, field.replace)
      ) ++
      List(
        optionalSyntax(ThemeFieldSchema.typeField),
        optionalSyntax(ThemeFieldSchema.delimiterField),
        optionalSyntax(ThemeFieldSchema.errorField),
        optionalSyntax(ThemeFieldSchema.normalField)
      )

  private def updateUi(update: (UiColors, String) => UiColors): (ThemeConfig, String) => ThemeConfig =
    (config, value) => config.copy(ui = update(config.ui, normalizeColorInput(value)))

  private def uiScalarDescriptor(field: ThemeFieldSchema.UiScalarFieldSchema): Descriptor =
    Descriptor(
      field.path,
      field.label,
      config => field.select(config.ui),
      updateUi((ui, value) => field.replace(ui, value))
    )

  private def uiTokenDescriptors(field: ThemeFieldSchema.UiTokenFieldSchema): List[Descriptor] =
    List(
      uiToken(
        s"${field.path}.foreground",
        s"${field.label} Foreground",
        field.select,
        field.replace,
        foreground = true
      ),
      uiToken(
        s"${field.path}.background",
        s"${field.label} Background",
        field.select,
        field.replace,
        foreground = false
      )
    )

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

  private def optionalSyntax(field: SyntaxFieldSchema): Descriptor =
    Descriptor(
      field.path,
      field.label,
      config => field.select(config.syntax).getOrElse(field.default).foreground,
      (config, value) =>
        val updatedElement =
          field.select(config.syntax).getOrElse(field.default).copy(foreground = normalizeColorInput(value))
        config.copy(syntax = field.replace(config.syntax, updatedElement))
    )

  private def normalizeColorInput(value: String): String =
    value.trim
