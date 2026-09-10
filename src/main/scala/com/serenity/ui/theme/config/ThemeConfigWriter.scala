package com.serenity.ui.theme.config

import java.awt.Color
import java.nio.file.Path

import cats.effect.IO
import com.serenity.io.AtomicFileWriter
import com.serenity.ui.theme.{ColorFormat, SyntaxElement, Theme, ThemeColor}

object ThemeConfigWriter:

  def themeToConfig(theme: Theme): ThemeConfig =
    ThemeConfig(
      name = theme.name,
      ui = UiColors(
        foreground = hex(theme.foreground),
        background = hex(theme.background),
        cursor = hex(theme.cursor),
        highlighted = tokenConfig(theme.highlighted),
        menuItem = tokenConfig(theme.menuItem),
        panel = tokenConfig(theme.panel),
        error = tokenConfig(theme.error),
        warning = Some(tokenConfig(theme.warning)),
        border = hex(theme.border),
        panelBorder = Some(hex(theme.panelBorder)),
        margin = Some(hex(theme.margin)),
        muted = hex(theme.muted),
        placeholder = hex(theme.placeholder)
      ),
      syntax = SyntaxColors(
        keyword = syntaxConfig(theme, SyntaxElement.Keyword),
        string = syntaxConfig(theme, SyntaxElement.String),
        comment = syntaxConfig(theme, SyntaxElement.Comment),
        number = syntaxConfig(theme, SyntaxElement.Number),
        operator = syntaxConfig(theme, SyntaxElement.Operator),
        identifier = syntaxConfig(theme, SyntaxElement.Identifier),
        typ = Some(syntaxConfig(theme, SyntaxElement.Type)),
        delimiter = Some(syntaxConfig(theme, SyntaxElement.Delimiter)),
        whitespace = Some(syntaxConfig(theme, SyntaxElement.Whitespace)),
        error = Some(syntaxConfig(theme, SyntaxElement.Error)),
        normal = Some(syntaxConfig(theme, SyntaxElement.Normal))
      )
    )

  def write(config: ThemeConfig, path: Path): IO[Unit] =
    AtomicFileWriter.writeString(path, render(config))

  def writeUserTheme(config: ThemeConfig, loader: ThemeConfigLoader = ThemeConfigLoader()): IO[Path] =
    for
      dir <- loader.ensureUserThemesDirectory
      path = dir.resolve(s"${fileNameFor(config.name)}.conf")
      _ <- write(config, path)
    yield path

  def fileNameFor(themeName: String): String =
    val sanitized =
      themeName.trim.toLowerCase
        .replaceAll("[^a-z0-9._-]+", "-")
        .replaceAll("^-+|-+$", "")
    if sanitized.isEmpty then "custom-theme" else sanitized

  def render(config: ThemeConfig): String =
    def optional(field: SyntaxFieldSchema): String =
      renderSyntax(field.select(config.syntax).getOrElse(field.default), 4)

    s"""theme {
       |  name = "${escape(config.name)}"
       |  ui {
       |    foreground = "${config.ui.foreground}"
       |    background = "${config.ui.background}"
       |    cursor = "${config.ui.cursor}"
       |    border = "${config.ui.border}"
       |    panel-border = "${config.ui.panelBorder.getOrElse(config.ui.border)}"
       |    margin = "${config.ui.margin.getOrElse(config.ui.background)}"
       |    muted = "${config.ui.muted}"
       |    placeholder = "${config.ui.placeholder}"
       |    highlighted ${renderToken(config.ui.highlighted, 4)}
       |    menu-item ${renderToken(config.ui.menuItem, 4)}
       |    panel ${renderToken(config.ui.panel, 4)}
       |    error ${renderToken(config.ui.error, 4)}
       |    warning ${renderToken(config.ui.warning.getOrElse(ThemeFieldSchema.warningDefault), 4)}
       |  }
       |  syntax {
       |    keyword ${renderSyntax(config.syntax.keyword, 4)}
       |    string ${renderSyntax(config.syntax.string, 4)}
       |    comment ${renderSyntax(config.syntax.comment, 4)}
       |    number ${renderSyntax(config.syntax.number, 4)}
       |    operator ${renderSyntax(config.syntax.operator, 4)}
       |    identifier ${renderSyntax(config.syntax.identifier, 4)}
       |    typ ${optional(ThemeFieldSchema.typeField)}
       |    delimiter ${optional(ThemeFieldSchema.delimiterField)}
       |    whitespace ${optional(ThemeFieldSchema.whitespaceField)}
       |    error ${optional(ThemeFieldSchema.errorField)}
       |    normal ${optional(ThemeFieldSchema.normalField)}
       |  }
       |}
       |""".stripMargin

  private def tokenConfig(color: ThemeColor): UiTokenConfig =
    UiTokenConfig(
      foreground = hex(color.foreground),
      background = hex(color.background),
      alpha = Option.when(color.alpha != 1.0)(color.alpha),
      style = StyleConfig(color.style.isBold, color.style.isItalic, color.style.isUnderlined)
    )

  private def syntaxConfig(theme: Theme, element: SyntaxElement): SyntaxElementConfig =
    val color = theme.colorFor(element)
    SyntaxElementConfig(
      foreground = hex(color.foreground),
      background = Some(hex(color.background)),
      style = StyleConfig(color.style.isBold, color.style.isItalic, color.style.isUnderlined)
    )

  private def renderToken(config: UiTokenConfig, indent: Int): String =
    val pad       = " " * indent
    val alphaLine = config.alpha.map(value => s"\n$pad  alpha = $value").getOrElse("")
    s"""{
       |$pad  foreground = "${config.foreground}"
       |$pad  background = "${config.background}"$alphaLine
       |$pad  style ${renderStyle(config.style, indent + 2)}
       |$pad}""".stripMargin

  private def renderSyntax(config: SyntaxElementConfig, indent: Int): String =
    val pad            = " " * indent
    val backgroundLine = config.background.map(value => s"""\n$pad  background = "$value"""").getOrElse("")
    s"""{
       |$pad  foreground = "${config.foreground}"$backgroundLine
       |$pad  style ${renderStyle(config.style, indent + 2)}
       |$pad}""".stripMargin

  private def renderStyle(style: StyleConfig, indent: Int): String =
    val pad = " " * indent
    s"""{
       |$pad  bold = ${style.bold}
       |$pad  italic = ${style.italic}
       |$pad  underline = ${style.underline}
       |$pad}""".stripMargin

  private def hex(color: Color): String =
    ColorFormat.toHex(color, withAlpha = false)

  private def escape(value: String): String =
    value.replace("\\", "\\\\").replace("\"", "\\\"")
