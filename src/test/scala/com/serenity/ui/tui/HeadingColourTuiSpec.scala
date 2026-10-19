package com.serenity.ui.tui

import java.awt.{Color, Font}
import java.io.StringWriter

import com.serenity.TestWorkspaceTrees
import com.serenity.config.{AppConfig, HeadingsConfig}
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{AppState, Buffer, BufferId, EditorPane, Focus, PaneId, RichTextState}
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, RendererEntryPoints}
import com.serenity.ui.theme.{HeadingColour, SyntaxElement, TextStyle, Theme, ThemeColor}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The terminal paints headings in the theme's heading colour, and in a level's own colour when one is configured. */
class HeadingColourTuiSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val viewport = ViewportSize(60, 16)
  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private val teal   = RenderColor.fromRgba(0x11, 0xaa, 0x99)
  private val orange = RenderColor.fromRgba(0xee, 0x88, 0x22)
  private val purple = RenderColor.fromRgba(0x88, 0x44, 0xcc)

  private def themed(element: SyntaxElement, colour: RenderColor): Theme => Theme =
    theme =>
      theme.copy(syntaxColors =
        theme.syntaxColors.updated(element, ThemeColor(colour, theme.background, TextStyle.bold))
      )

  private val theme =
    themed(SyntaxElement.Heading, teal).andThen(themed(SyntaxElement.HeadingMarker, purple))(Theme.dark)

  private def render(state: AppState, caches: RenderCaches = RenderCaches.create()): TerminalEmulator =
    val writer  = new StringWriter()
    val surface = new TerminalRenderSurface(viewport.width, viewport.height, writer, CellMetrics.cellUnit)
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      FontSpec.fromAwt(font),
      FontSpec.fromAwt(font),
      CellMetrics.cellUnit,
      None,
      caches
    )
    surface.flush()
    TerminalEmulator.blank(viewport.width, viewport.height).consume(writer.toString)

  private def stateOf(buffer: Buffer, config: AppConfig): AppState =
    val initial = AppState.initial(AppConfig.conventional)
    initial.copy(persisted =
      initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId),
        theme = theme,
        config = config
      )
    )

  private val document = RichTextDocument(
    List(
      RichTextParagraph.plain("Headline", role = ParagraphRole.Heading(1)),
      RichTextParagraph.plain("Subhead", role = ParagraphRole.Heading(2)),
      RichTextParagraph.plain("Plain body")
    )
  )

  private def proseState(config: AppConfig): AppState =
    stateOf(
      Buffer.fromString(bufferId, document.plainText).copy(richText = RichTextState(richTextDocument = Some(document))),
      config
    )

  private def foreground(screen: TerminalEmulator, text: String): Color =
    val (col, row) = screen.find(text).getOrElse(fail(s"no '$text' on screen:\n${screen.render}"))
    screen.cellAt(col, row).fg

  private def rgb(color: Color): Int = color.getRGB & 0xffffff

  private def withHeadings(headings: HeadingsConfig): AppConfig = AppConfig.conventional.withHeadingsConfig(headings)

  private def proseColours(config: AppConfig): (Color, Color, Color) =
    val screen = render(proseState(config))
    (foreground(screen, "Headline"), foreground(screen, "Subhead"), foreground(screen, "Plain body"))

  "A rich-text heading in the terminal" should "be painted in the text colour by default, as body text is" in {
    val (headline, subhead, body) = proseColours(AppConfig.conventional)

    rgb(headline) shouldBe rgb(theme.foreground.toAwt)
    rgb(subhead) shouldBe rgb(theme.foreground.toAwt)
    rgb(body) shouldBe rgb(theme.foreground.toAwt)
  }

  it should "follow the global heading colour" in {
    val (headline, subhead, body) = proseColours(withHeadings(HeadingsConfig(colour = Some(HeadingColour.Heading))))
    rgb(headline) shouldBe rgb(teal.toAwt)
    rgb(subhead) shouldBe rgb(teal.toAwt)
    rgb(body) shouldBe rgb(theme.foreground.toAwt)

    rgb(proseColours(withHeadings(HeadingsConfig(colour = Some(HeadingColour.Text))))._1) shouldBe
      rgb(theme.foreground.toAwt)
  }

  it should "let a level override the global colour" in {
    val config = withHeadings(
      HeadingsConfig(colour = Some(HeadingColour.Heading))
        .withLevel(2)(_.copy(colour = Some(HeadingColour.Fixed(orange))))
    )
    val (headline, subhead, _) = proseColours(config)

    rgb(headline) shouldBe rgb(teal.toAwt)
    rgb(subhead) shouldBe rgb(orange.toAwt)
    rgb(
      proseColours(withHeadings(HeadingsConfig().withLevel(1)(_.copy(colour = Some(HeadingColour.Heading)))))._1
    ) shouldBe
      rgb(teal.toAwt)
  }

  it should "show a colour changed between frames that share their render caches" in {
    val caches = RenderCaches.create()
    val orangeConfig =
      withHeadings(HeadingsConfig().withLevel(1)(_.copy(colour = Some(HeadingColour.Fixed(orange)))))
    val plain = rgb(theme.foreground.toAwt)

    rgb(foreground(render(proseState(AppConfig.conventional), caches), "Headline")) shouldBe plain
    rgb(foreground(render(proseState(orangeConfig), caches), "Headline")) shouldBe rgb(orange.toAwt)
    rgb(foreground(render(proseState(AppConfig.conventional), caches), "Headline")) shouldBe plain
  }

  private def markdownState(config: AppConfig, text: String = "# Chapter\n### Third\nplain text"): AppState =
    val base   = Buffer.fromString(bufferId, text)
    val buffer = base.copy(document = base.document.copy(language = Some(LanguageId.Markdown)))
    stateOf(buffer, config.withSyntaxHighlighting(true))

  private def markdownColours(config: AppConfig): (Color, Color, Color, Color) =
    val screen = render(markdownState(config))
    (
      foreground(screen, "Chapter"),
      foreground(screen, "Third"),
      foreground(screen, "#"),
      foreground(screen, "plain text")
    )

  "A Markdown heading in the terminal" should "be painted in the theme's heading and heading-marker colours by default" in {
    val (chapter, third, marker, body) = markdownColours(AppConfig.conventional)

    rgb(chapter) shouldBe rgb(teal.toAwt)
    rgb(third) shouldBe rgb(teal.toAwt)
    rgb(marker) shouldBe rgb(purple.toAwt)
    rgb(body) shouldBe rgb(theme.foreground.toAwt)
  }

  it should "follow the global text colour, keeping its marker colour" in {
    val (chapter, third, marker, _) = markdownColours(withHeadings(HeadingsConfig(colour = Some(HeadingColour.Text))))

    rgb(chapter) shouldBe rgb(theme.foreground.toAwt)
    rgb(third) shouldBe rgb(theme.foreground.toAwt)
    rgb(marker) shouldBe rgb(purple.toAwt)
  }

  it should "follow the global heading colour, and a level's own over it" in {
    val config = withHeadings(
      HeadingsConfig(colour = Some(HeadingColour.Text))
        .withLevel(1)(_.copy(colour = Some(HeadingColour.Heading)))
        .withLevel(3)(_.copy(colour = Some(HeadingColour.Fixed(orange))))
    )
    val (chapter, third, _, _) = markdownColours(config)

    rgb(chapter) shouldBe rgb(teal.toAwt)
    rgb(third) shouldBe rgb(orange.toAwt)
    rgb(markdownColours(withHeadings(HeadingsConfig(colour = Some(HeadingColour.Heading))))._1) shouldBe rgb(teal.toAwt)
  }
