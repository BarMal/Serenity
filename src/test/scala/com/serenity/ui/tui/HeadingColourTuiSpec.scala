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
import com.serenity.ui.theme.{SyntaxElement, TextStyle, Theme, ThemeColor}
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

  "A rich-text heading in the terminal" should "be painted in the theme's heading colour, and body text is not" in {
    val screen = render(proseState(AppConfig.conventional))

    rgb(foreground(screen, "Headline")) shouldBe rgb(teal.toAwt)
    rgb(foreground(screen, "Subhead")) shouldBe rgb(teal.toAwt)
    rgb(foreground(screen, "Plain body")) shouldBe rgb(theme.foreground.toAwt)
  }

  it should "be painted in its level's configured colour" in {
    val config = AppConfig.conventional.withHeadingsConfig(
      HeadingsConfig().withLevel(2)(_.copy(colour = Some(orange)))
    )
    val screen = render(proseState(config))

    rgb(foreground(screen, "Headline")) shouldBe rgb(teal.toAwt)
    rgb(foreground(screen, "Subhead")) shouldBe rgb(orange.toAwt)
  }

  it should "show a colour changed between frames that share their render caches" in {
    val caches = RenderCaches.create()
    val red    = AppConfig.conventional.withHeadingsConfig(HeadingsConfig().withLevel(1)(_.copy(colour = Some(orange))))

    rgb(foreground(render(proseState(AppConfig.conventional), caches), "Headline")) shouldBe rgb(teal.toAwt)
    rgb(foreground(render(proseState(red), caches), "Headline")) shouldBe rgb(orange.toAwt)
    rgb(foreground(render(proseState(AppConfig.conventional), caches), "Headline")) shouldBe rgb(teal.toAwt)
  }

  "A Markdown heading in the terminal" should "be painted in the theme's heading and heading-marker colours" in {
    val base   = Buffer.fromString(bufferId, "# Chapter\nplain text")
    val buffer = base.copy(document = base.document.copy(language = Some(LanguageId.Markdown)))
    val screen = render(stateOf(buffer, AppConfig.conventional.withSyntaxHighlighting(true)))

    rgb(foreground(screen, "Chapter")) shouldBe rgb(teal.toAwt)
    rgb(foreground(screen, "#")) shouldBe rgb(purple.toAwt)
    rgb(foreground(screen, "plain text")) shouldBe rgb(theme.foreground.toAwt)
  }
