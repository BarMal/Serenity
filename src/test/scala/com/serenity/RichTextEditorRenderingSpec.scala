package com.serenity

import java.awt.{Color, Font}

import com.serenity.config.AppConfig
import com.serenity.richtext.*
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, CursorPosition}
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.{RichTextStyling, TextStyle, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RichTextEditorRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont     = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val monoMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(80, 24)

  private def richDocument: RichTextDocument =
    RichTextDocument(
      List(
        com.serenity.richtext.RichTextParagraph(
          List(
            com.serenity.richtext.RichTextRun("plain "),
            com.serenity.richtext.RichTextRun(
              "bold",
              com.serenity.richtext.RichTextStyle(marks = Set(InlineMark.Bold))
            ),
            com.serenity.richtext.RichTextRun(" and "),
            com.serenity.richtext.RichTextRun(
              "underlined",
              com.serenity.richtext.RichTextStyle(marks = Set(InlineMark.Underline))
            )
          )
        )
      )
    )

  private def buildState(buffer: Buffer): com.serenity.state.models.AppState =
    val paneId = com.serenity.state.models.PaneId(0)
    com.serenity.state.models.AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = Layout(
        editorPanes = Map(paneId -> com.serenity.state.models.EditorPane.withBuffer(paneId, buffer.id)),
        activeEditorPaneId = Some(paneId)
      ),
      theme = Theme.light,
      config = AppConfig.default.withLineNumbers(false).withGutter(false)
    )

  "RichTextStyling" should "slice rich text runs for a visual line range" in {
    val styled = RichTextStyling.styledLine(
      document = richDocument,
      bufferLine = 0,
      startColumn = 6,
      endColumn = 14,
      theme = Theme.light
    )

    styled.map(_.content) shouldBe List("bold", " and")
    styled.head.style shouldBe TextStyle.bold
    styled(1).style shouldBe TextStyle.normal
  }

  it should "apply run foreground colors from hex metadata" in {
    val document = RichTextDocument(
      List(
        com.serenity.richtext.RichTextParagraph(
          List(
            com.serenity.richtext.RichTextRun(
              "colored",
              com.serenity.richtext.RichTextStyle(color = Some("#336699"))
            )
          )
        )
      )
    )

    val styled = RichTextStyling.styledLine(document, 0, 0, 7, Theme.light)

    styled.map(_.foregroundColor) shouldBe List(Color(0x33, 0x66, 0x99))
  }

  it should "carry run font family and size metadata into styled text" in {
    val document = RichTextDocument(
      List(
        com.serenity.richtext.RichTextParagraph(
          List(
            com.serenity.richtext.RichTextRun(
              "serif",
              com.serenity.richtext.RichTextStyle(fontFamily = Some(Font.SERIF), fontSize = Some(18.0f))
            )
          )
        )
      )
    )

    val styled = RichTextStyling.styledLine(document, 0, 0, 5, Theme.light)

    styled.map(_.style.fontFamily) shouldBe List(Some(Font.SERIF))
    styled.map(_.style.fontSize) shouldBe List(Some(18.0f))
  }

  it should "apply default heading styles from paragraph roles" in {
    val document = RichTextDocument(
      List(
        com.serenity.richtext.RichTextParagraph.plain(
          "Chapter One",
          role = ParagraphRole.Heading(1)
        )
      )
    )

    val styled = RichTextStyling.styledLine(document, 0, 0, 11, Theme.light)

    styled.map(_.style.isBold) shouldBe List(true)
    styled.map(_.style.fontSize) shouldBe List(Some(22.0f))
  }

  it should "let explicit run font size override heading defaults" in {
    val document = RichTextDocument(
      List(
        com.serenity.richtext.RichTextParagraph(
          List(
            com.serenity.richtext.RichTextRun(
              "Scene",
              com.serenity.richtext.RichTextStyle(fontSize = Some(15.0f))
            )
          ),
          role = ParagraphRole.Heading(2)
        )
      )
    )

    val styled = RichTextStyling.styledLine(document, 0, 0, 5, Theme.light)

    styled.map(_.style.isBold) shouldBe List(true)
    styled.map(_.style.fontSize) shouldBe List(Some(15.0f))
  }

  "Renderer" should "apply rich text marks to editor text" in {
    val buffer = Buffer
      .fromString(BufferId(1), richDocument.plainText)
      .copy(richTextDocument = Some(richDocument))
    val state   = buildState(buffer)
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, textFont, monoMetrics, None)

    surface.styleCalls should contain(surface.StyleCall("enable", TextStyle.bold))
    surface.styleCalls should contain(surface.StyleCall("disable", TextStyle.bold))
    surface.styleCalls should contain(surface.StyleCall("enable", TextStyle.underlined))
    surface.styleCalls should contain(surface.StyleCall("disable", TextStyle.underlined))
  }

  it should "render rich text marks when dirty buffer text still matches rich metadata" in {
    val buffer = Buffer
      .fromString(BufferId(1), richDocument.plainText)
      .copy(richTextDocument = Some(richDocument), isDirty = true)
    val state   = buildState(buffer)
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, textFont, monoMetrics, None)

    surface.styleCalls should contain(surface.StyleCall("enable", TextStyle.bold))
    surface.styleCalls should contain(surface.StyleCall("enable", TextStyle.underlined))
  }

  it should "render rich text marks after the cursor leaves the formatted word" in {
    val document = RichTextDocument
      .oneParagraph("alpha beta")
      .applyMark(
        RichTextRange(RichTextPosition(0, 6), RichTextPosition(0, 10)),
        InlineMark.Italic
      )
    val buffer = Buffer
      .fromString(BufferId(1), document.plainText)
      .copy(richTextDocument = Some(document), cursors = List(CursorPosition(0, 0)), selection = None)
    val state   = buildState(buffer)
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, textFont, monoMetrics, None)

    surface.styleCalls should contain(surface.StyleCall("enable", TextStyle.italic))
  }

  it should "ignore rich text marks when dirty buffer text no longer matches rich metadata" in {
    val buffer = Buffer
      .fromString(BufferId(1), "edited text")
      .copy(richTextDocument = Some(richDocument), isDirty = true)
    val state   = buildState(buffer)
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, textFont, monoMetrics, None)

    surface.styleCalls should not contain surface.StyleCall("enable", TextStyle.bold)
    surface.styleCalls should not contain surface.StyleCall("enable", TextStyle.underlined)
  }

  it should "apply rich text font metadata to editor text" in {
    val document = RichTextDocument(
      List(
        com.serenity.richtext.RichTextParagraph(
          List(
            com.serenity.richtext.RichTextRun("plain "),
            com.serenity.richtext.RichTextRun(
              "serif",
              com.serenity.richtext.RichTextStyle(fontFamily = Some(Font.SERIF), fontSize = Some(18.0f))
            )
          )
        )
      )
    )
    val buffer = Buffer
      .fromString(BufferId(1), document.plainText)
      .copy(richTextDocument = Some(document))
    val state   = buildState(buffer)
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, textFont, monoMetrics, None)

    surface.styleCalls should contain(
      surface.StyleCall("enable", TextStyle(fontFamily = Some(Font.SERIF), fontSize = Some(18.0f)))
    )
  }

  it should "render rich text heading paragraph roles" in {
    val document = RichTextDocument(
      List(
        com.serenity.richtext.RichTextParagraph.plain(
          "Chapter One",
          role = ParagraphRole.Heading(1)
        )
      )
    )
    val buffer = Buffer
      .fromString(BufferId(1), document.plainText)
      .copy(richTextDocument = Some(document))
    val state   = buildState(buffer)
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, textFont, monoMetrics, None)

    surface.styleCalls should contain(
      surface.StyleCall("enable", TextStyle(isBold = true, fontSize = Some(22.0f)))
    )
  }

  it should "render rich text paragraph alignment through the editor layout" in {
    val document = RichTextDocument(
      List(
        com.serenity.richtext.RichTextParagraph.plain(
          "Centered",
          alignment = ParagraphAlignment.Center
        )
      )
    )
    val buffer = Buffer
      .fromString(BufferId(1), document.plainText)
      .copy(richTextDocument = Some(document))
    val state   = buildState(buffer)
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, textFont, monoMetrics, None)

    val drawCall = surface.drawRunPxCalls.find(_.s == "Centered").getOrElse(fail("expected centered rich text draw"))
    drawCall.xPx should be > 0.0f
  }
