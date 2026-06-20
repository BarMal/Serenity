package com.serenity

import java.awt.{Color, Font}

import com.serenity.config.AppConfig
import com.serenity.richtext.{InlineMark, RichTextDocument}
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
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

  it should "ignore rich text marks when buffer text is dirty" in {
    val buffer = Buffer
      .fromString(BufferId(1), richDocument.plainText)
      .copy(richTextDocument = Some(richDocument), isDirty = true)
    val state   = buildState(buffer)
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    Renderer.render(state, cursorVisible = false, surface, viewportSize, monoFont, textFont, monoMetrics, None)

    surface.styleCalls should not contain surface.StyleCall("enable", TextStyle.bold)
    surface.styleCalls should not contain surface.StyleCall("enable", TextStyle.underlined)
  }
