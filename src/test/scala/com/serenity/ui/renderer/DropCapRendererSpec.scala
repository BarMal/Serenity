package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.MockRenderSurface
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.state.models.{AppState, TextCaretStop, TextVisualLine}
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.TextLayoutSnapshot
import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DropCapRendererSpec extends AnyFlatSpec with Matchers:

  "DropCapRenderer.renderGlyphPx" should "draw one pixel run spanning the glyph's full multi-line height" in {
    val surface = MockRenderSurface(80, 40)
    val style   = TextStyle(isBold = true, fontSize = Some(36.0f))

    DropCapRenderer.renderGlyphPx(
      surface,
      xOriginPx = 10.0f,
      yTopPx = 5,
      glyphWidthPx = 24.0f,
      glyphHeightPx = 48,
      glyphAscentPx = 36,
      glyphText = "C",
      glyphStyle = style,
      foreground = RenderColor.fromRgba(255, 0, 0),
      background = RenderColor.Black
    )

    surface.drawRunPxCalls should have size 1
    val call = surface.drawRunPxCalls.head
    call.s shouldBe "C"
    call.xPx shouldBe 10.0f
    call.yPx shouldBe 5
    call.bgWidthPx shouldBe 24.0f
    call.lineHeightPx shouldBe 48
    call.ascentPx shouldBe 36
    call.foreground shouldBe RenderColor.fromRgba(255, 0, 0)
  }

  it should "draw nothing for an empty glyph" in {
    val surface = MockRenderSurface(80, 40)

    DropCapRenderer.renderGlyphPx(
      surface,
      xOriginPx = 10.0f,
      yTopPx = 5,
      glyphWidthPx = 24.0f,
      glyphHeightPx = 48,
      glyphAscentPx = 36,
      glyphText = "",
      glyphStyle = TextStyle.normal,
      foreground = RenderColor.fromRgba(255, 0, 0),
      background = RenderColor.Black
    )

    surface.drawRunPxCalls shouldBe Nil
  }

  "DropCapRenderer.renderGlyphCell" should "write the character as a single bold cell" in {
    val surface = MockRenderSurface(80, 40)

    DropCapRenderer.renderGlyphCell(
      surface,
      x = 2,
      y = 1,
      glyphText = "C",
      accentForeground = RenderColor.fromRgba(255, 0, 0),
      background = RenderColor.Black
    )

    surface.getChar(2, 1) shouldBe 'C'
    surface.styleCalls.map(_.style) should contain(TextStyle(isBold = true))
  }

  private def dropCapSnapshot(text: String, lines: Int = 2): (TextLayoutSnapshot, TextVisualLine) =
    val document = RichTextDocument(List(RichTextParagraph.plain(text, role = ParagraphRole.dropCap(lines))))
    val visualLine = TextVisualLine(
      bufferLine = 0,
      startColumn = 0,
      endColumn = text.length,
      text = text,
      widthPx = text.length.toFloat * 8.0f,
      caretStops = (0 to text.length).map(i => TextCaretStop(i, i * 8.0f)).toVector
    )
    val snapshot = TextLayoutSnapshot(
      visualLines = Vector(visualLine),
      panelWidthPx = 400,
      lineHeightPx = 16,
      ascentPx = 12,
      richTextDocument = Some(document)
    )
    (snapshot, visualLine)

  "DropCapRenderer.adjustHomeLineDraw" should
    "paint the glyph and split it out of the ordinary text on the measured (GUI) path" in {
      val (snapshot, visualLine) = dropCapSnapshot("Hello world")
      val state                  = AppState.empty
      val surface                = MockRenderSurface(200, 40)
      val font                   = Font(Font.SANS_SERIF, Font.PLAIN, 12)
      val frc                    = TextLayoutSnapshot.defaultFontRenderContext()

      val (drawLine, drawSegments) = DropCapRenderer.adjustHomeLineDraw(
        snapshot = snapshot,
        state = state,
        visualLine = visualLine,
        styledSegments = None,
        measured = true,
        surface = surface,
        baseFont = font,
        frc = Some(frc),
        theme = state.persisted.theme,
        xOriginPx = 0.0f,
        lineTopPx = 0,
        normalLineHeightPx = 16
      )

      surface.drawRunPxCalls should have size 1
      surface.drawRunPxCalls.head.s shouldBe "H"
      drawLine.text shouldBe "ello world"
      drawLine.startColumn shouldBe 1
      drawSegments shouldBe None
    }

  it should "drop the same leading character from a supplied styled-segments list, keeping its colour" in {
    val (snapshot, visualLine) = dropCapSnapshot("Hello world")
    val state                  = AppState.empty
    val surface                = MockRenderSurface(200, 40)
    val font                   = Font(Font.SANS_SERIF, Font.PLAIN, 12)
    val frc                    = TextLayoutSnapshot.defaultFontRenderContext()
    val segments = Some(
      List(
        com.serenity.ui.theme.StyledText("Hello", TextStyle.normal, RenderColor.fromRgba(255, 0, 0), RenderColor.Black),
        com.serenity.ui.theme.StyledText(" world", TextStyle.normal, RenderColor.fromRgba(0, 0, 255), RenderColor.Black)
      )
    )

    val (_, drawSegments) = DropCapRenderer.adjustHomeLineDraw(
      snapshot = snapshot,
      state = state,
      visualLine = visualLine,
      styledSegments = segments,
      measured = true,
      surface = surface,
      baseFont = font,
      frc = Some(frc),
      theme = state.persisted.theme,
      xOriginPx = 0.0f,
      lineTopPx = 0,
      normalLineHeightPx = 16
    )

    drawSegments.map(_.map(_.content)) shouldBe Some(List("ello", " world"))
    drawSegments.map(_.map(_.foregroundColor)) shouldBe Some(
      List(RenderColor.fromRgba(255, 0, 0), RenderColor.fromRgba(0, 0, 255))
    )
  }

  it should "leave the visual line and segments unchanged on the cell/TUI path" in {
    val (snapshot, visualLine) = dropCapSnapshot("Hello world")
    val state                  = AppState.empty
    val surface                = MockRenderSurface(200, 40)
    val font                   = Font(Font.SANS_SERIF, Font.PLAIN, 12)

    val (drawLine, drawSegments) = DropCapRenderer.adjustHomeLineDraw(
      snapshot = snapshot,
      state = state,
      visualLine = visualLine,
      styledSegments = None,
      measured = false,
      surface = surface,
      baseFont = font,
      frc = None,
      theme = state.persisted.theme,
      xOriginPx = 0.0f,
      lineTopPx = 0,
      normalLineHeightPx = 16
    )

    drawLine shouldBe visualLine
    drawSegments shouldBe None
    surface.drawRunPxCalls shouldBe Nil
  }

  it should "leave a non-drop-cap paragraph's line untouched, even on the measured path" in {
    val document                  = RichTextDocument(List(RichTextParagraph.plain("Hello world")))
    val (dropCapSnap, visualLine) = dropCapSnapshot("Hello world")
    val snapshot                  = dropCapSnap.copy(richTextDocument = Some(document))
    val state                     = AppState.empty
    val surface                   = MockRenderSurface(200, 40)
    val font                      = Font(Font.SANS_SERIF, Font.PLAIN, 12)
    val frc                       = TextLayoutSnapshot.defaultFontRenderContext()

    val (drawLine, _) = DropCapRenderer.adjustHomeLineDraw(
      snapshot = snapshot,
      state = state,
      visualLine = visualLine,
      styledSegments = None,
      measured = true,
      surface = surface,
      baseFont = font,
      frc = Some(frc),
      theme = state.persisted.theme,
      xOriginPx = 0.0f,
      lineTopPx = 0,
      normalLineHeightPx = 16
    )

    drawLine shouldBe visualLine
    surface.drawRunPxCalls shouldBe Nil
  }

  "DropCapRenderer.paintCellHomeLineIfNeeded" should "overlay the first cell in bold on a drop cap home line" in {
    val (snapshot, visualLine) = dropCapSnapshot("Hello world")
    val state                  = AppState.empty
    val surface                = MockRenderSurface(200, 40)

    DropCapRenderer.paintCellHomeLineIfNeeded(snapshot, state, visualLine, surface, x = 2, y = 1)

    surface.getChar(2, 1) shouldBe 'H'
    surface.styleCalls.map(_.style) should contain(TextStyle(isBold = true))
  }

  it should "do nothing for a non-drop-cap paragraph" in {
    val document                  = RichTextDocument(List(RichTextParagraph.plain("Hello world")))
    val (dropCapSnap, visualLine) = dropCapSnapshot("Hello world")
    val snapshot                  = dropCapSnap.copy(richTextDocument = Some(document))
    val state                     = AppState.empty
    val surface                   = MockRenderSurface(200, 40)

    DropCapRenderer.paintCellHomeLineIfNeeded(snapshot, state, visualLine, surface, x = 2, y = 1)

    surface.putStringCalls shouldBe empty
  }

  it should "do nothing when the config toggle is off, even for a drop cap paragraph" in {
    val (snapshot, visualLine) = dropCapSnapshot("Hello world")
    val state                  = AppState.empty(com.serenity.config.AppConfig.default.withDropCapsEnabled(false))
    val surface                = MockRenderSurface(200, 40)

    DropCapRenderer.paintCellHomeLineIfNeeded(snapshot, state, visualLine, surface, x = 2, y = 1)

    surface.putStringCalls shouldBe empty
  }
