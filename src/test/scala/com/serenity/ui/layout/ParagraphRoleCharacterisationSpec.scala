package com.serenity.ui.layout

import java.awt.Font

import com.serenity.MockRenderSurface
import com.serenity.config.AppConfig
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextPosition, RichTextRange}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, Buffer, BufferId, RichTextState, Viewport}
import com.serenity.ui.renderer.DropCapRenderer
import com.serenity.ui.theme.RichTextStyling
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.serenity.ui.renderer.ShapedTextCache

/** Pins what a drop cap paragraph does to layout, paint and dirty tracking with the drop-caps toggle on and off. */
class ParagraphRoleCharacterisationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val frc  = TextLayoutSnapshot.defaultFontRenderContext()
  private val font = Font(Font.SANS_SERIF, Font.PLAIN, 13)
  private val prose =
    "The quick brown fox jumps over the lazy dog, again and again, until the row finally has to wrap somewhere. " * 3

  private def document(role: ParagraphRole): RichTextDocument =
    RichTextDocument(List(RichTextParagraph.plain(prose, role = role), RichTextParagraph.plain(prose)))

  private def buffer(role: ParagraphRole): Buffer =
    val doc = document(role)
    Buffer
      .fromString(BufferId(3), doc.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(doc)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 100)
      )

  private def snapshot(role: ParagraphRole, dropCapsEnabled: Boolean): TextLayoutSnapshot =
    TextLayoutSnapshot.fromBuffer(buffer(role), 240, font, frc, dropCapsEnabled = dropCapsEnabled)

  private val dropCap = ParagraphRole.dropCap(3)

  "glyphRowSpan" should "span the cap's rows while drop caps are enabled" in {
    DropCapLayout.glyphRowSpan(snapshot(dropCap, dropCapsEnabled = true)) shouldBe Map(0 -> (0 until 3))
  }

  it should "span nothing for a cap the toggle has degraded to body text" in {
    DropCapLayout.glyphRowSpan(snapshot(dropCap, dropCapsEnabled = false)) shouldBe empty
  }

  "closedOverGlyphs" should "close a later row of the glyph over the whole span while drop caps are enabled" in {
    DropCapLayout.closedOverGlyphs(Set(2), snapshot(dropCap, dropCapsEnabled = true)) shouldBe Set(0, 1, 2)
  }

  it should "leave a row alone when the toggle has degraded the cap" in {
    DropCapLayout.closedOverGlyphs(Set(2), snapshot(dropCap, dropCapsEnabled = false)) shouldBe Set(2)
  }

  "the left inset of a drop cap paragraph" should "indent exactly the glyph's rows while enabled" in {
    val offsets = snapshot(dropCap, dropCapsEnabled = true).visualLines.map(_.xOffsetPx)
    offsets.take(3).foreach(_ should be > 0.0f)
    offsets(3) shouldBe 0.0f
  }

  it should "indent nothing once the toggle is off" in {
    snapshot(dropCap, dropCapsEnabled = false).visualLines.map(_.xOffsetPx).distinct shouldBe Vector(0.0f)
  }

  "the line layout inputs" should "carry the cap's role and glyph width while enabled" in {
    val inputs = inputsFor(dropCapsEnabled = true)
    inputs.paragraphRole shouldBe dropCap
    inputs.dropCap.indentPx should be > 0.0f
  }

  it should "degrade to body text with no glyph width once the toggle is off" in {
    val inputs = inputsFor(dropCapsEnabled = false)
    inputs.paragraphRole shouldBe ParagraphRole.Body
    inputs.dropCap.indentPx shouldBe 0.0f
  }

  private def inputsFor(dropCapsEnabled: Boolean): LineLayout.LineLayoutInputs =
    LineLayout.lineLayoutInputs(
      font,
      frc,
      measuredLayout = true,
      RichTextContext(Some(document(dropCap)), 1.0f, dropCapsEnabled),
      lineIndex = 0,
      lineLength = prose.length,
      lineText = prose,
      shapedText = ShapedTextCache()
    )

  "dropCapSplitFontSpans" should "single the first character out at the paragraph's start" in {
    val (glyph, rest) = RichTextStyling.dropCapSplitFontSpans(document(dropCap), 0, 0, prose.length, 12.0f)
    glyph.map(_.text) shouldBe Some("T")
    rest.map(_.text).mkString shouldBe prose.drop(1)
  }

  "DirtyLineDiff" should "mark the rows from a paragraph whose role became a drop cap down" in {
    val before = DropCapSnapshots.snapshot(DropCapSnapshots.body(2), DropCapSnapshots.body(2))
    val capped = before.richTextDocument.map(_.setParagraphRole(startOf(0), dropCap))
    val after  = before.copy(richTextDocument = capped.map(identity))

    DirtyLineDiff.dirtyRows(Some(before), after) shouldBe Set(0, 1, 2, 3)
  }

  private def startOf(paragraph: Int): RichTextRange =
    RichTextRange(RichTextPosition(paragraph, 0), RichTextPosition(paragraph, 0))

  "DropCapRenderer.adjustHomeLineDraw" should "paint the glyph while drop caps are enabled" in {
    paintCalls(dropCapsEnabled = true) shouldBe 1
  }

  it should "paint nothing once the toggle is off" in {
    paintCalls(dropCapsEnabled = false) shouldBe 0
  }

  private def paintCalls(dropCapsEnabled: Boolean): Int =
    val laidOut = snapshot(dropCap, dropCapsEnabled)
    val state   = AppState.empty(AppConfig.default.withDropCapsEnabled(dropCapsEnabled))
    val surface = MockRenderSurface(400, 100)
    DropCapRenderer.adjustHomeLineDraw(
      snapshot = laidOut,
      visualLine = laidOut.visualLines(0),
      styledSegments = None,
      measured = true,
      surface = surface,
      baseFont = font,
      frc = Some(frc),
      theme = state.persisted.theme,
      glyphColour = state.persisted.theme.highlighted.foreground,
      xOriginPx = 0.0f,
      lineTopPx = 0
    )
    surface.drawRunPxCalls.size
