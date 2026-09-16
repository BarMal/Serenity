package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun}
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.TextLayoutSnapshot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Rich-text buffers must lay out at per-line heights driven by their own run sizes, scaled by the prose zoom -- so a
  * heading row is taller than a body row and both grow with the Prose Font Size setting (#1542 prose scale).
  */
class ProseFontScaleSnapshotSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def headingAndBody: RichTextDocument =
    RichTextDocument(
      List(
        RichTextParagraph(List(RichTextRun("Title")), role = ParagraphRole.Heading(1)),
        RichTextParagraph.plain("body")
      )
    )

  private def snapshotFor(baseFontSize: Float, proseScale: Float): TextLayoutSnapshot =
    val document = headingAndBody
    val buffer = Buffer
      .fromString(BufferId(1), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 40, visibleLines = 4)
      )
    val font =
      FontLoader.loadTextFont(FontConfig(textFontFamily = "SansSerif", textFontSize = baseFontSize)).unsafeRunSync()
    TextLayoutSnapshot.fromBuffer(buffer, panelWidthPx = 800, font, proseScale = proseScale)

  "A rich-text snapshot" should "use the measured layout" in {
    snapshotFor(12.0f, 1.0f).usesMeasuredLayout shouldBe true
  }

  it should "give the heading line a taller row than the body line" in {
    val lines   = snapshotFor(12.0f, 1.0f).visualLines
    val heading = lines.find(_.bufferLine == 0).getOrElse(fail("expected heading line"))
    val body    = lines.find(_.bufferLine == 1).getOrElse(fail("expected body line"))
    heading.heightPx should be > body.heightPx
  }

  it should "scale the heading row up with the prose zoom" in {
    val headingAtOne = snapshotFor(12.0f, 1.0f).visualLines.find(_.bufferLine == 0).get
    val headingAtTwo = snapshotFor(12.0f, 2.0f).visualLines.find(_.bufferLine == 0).get
    headingAtTwo.heightPx should be > headingAtOne.heightPx
  }

  it should "grow the body row with the base prose font size (the zoom of un-sized runs)" in {
    val bodyAtTwelve     = snapshotFor(12.0f, 1.0f).visualLines.find(_.bufferLine == 1).get
    val bodyAtTwentyFour = snapshotFor(24.0f, 1.0f).visualLines.find(_.bufferLine == 1).get
    bodyAtTwentyFour.heightPx should be > bodyAtTwelve.heightPx
  }

  it should "widen a heading line's caret advances relative to body-sized text" in {
    val lines   = snapshotFor(12.0f, 2.0f).visualLines
    val heading = lines.find(_.bufferLine == 0).get
    // "Title" (5 chars) at a scaled heading size is wider than 5 body chars would be.
    val bodyWidthPerChar = lines.find(_.bufferLine == 1).map(l => l.widthPx / l.text.length.max(1)).getOrElse(0.0f)
    (heading.widthPx / heading.text.length) should be > bodyWidthPerChar
  }
