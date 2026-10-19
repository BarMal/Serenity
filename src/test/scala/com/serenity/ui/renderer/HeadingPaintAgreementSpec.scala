package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, RichTextState, Viewport}
import com.serenity.ui.layout.{FontLineExtents, TextLayoutSnapshot}
import com.serenity.ui.theme.{HeadingSettings, HeadingStyle, TextStyle, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What is painted for a heading has to be what was measured for it, or the glyphs overrun or float inside their row.
  */
class HeadingPaintAgreementSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font = Font(Font.SERIF, Font.PLAIN, 14)
  private val frc  = TextLayoutSnapshot.defaultFontRenderContext()

  private val document = RichTextDocument(
    List(
      RichTextParagraph.plain("Title", role = ParagraphRole.Heading(1)),
      RichTextParagraph.plain("Body")
    )
  )

  private val buffer =
    Buffer
      .fromString(BufferId(1), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 10)
      )

  private def settingsWithH1(scale: Float): HeadingSettings =
    HeadingSettings(styles = HeadingStyle.defaults.updated(0, HeadingStyle.default(1).copy(sizeScale = scale)))

  private def snapshotWith(headings: HeadingSettings): TextLayoutSnapshot =
    TextLayoutSnapshot.fromBuffer(buffer, 600, font, headings = headings)

  private def paintedTitle(snapshot: TextLayoutSnapshot): TextStyle =
    val row = snapshot.visualLines.headOption.getOrElse(fail("expected a title row"))
    RendererLineSegments
      .richTextStyledSegments(row, Theme.dark, snapshot)
      .flatMap(_.headOption)
      .map(_.style)
      .getOrElse(fail("expected painted segments"))

  "A snapshot" should "carry the heading settings it was measured with" in {
    val custom = settingsWithH1(3.0f)

    snapshotWith(custom).headings shouldBe custom
    snapshotWith(HeadingSettings.default).headings shouldBe HeadingSettings.default
  }

  "A painted heading" should "use the size its row was measured at, for a non-default H1" in {
    val snapshot = snapshotWith(settingsWithH1(3.0f))
    val painted  = paintedTitle(snapshot)
    val row      = snapshot.visualLines.head

    painted.fontSize shouldBe Some(3.0f * 12.0f * snapshot.proseScale)
    val paintedFont = TextStyle.styledFont(font, painted)
    row.heightPx shouldBe FontLineExtents.of(paintedFont, frc).heightPx
  }

  it should "still use today's 22pt at the default settings" in {
    val snapshot = snapshotWith(HeadingSettings.default)

    paintedTitle(snapshot).fontSize shouldBe Some(22.0f * snapshot.proseScale)
  }

  it should "follow the bold and italic of its heading style" in {
    val custom = HeadingSettings(styles =
      HeadingStyle.defaults.updated(0, HeadingStyle.default(1).copy(bold = false, italic = true))
    )
    val painted = paintedTitle(snapshotWith(custom))

    painted.isBold shouldBe false
    painted.isItalic shouldBe true
  }
