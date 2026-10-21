package com.serenity.ui.theme

import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph, RichTextRun, RichTextStyle}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeadingStyleSpec extends AnyFlatSpec with Matchers:

  private val todaysSizes = Map(1 -> 22.0f, 2 -> 18.0f, 3 -> 16.0f, 4 -> 14.0f, 5 -> 14.0f, 6 -> 14.0f)

  private def headingSpans(level: Int, scale: Float, headings: HeadingSettings = HeadingSettings.default) =
    val document = RichTextDocument(List(RichTextParagraph.plain("Title", role = ParagraphRole.Heading(level))))
    RichTextStyling.styledFontSpans(document, 0, 0, 5, scale, headings)

  "HeadingStyle.default" should "be bold, plain and unpositioned at every level" in
    (1 to 6).foreach { level =>
      val style = HeadingStyle.default(level)
      style.bold shouldBe true
      style.italic shouldBe false
      style.family shouldBe None
      style.colour shouldBe None
      style.align shouldBe None
      style.spaceBeforeEm shouldBe 0.0f
      style.spaceAfterEm shouldBe 0.0f
    }

  it should "scale by today's sizes over the 12pt baseline" in
    todaysSizes.foreach {
      case (level, size) =>
        HeadingStyle.default(level).sizeScale * RichTextStyling.ProseZoomBaselinePx shouldBe size
    }

  "RichTextStyling" should "style headings exactly as before at a 12pt body, levels 1 to 6" in
    todaysSizes.foreach {
      case (level, size) =>
        headingSpans(level, scale = 1.0f).map(_.style) shouldBe List(TextStyle(isBold = true, fontSize = Some(size)))
    }

  it should "style headings exactly as before at a 24pt body, levels 1 to 6" in
    todaysSizes.foreach {
      case (level, size) =>
        headingSpans(level, scale = 2.0f).map(_.style) shouldBe List(
          TextStyle(isBold = true, fontSize = Some(size * 2.0f))
        )
    }

  it should "size an out-of-range level like the smallest heading, as before" in
    List(0, 7, 99).foreach { level =>
      headingSpans(level, scale = 1.0f).map(_.style) shouldBe List(TextStyle(isBold = true, fontSize = Some(14.0f)))
    }

  it should "let an imported run's own size and family win over the heading style" in {
    val run      = RichTextRun("Title", RichTextStyle(fontFamily = Some("Serif"), fontSize = Some(30.0f)))
    val document = RichTextDocument(List(RichTextParagraph(List(run), role = ParagraphRole.Heading(1))))

    RichTextStyling.styledFontSpans(document, 0, 0, 5).map(_.style) shouldBe
      List(TextStyle(isBold = true, fontFamily = Some("Serif"), fontSize = Some(30.0f)))
  }

  it should "resolve sizes from the supplied heading settings" in {
    val bigger = HeadingSettings.default.copy(styles = Vector.fill(6)(HeadingStyle.default(1).copy(sizeScale = 3.0f)))

    headingSpans(2, scale = 1.0f, bigger).map(_.style.fontSize) shouldBe List(Some(36.0f))
  }

  it should "damp heading sizes above the threshold using the logical body size" in {
    val big = HeadingSettings.default.copy(logicalBodyPt = 48.0f)

    headingSpans(1, scale = 4.0f, big).map(_.style.fontSize.map(_ / 4.0f)) shouldBe
      List(Some(HeadingScale.effective(22.0f / 12.0f, 48.0f, 24.0f, dampingOn = true) * 12.0f))
  }
