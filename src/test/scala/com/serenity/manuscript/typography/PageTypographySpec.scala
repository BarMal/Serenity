package com.serenity.manuscript.typography

import com.serenity.manuscript.PaperSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PageTypographySpec extends AnyFlatSpec with Matchers:

  "the Standard Manuscript Format Courier preset" should "be 12 pt Courier Prime, double-spaced, on 1-inch margins" in {
    val smf = PageTypography.StandardManuscriptCourier

    smf.body shouldBe FontSpec(FontFamily.CourierPrime, FaceStyle.Regular, 12f)
    smf.lineSpacing shouldBe LineSpacing.Multiple(2.0)
    smf.margins shouldBe PageMargins.uniform(72f)
  }

  it should "indent the first line half an inch, with no paragraph gap, ragged right" in {
    val smf = PageTypography.StandardManuscriptCourier

    (smf.firstLineIndent, smf.paragraphGap, smf.alignment) shouldBe (36f, 0f, LineAlignment.Ragged)
  }

  it should "be US Letter, 612 by 792 points" in {
    val smf = PageTypography.StandardManuscriptCourier

    (smf.pageWidth, smf.pageHeight) shouldBe (612f, 792f)
    (smf.textWidth, smf.textHeight) shouldBe (468f, 648f)
  }

  it should "have an A4 variant that changes only the paper" in {
    val a4 = PageTypography.StandardManuscriptCourierA4

    a4 shouldBe PageTypography.StandardManuscriptCourier.copy(paper = PaperSize.A4)
    a4.pageWidth shouldBe 595.3f +- 0.001f
    a4.pageHeight shouldBe 841.9f +- 0.001f
  }

  it should "carry the manuscript's running head, scene break and chapter drop" in {
    val smf = PageTypography.StandardManuscriptCourier

    smf.runningHead shouldBe "<$surname> / <$keyword> / <$p>"
    smf.sceneBreak shouldBe "#"
    smf.chapterDrop shouldBe 1.0 / 3.0
  }

  it should "keep two widow and two orphan lines" in {
    (PageTypography.StandardManuscriptCourier.widows, PageTypography.StandardManuscriptCourier.orphans) shouldBe (2, 2)
  }

  "TypographyPreset" should "be found by key, for either paper" in {
    TypographyPreset.fromKey(" SMF-Courier ") shouldBe Some(TypographyPreset.StandardManuscriptCourier)
    TypographyPreset.fromKey("smf-serif") shouldBe None
    TypographyPreset.StandardManuscriptCourier.typography(PaperSize.A4) shouldBe
      PageTypography.StandardManuscriptCourierA4
  }

  "FontFamily" should "name the four bundled Courier Prime faces" in {
    FontFamily.CourierPrime.resource(FaceStyle.Regular) shouldBe "/fonts/CourierPrime-Regular.ttf"
    FontFamily.CourierPrime.resource(FaceStyle.Italic) shouldBe "/fonts/CourierPrime-Italic.ttf"
    FontFamily.CourierPrime.resource(FaceStyle.Bold) shouldBe "/fonts/CourierPrime-Bold.ttf"
    FontFamily.CourierPrime.resource(FaceStyle.BoldItalic) shouldBe "/fonts/CourierPrime-BoldItalic.ttf"
  }
