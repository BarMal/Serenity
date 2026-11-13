package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The corpus has to keep covering what it was written to cover, so a later edit cannot drop a case unseen. */
class TextMeasureCorpusSpec extends AnyFlatSpec with Matchers:

  private lazy val entries = TextMeasureBaseline.entries()

  "Every pinned font" should "have the cell metrics, the layout choice and the certificate" in
    TextMeasureCorpus.fonts.foreach { font =>
      List("cell", "measured", "certificate").foreach(kind => entries.map(_.key) should contain(s"${font.label}.$kind"))
    }

  it should "have every corpus line measured" in {
    for
      font <- TextMeasureCorpus.fonts
      line <- TextMeasureCorpus.lines
    do entries.map(_.key).exists(_.startsWith(s"${font.label}.${line.id}.")) shouldBe true
  }

  "Every corpus line that is not structural only" should "have carets and, at each wrap width, rows, ends, widths and heights" in {
    val keys = entries.map(_.key).toSet
    for
      font <- TextMeasureCorpus.fonts
      line <- TextMeasureCorpus.lines.filterNot(TextMeasureCorpus.structuralOnly(font, _))
    do
      val prefix = s"${font.label}.${line.id}"
      keys should contain(s"$prefix.carets")
      List("rows", "ends", "widths", "heights").foreach(kind => keys should contain(s"$prefix.$kind@240"))
      List("rows", "ends", "widths", "heights").foreach(kind => keys should contain(s"$prefix.$kind@600"))
  }

  "The structural-only lines" should "have their caret stops counted and nothing else pinned" in {
    val keys = entries.map(_.key).toSet
    for
      font <- TextMeasureCorpus.fonts
      line <- TextMeasureCorpus.lines.filter(TextMeasureCorpus.structuralOnly(font, _))
    do
      keys should contain(s"${font.label}.${line.id}.stops")
      keys should not contain s"${font.label}.${line.id}.carets"
  }

  "The corpus" should "name each kind of text the replacement measurer has to handle" in {
    val ids = TextMeasureCorpus.lines.map(_.id).toSet
    ids should contain allOf (
      "ascii_code",
      "kerning_prose",
      "ligature_code",
      "tabs",
      "latin_marks",
      "cjk",
      "emoji_clusters",
      "paragraph",
      "ltr_rtl"
    )
  }

  it should "give no two measurements the same key" in {
    entries.map(_.key).distinct.length shouldBe entries.length
  }
