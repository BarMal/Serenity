package com.serenity

import com.serenity.richtext.{DocumentFeature, InlineAtom, RichTextDocument, RichTextParagraph}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.text.TextStatistics
import com.serenity.ui.theme.RichTextStyling
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A block line is one placeholder character that is drawn as a chip and read as nothing by every plain-text path. */
class OpaqueBlockPaintSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val document = RichTextDocument(
    List(
      RichTextParagraph.plain("before"),
      RichTextParagraph.block("<w:tbl/>", DocumentFeature.Tables),
      RichTextParagraph.plain("after")
    )
  )

  private val buffer = Buffer(
    id = BufferId(0),
    document = Document(content = Rope(document.plainText)),
    richText = RichTextState().withSyncedDocument(Some(document), contentVersion = 0L)
  )

  "A block line" should "be drawn and measured as one chip glyph in the placeholder's slot" in {
    val spans = RichTextStyling.styledFontSpans(document, bufferLine = 1, startColumn = 0, endColumn = 1)

    spans.map(_.text) shouldBe List(RichTextStyling.BlockGlyph)
  }

  it should "not be a soft break, so exports do not add a newline for it" in {
    InlineAtom.BlockCharacter should not be InlineAtom.SoftBreakCharacter
    buffer.plainTextExport(document.plainText) shouldBe "before\n\nafter"
  }

  it should "count as no word and no character" in {
    val statistics = TextStatistics.of(Rope(document.plainText))

    statistics.wordCount shouldBe 2
    statistics.characterCountExcludingWhitespace shouldBe "beforeafter".length
  }

  it should "be read as an empty line by the plain-text save path" in {
    document.exportText shouldBe "before\n\nafter"
  }
