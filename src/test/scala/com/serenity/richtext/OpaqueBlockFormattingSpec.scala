package com.serenity.richtext

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Formatting a block line records nothing, so the saved bytes cannot differ from what the screen shows. */
class OpaqueBlockFormattingSpec extends AnyFlatSpec with Matchers with OptionValues:
  private val block = RichTextParagraph.block("<w:tbl/>", DocumentFeature.Tables)

  private val document = RichTextDocument(
    List(RichTextParagraph.plain("before"), block, RichTextParagraph.plain("after"))
  )

  private val everything = RichTextRange(RichTextPosition(0, 0), RichTextPosition(2, 5))
  private val onBlock    = RichTextRange(RichTextPosition(1, 0), RichTextPosition(1, 1))

  "Formatting a range over a block" should "leave the block line as it was and format the text around it" in {
    val formatted = document.applyMark(everything, InlineMark.Bold)

    formatted.paragraphAt(1).value shouldBe block
    formatted.paragraphAt(0).value.hasMarkThroughout(0, 6, InlineMark.Bold) shouldBe true
    formatted.paragraphAt(2).value.hasMarkThroughout(0, 5, InlineMark.Bold) shouldBe true
  }

  it should "not let the block stop a toggle from removing a mark" in {
    val bold = document.applyMark(everything, InlineMark.Bold)

    bold
      .toggleMark(everything, InlineMark.Bold)
      .paragraphAt(0)
      .value
      .hasMarkThroughout(0, 6, InlineMark.Bold) shouldBe false
  }

  it should "ignore font, colour and size changes on the block" in {
    document.setFontFamily(onBlock, "Serif").paragraphAt(1).value shouldBe block
    document.setColor(onBlock, "#ff0000").paragraphAt(1).value shouldBe block
    document.setFontSize(onBlock, 20f).paragraphAt(1).value shouldBe block
  }

  it should "ignore a role or alignment change on the block" in {
    document.setParagraphRole(onBlock, ParagraphRole.Heading(1)).paragraphAt(1).value shouldBe block
    document.setParagraphAlignment(onBlock, ParagraphAlignment.Center).paragraphAt(1).value shouldBe block
  }
