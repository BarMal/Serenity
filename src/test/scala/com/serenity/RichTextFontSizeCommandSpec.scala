package com.serenity

import com.serenity.command.*
import com.serenity.richtext.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RichTextFontSizeCommandSpec extends AnyFlatSpec with Matchers:

  private def fullRange(text: String): RichTextRange =
    RichTextRange(RichTextPosition(0, 0), RichTextPosition(0, text.length))

  "RichTextDocument.adjustFontSize" should "nudge an un-sized run from the default size" in {
    val document = RichTextDocument(List(RichTextParagraph.plain("abc")))
    val adjusted = document.adjustFontSize(fullRange("abc"), deltaPt = 2.0f, defaultSize = 12.0f)
    adjusted.paragraphs.head.runs.head.style.fontSize shouldBe Some(14.0f)
  }

  it should "nudge an already-sized run relative to its current size" in {
    val styled   = RichTextParagraph(List(RichTextRun("abc", RichTextStyle.empty.withFontSize(20.0f))))
    val document = RichTextDocument(List(styled))
    document.adjustFontSize(fullRange("abc"), -6.0f, 12.0f).paragraphs.head.runs.head.style.fontSize shouldBe Some(
      14.0f
    )
  }

  it should "never drop below 1pt" in {
    val document = RichTextDocument(List(RichTextParagraph.plain("abc")))
    document.adjustFontSize(fullRange("abc"), -100.0f, 12.0f).paragraphs.head.runs.head.style.fontSize shouldBe Some(
      1.0f
    )
  }

  "The command registry" should "expose increase/decrease text-size commands on the selection" in {
    val registry = CommandRegistry.default
    registry.findCommand("increase-text-size").map(_.intent) shouldBe
      Some(CommandIntent.RichText(RichTextIntent.AdjustRichTextFontSize(2.0f)))
    registry.findCommand("decrease-text-size").map(_.intent) shouldBe
      Some(CommandIntent.RichText(RichTextIntent.AdjustRichTextFontSize(-2.0f)))
  }
