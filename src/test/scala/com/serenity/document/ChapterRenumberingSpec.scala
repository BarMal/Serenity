package com.serenity.document

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChapterRenumberingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def markdownBuffer(text: String): Buffer =
    val buffer = Buffer.fromString(BufferId(0), text)
    buffer.copy(document = buffer.document.copy(language = Some(LanguageId.Markdown)))

  private def edited(text: String): String =
    val buffer = markdownBuffer(text)
    // Applied in descending offset order, like the real reducer's multi-edit application, so an earlier edit's
    // offsets are never invalidated by a later one changing the text's length before it.
    ChapterRenumbering
      .pendingRenumbers(buffer)
      .sortBy { case (start, _, _) => -start }
      .foldLeft(buffer.document.content) {
        case (content, (start, end, replacement)) =>
          content.delete(start, end).getOrElse(content).insert(start, replacement).getOrElse(content)
      }
      .collect()

  "pendingRenumbers" should "find nothing to do when there are no chapter headings" in {
    ChapterRenumbering.pendingRenumbers(markdownBuffer("# Introduction\n\nSome text.\n")) shouldBe empty
  }

  it should "find nothing to do when chapters are already numbered 1, 2, 3 in order" in {
    val text = "# Chapter 1\n\ntext\n\n# Chapter 2\n\ntext\n\n# Chapter 3\n"
    ChapterRenumbering.pendingRenumbers(markdownBuffer(text)) shouldBe empty
  }

  it should "renumber chapters left out of sequence back to 1, 2, 3" in {
    val text = "# Chapter 5\n\ntext\n\n# Chapter 5\n\ntext\n\n# Chapter 9\n"
    edited(text) shouldBe "# Chapter 1\n\ntext\n\n# Chapter 2\n\ntext\n\n# Chapter 3\n"
  }

  it should "leave non-chapter headings untouched and out of the sequence count" in {
    val text = "# Introduction\n\n# Chapter 1\n\n# Notes\n\n# Chapter 5\n"
    edited(text) shouldBe "# Introduction\n\n# Chapter 1\n\n# Notes\n\n# Chapter 2\n"
  }

  it should "match case-insensitively" in {
    val text = "# chapter 4\n"
    edited(text) shouldBe "# chapter 1\n"
  }

  it should "preserve a trailing title after the chapter number" in {
    val text = "# Chapter 7: The Storm\n"
    edited(text) shouldBe "# Chapter 1: The Storm\n"
  }

  it should "do nothing for a buffer that is not Markdown" in {
    val buffer = Buffer.fromString(BufferId(0), "# Chapter 5\n\n# Chapter 9\n")
    ChapterRenumbering.pendingRenumbers(buffer) shouldBe empty
  }
