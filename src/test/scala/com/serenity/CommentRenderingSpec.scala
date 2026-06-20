package com.serenity

import com.serenity.document.CommentRendering
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, CursorPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommentRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "CommentRendering" should "extract raw and inline markdown views from line comments" in {
    val buffer = Buffer
      .fromString(BufferId(1), "val x = 1\n// **Important** note")
      .copy(language = Some(LanguageId.Scala), cursors = List(CursorPosition(1, 4)))

    val comment = CommentRendering.atCursor(buffer).getOrElse(fail("Expected comment"))

    comment.sourceLine shouldBe 1
    comment.raw shouldBe "// **Important** note"
    comment.inlineMarkdown shouldBe "Important note"
  }

  it should "extract block comments from code buffers" in {
    val buffer = Buffer
      .fromString(BufferId(1), "/* _Draft_ note */")
      .copy(language = Some(LanguageId.JavaScript), cursors = List(CursorPosition(0, 3)))

    val comment = CommentRendering.atCursor(buffer).getOrElse(fail("Expected comment"))

    comment.raw shouldBe "/* _Draft_ note */"
    comment.inlineMarkdown shouldBe "Draft note"
  }

  it should "extract prose comments from markdown buffers" in {
    val buffer = Buffer
      .fromString(BufferId(1), "<!-- **Review** this paragraph -->")
      .copy(language = Some(LanguageId.Markdown), cursors = List(CursorPosition(0, 8)))

    val comment = CommentRendering.atCursor(buffer).getOrElse(fail("Expected comment"))

    comment.raw shouldBe "<!-- **Review** this paragraph -->"
    comment.inlineMarkdown shouldBe "Review this paragraph"
  }

  it should "return no comment for ordinary source lines" in {
    val buffer = Buffer
      .fromString(BufferId(1), "val x = 1")
      .copy(language = Some(LanguageId.Scala), cursors = List(CursorPosition(0, 0)))

    CommentRendering.atCursor(buffer) shouldBe None
  }

end CommentRenderingSpec
