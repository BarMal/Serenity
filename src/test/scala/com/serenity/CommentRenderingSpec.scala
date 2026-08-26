package com.serenity

import com.serenity.document.CommentRendering
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommentRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "CommentRendering" should "extract raw and inline markdown views from line comments" in {
    val base = Buffer.fromString(BufferId(1), "val x = 1\n// **Important** note")
    val buffer = base
      .copy(document = base.document.copy(language = Some(LanguageId.Scala)), editing = base.editing.copy(cursors = List(CursorPosition(1, 4))))

    val comment = CommentRendering.atCursor(buffer).getOrElse(fail("Expected comment"))

    comment.sourceLine shouldBe 1
    comment.raw shouldBe "// **Important** note"
    comment.inlineMarkdown shouldBe "Important note"
  }

  it should "extract block comments from code buffers" in {
    val base = Buffer.fromString(BufferId(1), "/* _Draft_ note */")
    val buffer = base
      .copy(document = base.document.copy(language = Some(LanguageId.JavaScript)), editing = base.editing.copy(cursors = List(CursorPosition(0, 3))))

    val comment = CommentRendering.atCursor(buffer).getOrElse(fail("Expected comment"))

    comment.raw shouldBe "/* _Draft_ note */"
    comment.inlineMarkdown shouldBe "Draft note"
  }

  it should "extract multiline block comments from code buffers" in {
    val base = Buffer.fromString(
      BufferId(1),
      """val x = 1
        |/*
        | * **Review** this value
        | * before release
        | */
        |val y = 2""".stripMargin
    )
    val buffer = base
      .copy(document = base.document.copy(language = Some(LanguageId.Scala)), editing = base.editing.copy(cursors = List(CursorPosition(2, 6))))

    val comment = CommentRendering.atCursor(buffer).getOrElse(fail("Expected comment"))

    comment.sourceLine shouldBe 1
    comment.raw shouldBe "/*\n* **Review** this value\n* before release\n*/"
    comment.inlineMarkdown shouldBe "Review this value\nbefore release"
  }

  it should "extract prose comments from markdown buffers" in {
    val base = Buffer.fromString(BufferId(1), "<!-- **Review** this paragraph -->")
    val buffer = base
      .copy(document = base.document.copy(language = Some(LanguageId.Markdown)), editing = base.editing.copy(cursors = List(CursorPosition(0, 8))))

    val comment = CommentRendering.atCursor(buffer).getOrElse(fail("Expected comment"))

    comment.raw shouldBe "<!-- **Review** this paragraph -->"
    comment.inlineMarkdown shouldBe "Review this paragraph"
  }

  it should "render authored document comments at the cursor" in {
    val base = Buffer.fromString(BufferId(1), "Chapter text")
    val buffer = base
      .copy(
        editing = base.editing.copy(cursors = List(CursorPosition(0, 4))),
        annotations = base.annotations.copy(documentComments = List(
          DocumentComment(
            anchor = CursorPosition(0, 0),
            focus = CursorPosition(0, 7),
            text = "**Tighten** this opening."
          )
        ))
      )

    val comment = CommentRendering.atCursor(buffer).getOrElse(fail("Expected comment"))

    comment.sourceLine shouldBe 0
    comment.raw shouldBe "**Tighten** this opening."
    comment.inlineMarkdown shouldBe "Tighten this opening."
  }

  it should "extract multiline prose comments from markdown buffers" in {
    val base = Buffer.fromString(
      BufferId(1),
      """# Notes
        |<!--
        |**Review** this paragraph
        |before publishing
        |-->""".stripMargin
    )
    val buffer = base
      .copy(document = base.document.copy(language = Some(LanguageId.Markdown)), editing = base.editing.copy(cursors = List(CursorPosition(2, 4))))

    val comment = CommentRendering.atCursor(buffer).getOrElse(fail("Expected comment"))

    comment.sourceLine shouldBe 1
    comment.raw shouldBe "<!--\n**Review** this paragraph\nbefore publishing\n-->"
    comment.inlineMarkdown shouldBe "Review this paragraph\nbefore publishing"
  }

  it should "return no comment for ordinary source lines" in {
    val base = Buffer.fromString(BufferId(1), "val x = 1")
    val buffer = base
      .copy(document = base.document.copy(language = Some(LanguageId.Scala)), editing = base.editing.copy(cursors = List(CursorPosition(0, 0))))

    CommentRendering.atCursor(buffer) shouldBe None
  }

  it should "read only the cursor line when rendering a line comment" in {
    val targetLine = 9000
    val source = (0 to 10000)
      .map { line =>
        if line == targetLine then "// **Important** note"
        else s"val value$line = $line"
      }
      .mkString("\n")
    val guardedContent = GuardedGetLineRope(Rope(source), allowedLines = Set(targetLine))
    val base = Buffer.fromString(BufferId(1), "")
    val buffer = base
      .copy(
        document = base.document.copy(content = guardedContent, language = Some(LanguageId.Scala)),
        editing = base.editing.copy(cursors = List(CursorPosition(targetLine, 4)))
      )

    val comment = CommentRendering.atCursor(buffer).getOrElse(fail("Expected comment"))

    comment.sourceLine shouldBe targetLine
    comment.raw shouldBe "// **Important** note"
    comment.inlineMarkdown shouldBe "Important note"
  }

  final case class GuardedGetLineRope(delegate: Rope, allowedLines: Set[Int]) extends Rope:
    override def weight: Int =
      delegate.weight

    override def height: Int =
      delegate.height

    override def newlineCount: Int =
      delegate.newlineCount

    override def lastLineLength: Int =
      delegate.lastLineLength

    override def endsWithNewline: Boolean =
      delegate.endsWithNewline

    override def isWeightBalanced: Boolean =
      delegate.isWeightBalanced

    override def isHeightBalanced: Boolean =
      delegate.isHeightBalanced

    override def rebalance: Rope =
      this

    override def index(i: Int): Option[Char] =
      delegate.index(i)

    override def splitAt(index: Int): Option[(Rope, Rope)] =
      delegate.splitAt(index)

    override def getLine(lineIndex: Int): Option[String] =
      if allowedLines.contains(lineIndex) then delegate.getLine(lineIndex)
      else throw AssertionError(s"comment rendering should not read line $lineIndex")

    override def collect(): String =
      throw AssertionError("comment rendering should not materialise the whole buffer")

end CommentRenderingSpec
