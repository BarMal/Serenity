package com.serenity

import com.serenity.document.DocumentOutline
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.layout.{Location, Symbol, SymbolKind}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DocumentOutlineSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final case class NonCollectingRope(delegate: Rope) extends Rope:
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

    override def lineCount: Int =
      delegate.lineCount

    override def getLine(lineIndex: Int): Option[String] =
      delegate.getLine(lineIndex)

    override def lineColumnToOffset(line: Int, column: Int): Int =
      delegate.lineColumnToOffset(line, column)

    override def collect(): String =
      throw AssertionError("outline generation should not materialise the whole buffer")

  "DocumentOutline" should "extract Markdown headings as document navigation symbols" in {
    val buffer = Buffer
      .fromString(
        BufferId(1),
        """# Chapter One
          |
          |Body
          |
          |## Scene Two
          |not a heading
          |### Beat Three""".stripMargin
      )
      .copy(language = Some(LanguageId.Markdown))

    DocumentOutline.forBuffer(buffer) shouldBe List(
      Symbol("Chapter One", SymbolKind.Heading, Location(0, 0)),
      Symbol("Scene Two", SymbolKind.Heading, Location(4, 0)),
      Symbol("Beat Three", SymbolKind.Heading, Location(6, 0))
    )
  }

  it should "leave non-Markdown buffers without document navigation symbols" in {
    val buffer = Buffer.fromString(BufferId(1), "# Not Markdown").copy(language = Some(LanguageId.Scala))

    DocumentOutline.forBuffer(buffer) shouldBe Nil
  }

  it should "extract plaintext paragraph starts as section navigation symbols" in {
    val buffer = Buffer.fromString(
      BufferId(1),
      """Opening line
        |still opening
        |
        |Second section
        |more body
        |
        |A very long section title that should be clipped before it overwhelms the outline panel""".stripMargin
    )

    DocumentOutline.forBuffer(buffer) shouldBe List(
      Symbol("Opening line", SymbolKind.Section, Location(0, 0)),
      Symbol("Second section", SymbolKind.Section, Location(3, 0)),
      Symbol("A very long section title that should be clipped be...", SymbolKind.Section, Location(6, 0))
    )
  }

  it should "extract rich text heading paragraphs as document navigation symbols" in {
    val richDocument = RichTextDocument(
      List(
        RichTextParagraph.plain("Chapter One", role = ParagraphRole.Heading(1)),
        RichTextParagraph.plain("Body"),
        RichTextParagraph.plain("Scene Two", role = ParagraphRole.Heading(2))
      )
    )
    val buffer = Buffer
      .fromString(BufferId(1), "Chapter One\nBody\nScene Two")
      .copy(richTextDocument = Some(richDocument))

    DocumentOutline.forBuffer(buffer) shouldBe List(
      Symbol("Chapter One", SymbolKind.Heading, Location(0, 0)),
      Symbol("Scene Two", SymbolKind.Heading, Location(2, 0))
    )
  }

  it should "leave single-section plaintext buffers without document navigation symbols" in {
    val buffer = Buffer.fromString(BufferId(1), "Only one paragraph")

    DocumentOutline.forBuffer(buffer) shouldBe Nil
  }

  it should "extract Markdown headings without materialising the whole rope" in {
    val content = NonCollectingRope(Rope("# One\nbody\n## Two"))
    val buffer  = Buffer(BufferId(2), content).copy(language = Some(LanguageId.Markdown))

    DocumentOutline.forBuffer(buffer) shouldBe List(
      Symbol("One", SymbolKind.Heading, Location(0, 0)),
      Symbol("Two", SymbolKind.Heading, Location(2, 0))
    )
  }

  it should "extract plaintext sections without materialising the whole rope" in {
    val content = NonCollectingRope(Rope("Opening\nbody\n\nSecond\nbody"))
    val buffer  = Buffer(BufferId(3), content)

    DocumentOutline.forBuffer(buffer) shouldBe List(
      Symbol("Opening", SymbolKind.Section, Location(0, 0)),
      Symbol("Second", SymbolKind.Section, Location(3, 0))
    )
  }

  it should "validate rich text headings against buffer lines without materialising the whole rope" in {
    val richDocument = RichTextDocument(
      List(
        RichTextParagraph.plain("Chapter One", role = ParagraphRole.Heading(1)),
        RichTextParagraph.plain("Body")
      )
    )
    val content = NonCollectingRope(Rope("Chapter One\nBody"))
    val buffer  = Buffer(BufferId(4), content).copy(richTextDocument = Some(richDocument))

    DocumentOutline.forBuffer(buffer) shouldBe List(
      Symbol("Chapter One", SymbolKind.Heading, Location(0, 0))
    )
  }
end DocumentOutlineSpec
