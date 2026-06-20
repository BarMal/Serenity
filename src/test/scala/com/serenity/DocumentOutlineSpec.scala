package com.serenity

import com.serenity.document.DocumentOutline
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.layout.{Location, Symbol, SymbolKind}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DocumentOutlineSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

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

  it should "leave single-section plaintext buffers without document navigation symbols" in {
    val buffer = Buffer.fromString(BufferId(1), "Only one paragraph")

    DocumentOutline.forBuffer(buffer) shouldBe Nil
  }
end DocumentOutlineSpec
