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
    val buffer = Buffer.fromString(BufferId(1), "# Not Markdown")

    DocumentOutline.forBuffer(buffer) shouldBe Nil
  }
end DocumentOutlineSpec
