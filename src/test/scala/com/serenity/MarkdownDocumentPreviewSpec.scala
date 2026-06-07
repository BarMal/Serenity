package com.serenity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.serenity.markdown.MarkdownDocumentPreview

class MarkdownDocumentPreviewSpec extends AnyFlatSpec with Matchers:

  "MarkdownDocumentPreview" should "render headings as document presentation rows without source markers" in {
    val rows = MarkdownDocumentPreview.render(
      """# Release Notes
        |
        |## Highlights
        |Plain text""".stripMargin,
      maxWidth = 40
    )

    rows.take(5) shouldBe List(
      "RELEASE NOTES",
      "=============",
      "",
      "Highlights",
      "----------"
    )
    rows should contain("Plain text")
  }

  it should "align markdown tables as readable document rows" in {
    val rows = MarkdownDocumentPreview.render(
      """| Name | Role |
        || ---- | ---- |
        || Ada | Lead |
        || Grace | Reviewer |""".stripMargin,
      maxWidth = 80
    )

    rows shouldBe List(
      "Name   Role",
      "-----  --------",
      "Ada    Lead",
      "Grace  Reviewer"
    )
  }

  it should "present images and inline links without hiding editable source in the buffer" in {
    val rows = MarkdownDocumentPreview.render(
      """![Architecture](docs/arch.png)
        |Read the [guide](docs/guide.md) before release.""".stripMargin,
      maxWidth = 80
    )

    rows shouldBe List(
      "Image: Architecture (docs/arch.png)",
      "Read the guide (docs/guide.md) before release."
    )
  }

  it should "wrap prose to the preview width" in {
    val rows = MarkdownDocumentPreview.render(
      "This preview should wrap long prose without requiring the editor buffer to reflow.",
      maxWidth = 24
    )

    rows shouldBe List(
      "This preview should wrap",
      "long prose without",
      "requiring the editor",
      "buffer to reflow."
    )
  }

end MarkdownDocumentPreviewSpec
