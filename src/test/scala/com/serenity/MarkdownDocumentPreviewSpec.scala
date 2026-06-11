package com.serenity

import java.awt.Font
import java.nio.file.Paths

import com.serenity.markdown.MarkdownDocumentPreview
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkdownDocumentPreviewSpec extends AnyFlatSpec with Matchers:

  "MarkdownDocumentPreview" should "render CommonMark HTML for headings without source markers" in {
    val html = MarkdownDocumentPreview.renderHtmlFragment(
      """# Release Notes
        |
        |## Highlights
        |Plain text""".stripMargin,
      title = "notes.md"
    )

    html should include("<h1>Release Notes</h1>")
    html should include("<h2>Highlights</h2>")
    html should not include "# Release Notes"
  }

  it should "render GitHub-flavoured tables through the Markdown library" in {
    val html = MarkdownDocumentPreview.renderHtmlFragment(
      """| Name | Role |
        || ---- | ---- |
        || Ada | Lead |
        || Grace | Reviewer |""".stripMargin,
      title = "table.md"
    )

    html should include("<table>")
    html should include("<th>Name</th>")
    html should include("<td>Grace</td>")
  }

  it should "format markdown tables for inline lens rendering as closed box tables" in {
    val rows = MarkdownDocumentPreview.renderInlineLines(
      Vector(
        "| Task | Owner |",
        "| ---- | ----- |",
        "| Ship | Codex |",
        "| Test longer | QA |"
      )
    )

    rows shouldBe Vector(
      "\u250c\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u252c\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2510",
      "\u2502 Task        \u2502 Owner \u2502",
      "\u251c\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u253c\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2524",
      "\u2502 Ship        \u2502 Codex \u2502",
      "\u2502 Test longer \u2502 QA    \u2502",
      "\u2514\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2534\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2518"
    )
    rows.mkString("\n") should not include "| ---- | ----- |"
  }

  it should "identify only table source rows for block-level inline rendering" in {
    val lines = Vector(
      "Before",
      "| Task | Owner |",
      "| ---- | ----- |",
      "| Ship | Codex |",
      "",
      "After | not a table"
    )

    MarkdownDocumentPreview.inlineTableLineIndexes(lines) shouldBe Set(1, 2, 3)
  }

  it should "map inline preview rows back to their source lines" in {
    val lines = Vector(
      "Before",
      "| Task | Owner |",
      "| ---- | ----- |",
      "| Ship | Codex |",
      "After"
    )

    val preview = MarkdownDocumentPreview.renderInlineDocument(lines)

    preview.map(_.text) shouldBe Vector(
      "Before",
      "\u250c\u2500\u2500\u2500\u2500\u2500\u2500\u252c\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2510",
      "\u2502 Task \u2502 Owner \u2502",
      "\u251c\u2500\u2500\u2500\u2500\u2500\u2500\u253c\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2524",
      "\u2502 Ship \u2502 Codex \u2502",
      "\u2514\u2500\u2500\u2500\u2500\u2500\u2500\u2534\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2518",
      "After"
    )
    preview.map(_.sourceLine) shouldBe Vector(Some(0), None, Some(1), Some(2), Some(3), None, Some(4))
    MarkdownDocumentPreview.previewRowForSourceLine(lines, 3) shouldBe Some(4)
  }

  it should "preserve rendered image elements in the document preview HTML" in {
    val html = MarkdownDocumentPreview.renderHtmlFragment(
      """![Architecture](docs/arch.png)
        |Read the [guide](docs/guide.md) before release.""".stripMargin,
      title = "images.md"
    )

    html should include("""<img src="docs/arch.png" alt="Architecture" />""")
    html should include("""<a href="docs/guide.md">guide</a>""")
  }

  it should "resolve relative image sources against the markdown file directory" in {
    val baseUri  = Paths.get("docs").toAbsolutePath.toUri
    val expected = baseUri.resolve("arch.png").toString
    val html = MarkdownDocumentPreview.renderHtmlFragment(
      "![Architecture](arch.png)",
      title = "images.md",
      baseUri = Some(baseUri)
    )

    html should include(s"""src="$expected"""")
  }

  it should "render Markdown HTML to a Java2D image for pinned previews" in {
    val image = MarkdownDocumentPreview.renderImage(
      source = """# Rendered
                 |
                 || Name | Role |
                 || ---- | ---- |
                 || Ada | Lead |""".stripMargin,
      title = "rendered.md",
      widthPx = 420,
      heightPx = 280,
      theme = Theme.default,
      font = Font(Font.SANS_SERIF, Font.PLAIN, 14)
    )

    image.getWidth shouldBe 420
    image.getHeight shouldBe 280
  }

end MarkdownDocumentPreviewSpec
