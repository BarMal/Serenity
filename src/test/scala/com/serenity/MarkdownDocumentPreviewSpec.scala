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
