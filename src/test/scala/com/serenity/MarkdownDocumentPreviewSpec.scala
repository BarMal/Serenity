package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Color, Font}
import java.io.ByteArrayOutputStream
import java.net.{InetAddress, InetSocketAddress}
import java.nio.file.{Files, Path, Paths}
import java.util.Base64
import java.util.concurrent.atomic.AtomicInteger
import javax.imageio.ImageIO

import scala.util.Try

import com.serenity.markdown.{MarkdownDocumentPreview, MarkdownPreviewCache}
import com.serenity.ui.color.RenderColor
import com.serenity.ui.theme.Theme
import com.sun.net.httpserver.HttpServer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkdownDocumentPreviewSpec extends AnyFlatSpec with Matchers:

  // Shared across every test in this spec (issue #1677): `MarkdownDocumentPreview`'s caches are instance-scoped now,
  // not a JVM-wide singleton, so the "reuse"/"not reuse" identity assertions below need one real instance threaded
  // through both calls being compared, exactly as a single `StateManager`'s `RenderCaches.markdownPreviewCache` would
  // be threaded through its own repeated renders.
  private val cache = MarkdownPreviewCache()

  "MarkdownDocumentPreview" should "render CommonMark HTML for headings without source markers" in {
    val html = MarkdownDocumentPreview.renderHtmlFragment(
      """# Release Notes
        |
        |## Highlights
        |Plain text""".stripMargin,
      title = "notes.md",
      cache = cache
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
      title = "table.md",
      cache = cache
    )

    html should include("<table>")
    html should include("<th>Name</th>")
    html should include("<td>Grace</td>")
  }

  it should "reuse rendered HTML fragments for identical source and base URI" in {
    val source = "# Cached\n\nBody"
    val first  = MarkdownDocumentPreview.renderHtmlFragment(source, title = "cached.md", cache = cache)
    val second = MarkdownDocumentPreview.renderHtmlFragment(source, title = "cached.md", cache = cache)

    second should be theSameInstanceAs first
  }

  it should "not reuse cached HTML fragments when source content changes" in {
    val first  = MarkdownDocumentPreview.renderHtmlFragment("# Cached", title = "cached.md", cache = cache)
    val second = MarkdownDocumentPreview.renderHtmlFragment("# Changed", title = "cached.md", cache = cache)

    second should not be theSameInstanceAs(first)
    second should include("<h1>Changed</h1>")
  }

  it should "format markdown tables for panel preview rendering as closed box tables" in {
    val rows = MarkdownDocumentPreview.renderInlineLines(
      Vector(
        "| Task | Owner |",
        "| ---- | ----- |",
        "| Ship | Codex |",
        "| Test longer | QA |"
      ),
      cache = cache
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

  it should "map inline preview rows back to their source lines" in {
    val lines = Vector(
      "Before",
      "| Task | Owner |",
      "| ---- | ----- |",
      "| Ship | Codex |",
      "After"
    )

    val preview = MarkdownDocumentPreview.renderInlineDocument(lines, cache = cache)

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
  }

  it should "preserve rendered image elements in the document preview HTML" in {
    val html = MarkdownDocumentPreview.renderHtmlFragment(
      """![Architecture](docs/arch.png)
        |Read the [guide](docs/guide.md) before release.""".stripMargin,
      title = "images.md",
      cache = cache
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
      baseUri = Some(baseUri),
      cache = cache
    )

    html should include(s"""src="$expected"""")
  }

  it should "render local images only when they are below the preview resource root" in {
    val root       = TestTemp.directory("serenity-markdown-preview-root")
    val localImage = root.resolve("local.png")
    writeSolidImage(localImage, Color(220, 30, 40), width = 8, height = 8)

    val image = MarkdownDocumentPreview.renderImage(
      source = "![Local](local.png)",
      title = "local.md",
      widthPx = 180,
      heightPx = 120,
      theme = Theme.default,
      font = Font(Font.SANS_SERIF, Font.PLAIN, 14),
      baseUri = Some(root.toUri),
      cache = cache
    )

    containsColor(image, Color(220, 30, 40)) shouldBe true
  }

  it should "deny images outside the preview resource root" in {
    val root        = TestTemp.directory("serenity-markdown-preview-root")
    val outsideRoot = TestTemp.directory("serenity-markdown-preview-outside")
    val outside     = outsideRoot.resolve("outside.png")
    writeSolidImage(outside, Color(30, 220, 40), width = 8, height = 8)

    val image = MarkdownDocumentPreview.renderImage(
      source = s"![Outside](${outside.toUri})",
      title = "outside.md",
      widthPx = 180,
      heightPx = 120,
      theme = Theme.default,
      font = Font(Font.SANS_SERIF, Font.PLAIN, 14),
      baseUri = Some(root.toUri),
      cache = cache
    )

    containsColor(image, Color(30, 220, 40)) shouldBe false
  }

  it should "deny relative traversal outside the preview resource root" in {
    val root        = TestTemp.directory("serenity-markdown-preview-root")
    val outsideRoot = TestTemp.directory("serenity-markdown-preview-outside")
    val outside     = outsideRoot.resolve("outside.png")
    writeSolidImage(outside, Color(30, 220, 40), width = 8, height = 8)

    val image = MarkdownDocumentPreview.renderImage(
      source = "![Traversal](../" + outsideRoot.getFileName + "/outside.png)",
      title = "traversal.md",
      widthPx = 180,
      heightPx = 120,
      theme = Theme.default,
      font = Font(Font.SANS_SERIF, Font.PLAIN, 14),
      baseUri = Some(root.toUri),
      cache = cache
    )

    containsColor(image, Color(30, 220, 40)) shouldBe false
  }

  it should "deny symlinked images that resolve outside the preview resource root" in {
    val root        = TestTemp.directory("serenity-markdown-preview-root")
    val outsideRoot = TestTemp.directory("serenity-markdown-preview-outside")
    val outside     = outsideRoot.resolve("outside.png")
    writeSolidImage(outside, Color(30, 220, 40), width = 8, height = 8)
    val link = Try(Files.createSymbolicLink(root.resolve("linked.png"), outside)).toOption
    assume(link.nonEmpty, "symbolic links are unavailable on this platform")

    val image = MarkdownDocumentPreview.renderImage(
      source = "![Symlink](linked.png)",
      title = "symlink.md",
      widthPx = 180,
      heightPx = 120,
      theme = Theme.default,
      font = Font(Font.SANS_SERIF, Font.PLAIN, 14),
      baseUri = Some(root.toUri),
      cache = cache
    )

    containsColor(image, Color(30, 220, 40)) shouldBe false
  }

  it should "deny remote images without making a network request" in {
    val requests = new AtomicInteger(0)
    val server   = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress, 0), 0)
    server.createContext(
      "/image.png",
      exchange =>
        requests.incrementAndGet()
        val body = Array.emptyByteArray
        exchange.sendResponseHeaders(200, body.length)
        exchange.getResponseBody.close()
    )
    server.start()
    try
      val image = MarkdownDocumentPreview.renderImage(
        source = s"![Remote](http://127.0.0.1:${server.getAddress.getPort}/image.png)",
        title = "remote.md",
        widthPx = 180,
        heightPx = 120,
        theme = Theme.default,
        font = Font(Font.SANS_SERIF, Font.PLAIN, 14),
        cache = cache
      )

      image.getWidth shouldBe 180
      image.getHeight shouldBe 120
      requests.get() shouldBe 0
    finally server.stop(0)
  }

  it should "deny oversized data URI images before decoding them" in {
    val embedded = new BufferedImage(8, 8, BufferedImage.TYPE_INT_ARGB)
    for
      x <- 0 until embedded.getWidth
      y <- 0 until embedded.getHeight
    do embedded.setRGB(x, y, Color(220, 30, 40).getRGB)
    val embeddedBytes = new ByteArrayOutputStream()
    val _             = ImageIO.write(embedded, "png", embeddedBytes)
    val validImage = MarkdownDocumentPreview.renderImage(
      source = s"![Embedded](data:image/png;base64,${Base64.getEncoder.encodeToString(embeddedBytes.toByteArray)})",
      title = "embedded-valid.md",
      widthPx = 180,
      heightPx = 120,
      theme = Theme.default,
      font = Font(Font.SANS_SERIF, Font.PLAIN, 14),
      cache = cache
    )
    val payload = Base64.getEncoder.encodeToString(Array.fill[Byte](3 * 1024 * 1024)(1))
    val oversizedImage = MarkdownDocumentPreview.renderImage(
      source = s"![Embedded](data:image/png;base64,$payload)",
      title = "embedded.md",
      widthPx = 180,
      heightPx = 120,
      theme = Theme.default,
      font = Font(Font.SANS_SERIF, Font.PLAIN, 14),
      cache = cache
    )

    validImage.getWidth shouldBe 180
    validImage.getHeight shouldBe 120
    containsColor(validImage, Color(220, 30, 40)) shouldBe false
    oversizedImage.getWidth shouldBe 180
    oversizedImage.getHeight shouldBe 120
  }

  it should "bound image bytes and decoded dimensions" in {
    val root       = TestTemp.directory("serenity-markdown-preview-bounds")
    val largeBytes = root.resolve("large.bin")
    Files.write(largeBytes, Array.fill[Byte](3 * 1024 * 1024)(1))
    val largeImage = root.resolve("large.png")
    writeSolidImage(largeImage, Color(40, 30, 220), width = 5000, height = 1)

    val bytesBounded = MarkdownDocumentPreview.renderImage(
      source = "![Large bytes](large.bin)",
      title = "large-bytes.md",
      widthPx = 180,
      heightPx = 120,
      theme = Theme.default,
      font = Font(Font.SANS_SERIF, Font.PLAIN, 14),
      baseUri = Some(root.toUri),
      cache = cache
    )
    val dimensionsBounded = MarkdownDocumentPreview.renderImage(
      source = "![Large dimensions](large.png)",
      title = "large-dimensions.md",
      widthPx = 180,
      heightPx = 120,
      theme = Theme.default,
      font = Font(Font.SANS_SERIF, Font.PLAIN, 14),
      baseUri = Some(root.toUri),
      cache = cache
    )

    bytesBounded.getWidth shouldBe 180
    bytesBounded.getHeight shouldBe 120
    dimensionsBounded.getWidth shouldBe 180
    dimensionsBounded.getHeight shouldBe 120
    containsColor(bytesBounded, Color(40, 30, 220)) shouldBe false
    containsColor(dimensionsBounded, Color(40, 30, 220)) shouldBe false
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
      font = Font(Font.SANS_SERIF, Font.PLAIN, 14),
      cache = cache
    )

    image.getWidth shouldBe 420
    image.getHeight shouldBe 280
  }

  it should "fill the full split preview image with the panel background" in {
    val theme = Theme.default.copy(
      background = RenderColor.fromRgba(10, 20, 30),
      panel = Theme.default.panel.copy(background = RenderColor.fromRgba(40, 50, 60))
    )

    val image = MarkdownDocumentPreview.renderImage(
      source = "# Short",
      title = "short.md",
      widthPx = 180,
      heightPx = 240,
      theme = theme,
      font = Font(Font.SANS_SERIF, Font.PLAIN, 14),
      cache = cache
    )

    RenderColor.fromArgb(image.getRGB(image.getWidth - 2, image.getHeight - 2)) shouldBe theme.panel.background
  }

  it should "reuse rendered images for identical preview inputs" in {
    val font = Font(Font.SANS_SERIF, Font.PLAIN, 14)
    val first = MarkdownDocumentPreview.renderImage(
      source = "# Cached",
      title = "cached.md",
      widthPx = 240,
      heightPx = 160,
      theme = Theme.default,
      font = font,
      cache = cache
    )
    val second = MarkdownDocumentPreview.renderImage(
      source = "# Cached",
      title = "cached.md",
      widthPx = 240,
      heightPx = 160,
      theme = Theme.default,
      font = font,
      cache = cache
    )

    second should be theSameInstanceAs first
  }

  it should "not reuse cached preview images when markdown content changes" in {
    val font = Font(Font.SANS_SERIF, Font.PLAIN, 14)
    val first = MarkdownDocumentPreview.renderImage(
      source = "# Cached",
      title = "cached.md",
      widthPx = 240,
      heightPx = 160,
      theme = Theme.default,
      font = font,
      cache = cache
    )
    val second = MarkdownDocumentPreview.renderImage(
      source = "# Changed",
      title = "cached.md",
      widthPx = 240,
      heightPx = 160,
      theme = Theme.default,
      font = font,
      cache = cache
    )

    second should not be theSameInstanceAs(first)
  }

  it should "leave rendering unaffected by default -- reuse is opt-in via reuseLastRenderWhileEditing" in {
    val font = Font(Font.SANS_SERIF, Font.PLAIN, 14)
    val first = MarkdownDocumentPreview.renderImage(
      source = "# Default first",
      title = "reuse-default.md",
      widthPx = 240,
      heightPx = 160,
      theme = Theme.default,
      font = font,
      cache = cache
    )
    val second = MarkdownDocumentPreview.renderImage(
      source = "# Default second, totally different content",
      title = "reuse-default.md",
      widthPx = 240,
      heightPx = 160,
      theme = Theme.default,
      font = font,
      cache = cache
    )

    second should not be theSameInstanceAs(first)
  }

  it should "reuse the last rendered image for changing markdown content while an edit burst is signalled in flight" in {
    val font = Font(Font.SANS_SERIF, Font.PLAIN, 14)
    val first = MarkdownDocumentPreview.renderImage(
      source = "# Edit burst first",
      title = "edit-burst.md",
      widthPx = 240,
      heightPx = 160,
      theme = Theme.default,
      font = font,
      reuseLastRenderWhileEditing = true,
      cache = cache
    )
    val second = MarkdownDocumentPreview.renderImage(
      source = "# Edit burst second, totally different content",
      title = "edit-burst.md",
      widthPx = 240,
      heightPx = 160,
      theme = Theme.default,
      font = font,
      reuseLastRenderWhileEditing = true,
      cache = cache
    )

    second should be theSameInstanceAs first
  }

  it should "render fresh once the caller signals the edit burst has settled" in {
    val font = Font(Font.SANS_SERIF, Font.PLAIN, 14)
    val first = MarkdownDocumentPreview.renderImage(
      source = "# Settled first",
      title = "settled.md",
      widthPx = 240,
      heightPx = 160,
      theme = Theme.default,
      font = font,
      reuseLastRenderWhileEditing = true,
      cache = cache
    )
    val second = MarkdownDocumentPreview.renderImage(
      source = "# Settled second",
      title = "settled.md",
      widthPx = 240,
      heightPx = 160,
      theme = Theme.default,
      font = font,
      reuseLastRenderWhileEditing = false,
      cache = cache
    )

    second should not be theSameInstanceAs(first)
  }

  it should "not reuse a rendered image across a different rendered size even while mid-edit" in {
    val font = Font(Font.SANS_SERIF, Font.PLAIN, 14)
    val _ = MarkdownDocumentPreview.renderImage(
      source = "# Edit-burst sizing",
      title = "edit-burst-sizing.md",
      widthPx = 240,
      heightPx = 160,
      theme = Theme.default,
      font = font,
      reuseLastRenderWhileEditing = true,
      cache = cache
    )
    val resized = MarkdownDocumentPreview.renderImage(
      source = "# Edit-burst sizing",
      title = "edit-burst-sizing.md",
      widthPx = 320,
      heightPx = 160,
      theme = Theme.default,
      font = font,
      reuseLastRenderWhileEditing = true,
      cache = cache
    )

    resized.getWidth shouldBe 320
  }

  it should "render fresh content immediately when no prior render exists for that slot" in {
    val image = MarkdownDocumentPreview.renderImage(
      source = "# First ever render for this slot",
      title = "reuse-fresh.md",
      widthPx = 200,
      heightPx = 140,
      theme = Theme.default,
      font = Font(Font.SANS_SERIF, Font.PLAIN, 14),
      reuseLastRenderWhileEditing = true,
      cache = cache
    )

    image.getWidth shouldBe 200
    image.getHeight shouldBe 140
  }

  private def writeSolidImage(path: Path, color: Color, width: Int, height: Int): Unit =
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    for
      x <- 0 until width
      y <- 0 until height
    do image.setRGB(x, y, color.getRGB)
    val _ = ImageIO.write(image, "png", path.toFile)

  private def containsColor(image: BufferedImage, color: Color): Boolean =
    (for
      x <- 0 until image.getWidth
      y <- 0 until image.getHeight
      if image.getRGB(x, y) == color.getRGB
    yield true).headOption.getOrElse(false)

end MarkdownDocumentPreviewSpec
