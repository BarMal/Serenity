package com.serenity.markdown

import java.awt.Font

import cats.effect.IO
import cats.effect.syntax.all.*
import cats.effect.unsafe.implicits.global
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1677: `MarkdownPreviewCache` (the HTML-fragment/image/inline-document caches backing [[MarkdownDocumentPreview]])
  * used to live as a JVM-wide singleton `object`, shared by every caller and every concurrently-running spec in the
  * same JVM. It is instance-scoped now -- one instance per render-owning entity, held on
  * [[com.serenity.state.manager.RenderCaches]] alongside the render/mouse-hit-testing caches
  * `RenderCachesIsolationSpec` already covers -- so this pins the same acceptance criterion for this cache
  * specifically: two independently constructed instances share no state, including under concurrent access, and a
  * single instance still behaves like the old cache did for its own repeated calls.
  */
class MarkdownPreviewCacheIsolationSpec extends AnyFlatSpec with Matchers:

  private val font = Font(Font.SANS_SERIF, Font.PLAIN, 12)

  "Two independently constructed MarkdownPreviewCache instances" should
    "never let one answer a lookup the other cached" in {
      val cacheA = MarkdownPreviewCache()
      val cacheB = MarkdownPreviewCache()

      val first  = MarkdownDocumentPreview.renderHtmlFragment("# Cached", "doc.md", cacheA)
      val second = MarkdownDocumentPreview.renderHtmlFragment("# Cached", "doc.md", cacheB)

      // Same input, but a fresh render on `cacheB` -- a shared JVM-wide cache would have returned `first` unchanged.
      second should not be theSameInstanceAs(first)
      second shouldBe first
    }

  it should "keep image renders independent per instance too" in {
    val cacheA = MarkdownPreviewCache()
    val cacheB = MarkdownPreviewCache()

    val first = MarkdownDocumentPreview.renderImage(
      source = "# Cached",
      title = "doc.md",
      widthPx = 120,
      heightPx = 80,
      theme = Theme.default,
      font = font,
      cache = cacheA
    )
    val second = MarkdownDocumentPreview.renderImage(
      source = "# Cached",
      title = "doc.md",
      widthPx = 120,
      heightPx = 80,
      theme = Theme.default,
      font = font,
      cache = cacheB
    )

    second should not be theSameInstanceAs(first)
  }

  "A single MarkdownPreviewCache instance" should "still reuse a rendered HTML fragment across repeated calls" in {
    val cache  = MarkdownPreviewCache()
    val source = "# Cached\n\nBody"

    val first  = MarkdownDocumentPreview.renderHtmlFragment(source, "doc.md", cache)
    val second = MarkdownDocumentPreview.renderHtmlFragment(source, "doc.md", cache)

    second should be theSameInstanceAs first
  }

  it should "never let one manager's cached render answer for another's under concurrent access" in {
    def renderAndCheck(cache: MarkdownPreviewCache, tag: Int): IO[Unit] = IO {
      val source = s"# Doc $tag"
      val first  = MarkdownDocumentPreview.renderHtmlFragment(source, s"doc-$tag.md", cache)
      val second = MarkdownDocumentPreview.renderHtmlFragment(source, s"doc-$tag.md", cache)
      second should be theSameInstanceAs first
      second should include(s"Doc $tag")
    }

    val cacheA = MarkdownPreviewCache()
    val cacheB = MarkdownPreviewCache()

    val program =
      (0 until 40).toList.parTraverseN(16) { i =>
        if i % 2 == 0 then renderAndCheck(cacheA, i) else renderAndCheck(cacheB, i)
      }

    program.unsafeRunSync()
  }
end MarkdownPreviewCacheIsolationSpec
