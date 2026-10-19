package com.serenity.ui.layout

import java.awt.Font

import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, RichTextState}
import com.serenity.ui.theme.{HeadingSettings, HeadingStyle}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeadingCacheKeySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font       = Font(Font.SERIF, Font.PLAIN, 14)
  private val panelWidth = 220
  private val title      = "A heading long enough to wrap once it grows"

  private val document = RichTextDocument(
    List(
      RichTextParagraph.plain(title, role = ParagraphRole.Heading(1)),
      RichTextParagraph.plain("Body.")
    )
  )

  private val buffer =
    Buffer.fromString(BufferId(1), document.plainText).copy(richText = RichTextState(richTextDocument = Some(document)))

  private def settingsWithH1(scale: Float): HeadingSettings =
    HeadingSettings(styles = HeadingStyle.defaults.updated(0, HeadingStyle.default(1).copy(sizeScale = scale)))

  private def contextWith(headings: HeadingSettings): RichTextContext =
    RichTextContext(Some(document), proseScale = 1.0f, dropCapsEnabled = true, headings = headings)

  private def wrapTitle(cache: WrappedLineCache, headings: HeadingSettings) =
    TextLayoutSnapshot.boundedVisualLinesForText(
      title,
      bufferLine = 0,
      panelWidthPx = panelWidth,
      font = font,
      wrapCache = cache,
      richText = contextWith(headings)
    )

  "The wrap cache" should "not serve a heading's rows from before its size changed" in {
    val cache = WrappedLineCache.bounded()
    val big   = settingsWithH1(4.0f)

    val first  = wrapTitle(cache, HeadingSettings.default)
    val second = wrapTitle(cache, big)
    val back   = wrapTitle(cache, HeadingSettings.default)

    second.size should be > first.size
    second.map(_.heightPx).max should be > first.map(_.heightPx).max
    back.map(_.text) shouldBe first.map(_.text)
    back.map(_.widthPx) shouldBe first.map(_.widthPx)
  }

  it should "give the same rows as an uncached wrap under the changed settings" in {
    val cache = WrappedLineCache.bounded()
    val big   = settingsWithH1(4.0f)
    wrapTitle(cache, HeadingSettings.default)

    wrapTitle(cache, big).map(row => (row.text, row.widthPx, row.heightPx)) shouldBe
      wrapTitle(WrappedLineCache.Uncached, big).map(row => (row.text, row.widthPx, row.heightPx))
  }

  "The row-count stamp" should "be the rich-text document itself while headings are the defaults" in {
    VisualLineIndexStore.stampOf(contextWith(HeadingSettings.default)) should be theSameInstanceAs document
    VisualLineIndexStore.stampOf(contextWith(HeadingSettings())) should be theSameInstanceAs document
  }

  it should "differ once the heading settings do, and stay related while they match" in {
    val custom = VisualLineIndexStore.stampOf(contextWith(settingsWithH1(4.0f)))
    val same   = VisualLineIndexStore.stampOf(contextWith(settingsWithH1(4.0f)))
    val plain  = VisualLineIndexStore.stampOf(contextWith(HeadingSettings.default))

    VisualLineIndexStore.stampsRelate(plain, custom) shouldBe VisualLineIndexStore.StampRelation.Unrelated
    VisualLineIndexStore.stampsRelate(custom, plain) shouldBe VisualLineIndexStore.StampRelation.Unrelated
    VisualLineIndexStore.stampsRelate(custom, same) shouldBe VisualLineIndexStore.StampRelation.Same
  }

  "Visual row counts" should "follow a heading size change through a shared cache" in {
    val cache = WrappedLineCache.bounded()

    def rows(headings: HeadingSettings): Int =
      VisualRowCounts.forBuffer(buffer, panelWidth, font, None, false, cache, headings = headings).rowsIn(0)

    val defaults = rows(HeadingSettings.default)
    val big      = rows(settingsWithH1(4.0f))

    big should be > defaults
    rows(HeadingSettings.default) shouldBe defaults
    rows(settingsWithH1(4.0f)) shouldBe big
  }
