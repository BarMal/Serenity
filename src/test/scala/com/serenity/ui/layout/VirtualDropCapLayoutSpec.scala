package com.serenity.ui.layout

import java.awt.Font

import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, RichTextState, Viewport}
import com.serenity.ui.openings.{HandDropCapRule, LetterMode, OpeningRules, OpeningSettings, VirtualDropCaps}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A virtual drop cap goes through the layout a hand-made one does: its rows are inset beside the glyph, a short
  * paragraph reserves the glyph's height, and the glyph's rows are one span.
  */
class VirtualDropCapLayoutSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val frc  = TextLayoutSnapshot.defaultFontRenderContext()
  private val font = Font(Font.SANS_SERIF, Font.PLAIN, 13)
  private val prose =
    "The quick brown fox jumps over the lazy dog, again and again, until the row finally has to wrap somewhere. " * 3

  private val on = OpeningSettings(enabled = true)

  private def document(opening: RichTextParagraph = RichTextParagraph.plain(prose)): RichTextDocument =
    RichTextDocument(
      List(
        RichTextParagraph.plain("Chapter One", role = ParagraphRole.Heading(1)),
        opening,
        RichTextParagraph.plain(prose)
      )
    )

  private def buffer(document: RichTextDocument): Buffer =
    Buffer
      .fromString(BufferId(7), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 100)
      )

  private def snapshot(
    document: RichTextDocument,
    settings: OpeningSettings,
    dropCapsEnabled: Boolean = true
  ): TextLayoutSnapshot =
    TextLayoutSnapshot.fromBuffer(
      buffer(document),
      240,
      font,
      frc,
      dropCapsEnabled = dropCapsEnabled,
      openings = settings
    )

  private def homeRowOfOpening(snapshot: TextLayoutSnapshot): Int =
    snapshot.visualLines.indexWhere(line => line.bufferLine == 1 && line.startColumn == 0)

  "An opening with a virtual drop cap" should "span two rows from the opening's first row" in {
    val laidOut = snapshot(document(), on)
    val home    = homeRowOfOpening(laidOut)

    DropCapLayout.glyphRowSpan(laidOut) shouldBe Map(home -> (home until home + VirtualDropCaps.OpeningLines))
  }

  it should "inset exactly the rows beside the glyph" in {
    val laidOut = snapshot(document(), on)
    val home    = homeRowOfOpening(laidOut)
    val offsets = laidOut.visualLines.map(_.xOffsetPx)

    offsets.slice(home, home + VirtualDropCaps.OpeningLines).foreach(_ should be > 0.0f)
    offsets(home + VirtualDropCaps.OpeningLines) shouldBe 0.0f
    offsets(home - 1) shouldBe 0.0f
  }

  it should "close a later row of the glyph over the whole span" in {
    val laidOut = snapshot(document(), on)
    val home    = homeRowOfOpening(laidOut)

    DropCapLayout.closedOverGlyphs(Set(home + 1), laidOut) shouldBe (home to home + 1).toSet
  }

  it should "lay out exactly as the same paragraph with a hand-made drop cap does" in {
    val virtual = snapshot(document(), on)
    val hand =
      snapshot(
        document(RichTextParagraph.plain(prose, role = ParagraphRole.dropCap(VirtualDropCaps.OpeningLines))),
        OpeningSettings.default
      )

    virtual.visualLines shouldBe hand.visualLines
    DropCapLayout.glyphRowSpan(virtual) shouldBe DropCapLayout.glyphRowSpan(hand)
  }

  it should "reserve the glyph's full height when the paragraph is shorter than the glyph" in {
    val laidOut = snapshot(document(RichTextParagraph.plain("Short opening.")), on)
    val home    = homeRowOfOpening(laidOut)
    val rows    = laidOut.visualLines.filter(_.bufferLine == 1)

    rows should have size 1
    rows
      .map(row => row.heightPx + row.reservedBelowPx)
      .sum should be >= VirtualDropCaps.OpeningLines * laidOut.lineHeightPx
    DropCapLayout.glyphRowSpan(laidOut).keySet shouldBe Set(home)
  }

  it should "leave the buffer's text alone" in {
    val text = document().plainText

    buffer(document()).document.content.toString shouldBe text
    snapshot(document(), on).richTextDocument.map(_.plainText) shouldBe Some(text)
  }

  "With openings off, layout" should "be exactly what it was" in {
    val before = snapshot(document(), OpeningSettings.default)

    List(
      on.copy(enabled = false),
      on.copy(enabled = false, letterMode = LetterMode.Raised),
      on.copy(enabled = false, letterMode = LetterMode.Hybrid),
      on.copy(enabled = false, smallCapsFirstLine = true, letterMode = LetterMode.Raised)
    ).foreach(settings => snapshot(document(), settings).visualLines shouldBe before.visualLines)
    DropCapLayout.glyphRowSpan(before) shouldBe empty
  }

  "A virtual drop cap" should "give way to the toggle that disables drop caps" in {
    val off = snapshot(document(), on, dropCapsEnabled = false)

    off.visualLines shouldBe snapshot(document(), OpeningSettings.default, dropCapsEnabled = false).visualLines
    DropCapLayout.glyphRowSpan(off) shouldBe empty
  }

  "A hand-made drop cap on an opening" should "keep its own span under hand and lose it to the opening under opening" in {
    val handMade = document(RichTextParagraph.plain(prose, role = ParagraphRole.dropCap(5)))
    def span(rule: HandDropCapRule): Int =
      val laidOut = snapshot(handMade, on.copy(rules = OpeningRules(handDropCap = rule)))
      DropCapLayout.glyphRowSpan(laidOut).values.map(_.size).sum

    span(HandDropCapRule.HandWins) shouldBe 5
    span(HandDropCapRule.HandPlusSmallCaps) shouldBe 5
    span(HandDropCapRule.OpeningWins) shouldBe VirtualDropCaps.OpeningLines
  }

  "The visual row counts" should "agree with the rows the snapshot lays out, whichever cache holds them" in {
    val doc = document()
    def counted(settings: OpeningSettings, cache: WrappedLineCache): Int =
      VisualRowCounts
        .forBuffer(buffer(doc), 240, font, None, forceCellLayout = false, cache, openings = settings)
        .rowsBetween(0, 3)

    val cache = WrappedLineCache.bounded()
    List(WrappedLineCache.Uncached, cache).foreach { wrapCache =>
      counted(on, wrapCache) shouldBe snapshot(doc, on).visualLines.size
    }
    counted(OpeningSettings.default, cache) shouldBe snapshot(doc, OpeningSettings.default).visualLines.size
    counted(on, cache) shouldBe snapshot(doc, on).visualLines.size
  }

  "The row-count stamp" should "be the document itself at the default gap beside drop caps, and differ from it at any other" in {
    val doc = document()
    def stamp(gapEm: Float) =
      VisualLineIndexStore.stampOf(RichTextContext(Some(doc), 1.0f, dropCapsEnabled = true, dropCapGapEm = gapEm))

    stamp(DropCapLayout.DefaultGapEm) should be theSameInstanceAs doc
    VisualLineIndexStore.stampsRelate(stamp(1.5f), stamp(1.5f)) shouldBe VisualLineIndexStore.StampRelation.Same
    VisualLineIndexStore.stampsRelate(stamp(1.5f), stamp(0.25f)) shouldBe VisualLineIndexStore.StampRelation.Unrelated
    VisualLineIndexStore.stampsRelate(stamp(DropCapLayout.DefaultGapEm), stamp(1.5f)) shouldBe
      VisualLineIndexStore.StampRelation.Unrelated
  }

  "The visual row counts" should "follow a change of the gap beside drop caps through a warm cache" in {
    val doc   = document(RichTextParagraph.plain(prose, role = ParagraphRole.dropCap(VirtualDropCaps.OpeningLines)))
    val cache = WrappedLineCache.bounded()
    def counted(gapEm: Float): Int =
      VisualRowCounts
        .forBuffer(buffer(doc), 240, font, None, forceCellLayout = false, cache, dropCapGapEm = gapEm)
        .rowsBetween(0, 3)
    def laidOut(gapEm: Float): Int =
      TextLayoutSnapshot.fromBuffer(buffer(doc), 240, font, frc, dropCapGapEm = gapEm).visualLines.size

    List(0.0f, 2.0f, 0.0f, 0.5f, 2.0f).foreach(gapEm => counted(gapEm) shouldBe laidOut(gapEm))
  }
