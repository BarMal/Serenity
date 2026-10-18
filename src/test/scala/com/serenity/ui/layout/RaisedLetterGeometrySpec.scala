package com.serenity.ui.layout

import java.awt.Font

import com.serenity.MockRenderSurface
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, Buffer, BufferId, RichTextState, TextVisualLine, Viewport}
import com.serenity.ui.openings.{HandDropCapRule, LetterMode, OpeningRules, OpeningSettings, RaisedRowRule}
import com.serenity.ui.renderer.DropCapRenderer
import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A raised letter stands on the baseline of the opening's first line and rises one line above its capitals; a hybrid
  * letter does the same from the baseline of the line it sinks to. Either way the letter is painted whole, and
  * `raised_row` decides whether the first row grows to make room for the rising part or the letter reaches into the
  * room above it.
  */
class RaisedLetterGeometrySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val frc = TextLayoutSnapshot.defaultFontRenderContext()

  private val prose =
    "he quick brown fox jumps over the lazy dog while the afternoon sun settles over the hills and the river runs " +
      "slow beneath the old stone bridge, and nobody on the road says a word about the weather. "

  private val tolerancePx = 1.0f
  private val scales      = List("1x" -> 14.0f, "2x" -> 28.0f)
  private val rowRules    = List(RaisedRowRule.Grow, RaisedRowRule.Clamp)

  private def raised(rule: RaisedRowRule): OpeningSettings =
    OpeningSettings(enabled = true, letterMode = LetterMode.Raised, raisedRow = rule)

  private def hybrid(rule: RaisedRowRule, sink: Int): OpeningSettings =
    OpeningSettings(enabled = true, letterMode = LetterMode.Hybrid, raisedRow = rule, hybridSinkLines = sink)

  private val dropSettings = OpeningSettings(enabled = true)
  private val offSettings  = OpeningSettings.default

  final private case class Laid(snapshot: TextLayoutSnapshot, font: Font, sizePx: Float, document: RichTextDocument):

    val home: Int = snapshot.visualLines.indexWhere(line => line.bufferLine == 1 && line.startColumn == 0)

    private def extentOf(line: TextVisualLine): Int =
      line.spaceBeforePx + (if line.heightPx > 0 then line.heightPx else snapshot.lineHeightPx) +
        line.reservedBelowPx + line.spaceAfterPx

    def textTopOf(row: Int): Int =
      snapshot.visualLines.take(row).map(extentOf).sum + snapshot.visualLines(row).spaceBeforePx

    def baselineOf(row: Int): Int = textTopOf(row) + snapshot.visualLines(row).ascentPx

    def bodyCapHeightPx: Float = -font.createGlyphVector(frc, "H").getVisualBounds.getMinY.toFloat

  final private case class Drawn(
      callTopPx: Int,
      callBottomPx: Int,
      baselinePx: Float,
      inkTopPx: Float,
      inkBottomPx: Float,
      glyphWidthPx: Float,
      glyphSizePx: Float,
      clipsGlyph: Boolean
  )

  private val originPx = 400

  private def laid(
    settings: OpeningSettings,
    sizePx: Float,
    letter: String = "T",
    copies: Int = 3,
    gapEm: Float = 0.5f,
    role: ParagraphRole = ParagraphRole.Body,
    dropCapsEnabled: Boolean = true,
    forceCellLayout: Boolean = false
  ): Laid =
    val font = BundledInterFont.plain(sizePx)
    val document = RichTextDocument(
      List(
        RichTextParagraph.plain("Chapter One", role = ParagraphRole.Heading(1)),
        RichTextParagraph.plain(letter + (prose * copies), role = role),
        RichTextParagraph.plain(prose)
      )
    )
    val buffer = Buffer
      .fromString(BufferId(41), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 30)
      )
    val snapshot = TextLayoutSnapshot.fromBuffer(
      buffer,
      panelWidthPx = (sizePx * 40).toInt,
      font,
      proseScale = sizePx / 14.0f,
      dropCapsEnabled = dropCapsEnabled,
      openings = settings,
      dropCapGapEm = gapEm,
      forceCellLayout = forceCellLayout
    )
    Laid(snapshot, font, sizePx, document)

  /** What the renderer paints for the letter, read from the pixel run it asks the surface for. */
  private def drawn(laid: Laid, letter: String = "T", clipTopPx: Int = 0): Drawn =
    val theme   = AppState.empty.persisted.theme
    val surface = MockRenderSurface(2000, 1200)
    val _ = DropCapRenderer.adjustHomeLineDraw(
      snapshot = laid.snapshot,
      visualLine = laid.snapshot.visualLines(laid.home),
      styledSegments = None,
      measured = true,
      surface = surface,
      baseFont = laid.font,
      frc = Some(frc),
      theme = theme,
      glyphColour = theme.highlighted.foreground,
      xOriginPx = 0.0f,
      lineTopPx = originPx + laid.textTopOf(laid.home),
      clipTopPx = clipTopPx
    )
    val call       = surface.drawRunPxCalls.head
    val glyphStyle = surface.styleCalls.collectFirst { case surface.StyleCall("enable", style) => style }.get
    val glyphFont  = TextStyle.styledFont(laid.font, glyphStyle)
    val ink        = glyphFont.createGlyphVector(frc, letter).getVisualBounds
    val baseline   = (call.yPx + call.ascentPx).toFloat - originPx
    Drawn(
      call.yPx - originPx,
      call.yPx + call.lineHeightPx - originPx,
      baseline,
      baseline + ink.getMinY.toFloat,
      baseline + ink.getMaxY.toFloat,
      call.bgWidthPx,
      glyphFont.getSize2D,
      call.clipGlyphToRun
    )

  private def startX(line: TextVisualLine): Float = line.xForColumn(line.startColumn).get

  for
    (label, sizePx) <- scales
    rule            <- rowRules
  do
    s"A raised letter at $label with the row to $rule" should "stand on the baseline of the opening's first line" in {
      val s = laid(raised(rule), sizePx)

      drawn(s).baselinePx shouldBe s.baselineOf(s.home).toFloat +- tolerancePx
    }

    it should "rise one line above the capitals of the first line" in {
      val s = laid(raised(rule), sizePx)

      drawn(s).inkTopPx shouldBe (s.baselineOf(s.home) - s.bodyCapHeightPx - s.snapshot.lineHeightPx) +- tolerancePx
    }

    it should "be set in the glyph size of a two-line drop cap" in {
      val s    = laid(raised(rule), sizePx)
      val hand = laid(offSettings, sizePx, role = ParagraphRole.dropCap(2))

      drawn(s).glyphSizePx shouldBe drawn(hand).glyphSizePx +- 0.01f
    }

    it should "indent the first line beside the letter and no later line" in {
      val s     = laid(raised(rule), sizePx)
      val shown = drawn(s)
      val home  = s.snapshot.visualLines(s.home)

      home.xForColumn(1).get shouldBe shown.glyphWidthPx + 0.5f * sizePx +- 0.5f
      s.snapshot.visualLines(s.home + 1).xOffsetPx shouldBe 0.0f
      startX(s.snapshot.visualLines(s.home + 1)) shouldBe 0.0f +- 0.5f
      startX(s.snapshot.visualLines(s.home + 2)) shouldBe 0.0f +- 0.5f
    }

    it should "paint the whole letter, never cutting it to its run" in {
      val s     = laid(raised(rule), sizePx)
      val shown = drawn(s)

      shown.clipsGlyph shouldBe false
      shown.callBottomPx.toFloat should be >= shown.inkBottomPx - tolerancePx
    }

    it should "keep the gap the document sets between the letter and the text" in
      List(0.0f, 1.0f, 2.0f).foreach { gapEm =>
        val s = laid(raised(rule), sizePx, gapEm = gapEm)

        s.snapshot.visualLines(s.home).xForColumn(1).get shouldBe drawn(s).glyphWidthPx + gapEm * sizePx +- 0.5f
      }

    for sink <- 2 to 4 do
      s"A hybrid letter sinking $sink lines at $label with the row to $rule" should
        s"stand on the baseline of line $sink" in {
          val s = laid(hybrid(rule, sink), sizePx)

          drawn(s).baselinePx shouldBe s.baselineOf(s.home + sink - 1).toFloat +- tolerancePx
        }

      it should "rise one line above the capitals of the first line" in {
        val s = laid(hybrid(rule, sink), sizePx)

        drawn(s).inkTopPx shouldBe
          (s.baselineOf(s.home) - s.bodyCapHeightPx - s.snapshot.lineHeightPx) +- tolerancePx
      }

      it should s"be one line taller than the $sink-line drop cap" in {
        val s    = laid(hybrid(rule, sink), sizePx)
        val hand = laid(offSettings, sizePx, role = ParagraphRole.dropCap(sink))
        val cap  = drawn(s)
        val ref  = drawn(hand)

        (cap.baselinePx - cap.inkTopPx) shouldBe (ref.baselinePx - ref.inkTopPx + s.snapshot.lineHeightPx) +-
          tolerancePx
      }

      it should "start the first lines it spans at one indent and the next at the margin" in {
        val s      = laid(hybrid(rule, sink), sizePx)
        val shown  = drawn(s)
        val indent = shown.glyphWidthPx + 0.5f * sizePx
        val lines  = s.snapshot.visualLines

        lines(s.home).xForColumn(1).get shouldBe indent +- 0.5f
        (1 until sink).foreach(offset => startX(lines(s.home + offset)) shouldBe indent +- 0.5f)
        startX(lines(s.home + sink)) shouldBe 0.0f +- 0.5f
        lines(s.home - 1).xOffsetPx shouldBe 0.0f
      }

  for (label, sizePx) <- scales do
    s"With the row to grow, a raised letter at $label" should "make the first row taller by what rises above the line" in {
      val off   = laid(offSettings, sizePx)
      val s     = laid(raised(RaisedRowRule.Grow), sizePx)
      val home  = s.snapshot.visualLines(s.home)
      val plain = off.snapshot.visualLines(off.home)

      home.heightPx should be > plain.heightPx
      (home.heightPx - plain.heightPx) shouldBe (home.ascentPx - plain.ascentPx)
      s.textTopOf(s.home + 1) - off.textTopOf(off.home + 1) shouldBe (home.heightPx - plain.heightPx)
    }

    it should "keep the letter below the row above it" in {
      val s = laid(raised(RaisedRowRule.Grow), sizePx)

      drawn(s).callTopPx should be >= s.textTopOf(s.home)
      drawn(s).inkTopPx should be >= (s.textTopOf(s.home) - tolerancePx)
    }

    it should "paint the letter over the rows it covers and no row above" in {
      val s = laid(raised(RaisedRowRule.Grow), sizePx)

      DropCapLayout.glyphRowSpan(s.snapshot)(s.home).start shouldBe s.home
    }

    it should "grow the first row of a hybrid letter the same way" in {
      val off = laid(offSettings, sizePx)
      val s   = laid(hybrid(RaisedRowRule.Grow, 3), sizePx)

      s.snapshot.visualLines(s.home).heightPx should be > off.snapshot.visualLines(off.home).heightPx
      drawn(s).callTopPx should be >= s.textTopOf(s.home)
    }

    s"With the row to clamp, a raised letter at $label" should "leave the first row as tall as it was" in {
      val off = laid(offSettings, sizePx)
      val s   = laid(raised(RaisedRowRule.Clamp), sizePx)

      s.snapshot.visualLines(s.home).heightPx shouldBe off.snapshot.visualLines(off.home).heightPx
      s.snapshot.visualLines(s.home).ascentPx shouldBe off.snapshot.visualLines(off.home).ascentPx
      s.textTopOf(s.home + 1) shouldBe off.textTopOf(off.home + 1)
    }

    it should "reach into the room above the first row without a background of its own there" in {
      val s = laid(raised(RaisedRowRule.Clamp), sizePx)

      drawn(s).inkTopPx should be < s.textTopOf(s.home).toFloat
      drawn(s).callTopPx shouldBe s.textTopOf(s.home)
    }

    it should "be cut where the pane begins when it would reach above it" in {
      val s       = laid(raised(RaisedRowRule.Clamp), sizePx)
      val paneTop = s.textTopOf(s.home)
      val shown   = drawn(s, clipTopPx = originPx + paneTop)

      shown.callTopPx shouldBe paneTop
      shown.clipsGlyph shouldBe true
    }

    it should "paint the letter whole when the pane begins above its reach" in {
      val s     = laid(raised(RaisedRowRule.Clamp), sizePx)
      val shown = drawn(s, clipTopPx = originPx + s.textTopOf(s.home) - 1000)

      shown.clipsGlyph shouldBe false
    }

    it should "paint the letter over the row above it, so the two repaint together" in {
      val s    = laid(raised(RaisedRowRule.Clamp), sizePx)
      val span = DropCapLayout.glyphRowSpan(s.snapshot)(s.home)

      span.start shouldBe s.home - 1
      DropCapLayout.closedOverGlyphs(Set(s.home - 1), s.snapshot) should contain(s.home)
      DropCapLayout.closedOverGlyphs(Set(s.home), s.snapshot) should contain(s.home - 1)
    }

    it should "paint a hybrid letter over the row above it as well" in {
      val s = laid(hybrid(RaisedRowRule.Clamp, 3), sizePx)

      DropCapLayout.glyphRowSpan(s.snapshot)(s.home).start shouldBe s.home - 1
      DropCapLayout.glyphRowSpan(s.snapshot)(s.home).contains(s.home + 2) shouldBe true
    }

    s"A drop cap at $label" should "paint over the rows it spans and no row above" in {
      val s = laid(dropSettings, sizePx)

      DropCapLayout.glyphRowSpan(s.snapshot)(s.home) shouldBe (s.home until s.home + 2)
    }

  for letter <- List("Q", "J", "g") do
    s"A raised $letter" should "be painted whole in either mode" in
      rowRules.foreach { rule =>
        val s     = laid(raised(rule), 14.0f, letter = letter)
        val shown = drawn(s, letter)

        withClue(s"$rule: ") {
          shown.clipsGlyph shouldBe false
          shown.callBottomPx.toFloat should be >= shown.inkBottomPx - tolerancePx
          if rule == RaisedRowRule.Grow then shown.inkTopPx should be >= (s.textTopOf(s.home) - tolerancePx)
        }
      }

  "A raised letter on a short opening" should "reserve room for the whole letter below it" in
    rowRules.foreach { rule =>
      val s     = laid(raised(rule), 14.0f, letter = "Quiet.", copies = 0)
      val shown = drawn(s, "Q")
      val rows  = s.snapshot.visualLines.filter(_.bufferLine == 1)

      withClue(s"$rule: ") {
        rows should have size 1
        val bottom = s.textTopOf(s.home) + rows.map(row => row.heightPx + row.reservedBelowPx).sum
        shown.inkBottomPx should be <= bottom.toFloat + tolerancePx
      }
    }

  "A hand-made drop cap on an opening" should "be kept under hand, whatever letter mode is set" in
    List(raised(RaisedRowRule.Grow), hybrid(RaisedRowRule.Clamp, 3)).foreach { settings =>
      val s   = laid(settings, 14.0f, role = ParagraphRole.dropCap(4))
      val off = laid(offSettings, 14.0f, role = ParagraphRole.dropCap(4))

      s.snapshot.visualLines shouldBe off.snapshot.visualLines
    }

  it should "give way to the raised letter when the opening's wins" in {
    val rules = OpeningRules(handDropCap = HandDropCapRule.OpeningWins)
    val s     = laid(raised(RaisedRowRule.Grow).copy(rules = rules), 14.0f, role = ParagraphRole.dropCap(4))
    val plain = laid(raised(RaisedRowRule.Grow), 14.0f)

    s.snapshot.visualLines shouldBe plain.snapshot.visualLines
  }

  "A raised or hybrid letter" should "give way to the toggle that disables drop caps" in
    List(raised(RaisedRowRule.Grow), hybrid(RaisedRowRule.Clamp, 3)).foreach { settings =>
      val s   = laid(settings, 14.0f, dropCapsEnabled = false)
      val off = laid(offSettings, 14.0f, dropCapsEnabled = false)

      s.snapshot.visualLines shouldBe off.snapshot.visualLines
      DropCapLayout.glyphRowSpan(s.snapshot) shouldBe empty
    }

  it should "change nothing while openings are off" in {
    val off = laid(offSettings, 14.0f)

    List(LetterMode.Raised, LetterMode.Hybrid).foreach { mode =>
      laid(OpeningSettings(letterMode = mode), 14.0f).snapshot.visualLines shouldBe off.snapshot.visualLines
    }
  }

  it should "still highlight the first letter on the cell grid" in
    List(raised(RaisedRowRule.Grow), hybrid(RaisedRowRule.Clamp, 3)).foreach { settings =>
      val s       = laid(settings, 14.0f, forceCellLayout = true)
      val surface = MockRenderSurface(200, 60)
      DropCapRenderer.paintCellHomeLineIfNeeded(
        s.snapshot,
        AppState.empty,
        s.snapshot.visualLines(s.home),
        surface,
        0,
        3
      )

      s.snapshot.usesMeasuredLayout shouldBe false
      surface.putStringCalls.map(_.s) shouldBe List("T")
    }
