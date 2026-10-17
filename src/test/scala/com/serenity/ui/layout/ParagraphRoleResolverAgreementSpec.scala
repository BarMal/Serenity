package com.serenity.ui.layout

import java.awt.Font

import com.serenity.MockRenderSurface
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, Buffer, BufferId, RichTextState, Viewport}
import com.serenity.ui.openings.{ChapterOpenings, EffectiveRole, OpeningContext, ParagraphRoles}
import com.serenity.ui.renderer.DropCapRenderer
import com.serenity.ui.theme.RichTextStyling
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Every site that reads a paragraph's role asks the one resolver, so a role only the resolver knows about (here, body
  * text that it declares a drop cap) is laid out, painted, spanned and dirty-tracked as that role everywhere.
  */
class ParagraphRoleResolverAgreementSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val frc  = TextLayoutSnapshot.defaultFontRenderContext()
  private val font = Font(Font.SANS_SERIF, Font.PLAIN, 13)
  private val prose =
    "The quick brown fox jumps over the lazy dog, again and again, until the row finally has to wrap somewhere. " * 3

  private def bodyDocument(): RichTextDocument =
    RichTextDocument(List(RichTextParagraph.plain(prose), RichTextParagraph.plain(prose)))

  private val bodyAsCap: ChapterOpenings.Resolver = (document, line, context) =>
    document.paragraphAt(line).map(_.role) match
      case Some(ParagraphRole.Body) => EffectiveRole.Real(ParagraphRole.dropCap(3))
      case _                        => ChapterOpenings.roleFor(document, line, context)

  private def context(resolver: ChapterOpenings.Resolver): RichTextContext =
    RichTextContext(Some(bodyDocument()), 1.0f, dropCapsEnabled = true, resolver = resolver)

  private def laidOut(resolver: ChapterOpenings.Resolver): TextLayoutSnapshot =
    val richText = context(resolver)
    val document = richText.document.getOrElse(bodyDocument())
    val buffer = Buffer
      .fromString(BufferId(4), document.plainText)
      .copy(
        richText = RichTextState(richTextDocument = Some(document)),
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleColumns = 80, visibleLines = 100)
      )
    val lines = TextLayoutSnapshot.collectVisualLines(
      buffer,
      buffer.document.content.lineCount,
      240,
      font,
      frc,
      measuredLayout = true,
      CellMetrics.fromFont(font),
      Int.MaxValue,
      richText,
      wordWrapEnabled = true,
      WrappedLineCache.Uncached
    )
    TextLayoutSnapshot(
      visualLines = lines,
      panelWidthPx = 240,
      lineHeightPx = 16,
      ascentPx = 13,
      usesMeasuredLayout = true,
      richTextDocument = Some(document),
      roles = richText.roles
    )

  "the line layout" should "reserve the glyph's width for a paragraph the resolver declares a drop cap" in {
    val inputs = LineLayout.lineLayoutInputs(
      font,
      frc,
      measuredLayout = true,
      context(bodyAsCap),
      lineIndex = 0,
      lineLength = prose.length,
      lineText = prose
    )

    inputs.paragraphRole shouldBe ParagraphRole.dropCap(3)
    inputs.glyphWidthPx should be > 0.0f
  }

  it should "indent the rows the resolver's glyph spans" in {
    val offsets = laidOut(bodyAsCap).visualLines.map(_.xOffsetPx)

    offsets.take(3).foreach(_ should be > 0.0f)
    offsets(3) shouldBe 0.0f
  }

  "glyphRowSpan" should "span the resolver's glyph" in {
    DropCapLayout.glyphRowSpan(laidOut(bodyAsCap)).get(0) shouldBe Some(0 until 3)
  }

  "the font split" should "single the first character out of a paragraph the resolver declares a drop cap" in {
    val (glyph, _) =
      RichTextStyling.dropCapSplitFontSpans(bodyDocument(), 0, 0, prose.length, 12.0f, roles = context(bodyAsCap).roles)

    glyph.map(_.text) shouldBe Some("T")
  }

  it should "leave a paragraph alone when drop caps are disabled" in {
    val capped = RichTextDocument(List(RichTextParagraph.plain(prose, role = ParagraphRole.dropCap(3))))
    val (glyph, _) =
      RichTextStyling.dropCapSplitFontSpans(capped, 0, 0, prose.length, 12.0f, roles = ParagraphRoles.standard(false))

    glyph shouldBe None
  }

  "DropCapRenderer" should "paint the glyph of a paragraph the resolver declares a drop cap" in {
    val snapshot = laidOut(bodyAsCap)
    val state    = AppState.empty
    val surface  = MockRenderSurface(400, 100)

    DropCapRenderer.adjustHomeLineDraw(
      snapshot = snapshot,
      visualLine = snapshot.visualLines(0),
      styledSegments = None,
      measured = true,
      surface = surface,
      baseFont = font,
      frc = Some(frc),
      theme = state.persisted.theme,
      xOriginPx = 0.0f,
      lineTopPx = 0,
      normalLineHeightPx = snapshot.lineHeightPx
    )

    surface.drawRunPxCalls should have size 1
  }

  "DirtyLineDiff" should "mark rows dirty when only the resolver's role for a paragraph changed" in {
    val before = laidOut(ChapterOpenings.standard)
    val after  = laidOut(bodyAsCap).copy(visualLines = before.visualLines)

    DirtyLineDiff.dirtyRows(Some(before), after) should contain(0)
  }

  it should "not mark rows dirty for a role change the toggle hides" in {
    def capped(lines: Int) =
      RichTextDocument(List(RichTextParagraph.plain(prose, role = ParagraphRole.dropCap(lines))))
    val off    = ParagraphRoles.standard(false)
    val laid   = laidOut(ChapterOpenings.standard)
    val before = laid.copy(richTextDocument = Some(capped(3)), roles = off)
    val after  = laid.copy(richTextDocument = Some(capped(2)), roles = off)

    DirtyLineDiff.dirtyRows(Some(before), after) shouldBe Set.empty[Int]
  }

  "ChapterOpenings.roleFor" should "degrade a drop cap to body text only while drop caps are disabled" in {
    val capped = RichTextDocument(List(RichTextParagraph.plain(prose, role = ParagraphRole.dropCap(3))))

    ChapterOpenings.roleFor(capped, 0, OpeningContext(true)) shouldBe EffectiveRole.Real(ParagraphRole.dropCap(3))
    ChapterOpenings.roleFor(capped, 0, OpeningContext(false)) shouldBe EffectiveRole.Real(ParagraphRole.Body)
  }

  it should "pass every other role through and read a missing paragraph as body text" in {
    val heading = RichTextDocument(List(RichTextParagraph.plain("Title", role = ParagraphRole.Heading(2))))

    ChapterOpenings.roleFor(heading, 0, OpeningContext(false)) shouldBe EffectiveRole.Real(ParagraphRole.Heading(2))
    ChapterOpenings.roleFor(heading, 5, OpeningContext(true)) shouldBe EffectiveRole.Real(ParagraphRole.Body)
  }
