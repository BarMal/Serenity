package com.serenity.ui.layout

import java.awt.Font
import java.awt.font.FontRenderContext

/** Stable keyboard-focus identity for an interactive surface primitive. */
final case class SurfaceFocusId(value: String)

/** Stable action identity for a selectable surface item. */
final case class SurfaceActionId(value: String)

/** Device-independent intrinsic size produced by the pure surface layout pass. */
final case class SurfaceIntrinsicSize(width: Double, height: Double)

/** Immutable text metrics used to resolve proportional surface content in logical pixels. */
final case class SurfaceCompositionMetrics(
    font: Font,
    fontRenderContext: FontRenderContext,
    lineHeightPx: Double
):

  /** Measure text with the same proportional shaping helper used by editor and overlay rendering. */
  def textWidth(text: String): Double =
    TextLayoutSnapshot.caretXsForText(text, font, fontRenderContext).lastOption.getOrElse(0.0f).toDouble

object SurfaceCompositionMetrics:

  /** Build immutable composition metrics from a UI font and render context. */
  def fromFont(font: Font, fontRenderContext: FontRenderContext): SurfaceCompositionMetrics =
    val lineHeight = math.max(1.0, font.getLineMetrics("Mg", fontRenderContext).getHeight.toDouble)
    SurfaceCompositionMetrics(font, fontRenderContext, lineHeight)

/** Paint operation emitted by the declarative surface layout. */
enum SurfacePaintKind:
  case Text
  case Spacer
  case TextInput
  case ActionItem
  case Heading
  // Chrome rows a generic, non-bespoke composition (`RowsSurfaceComposition`, issue #1683) tags distinctly from a
  // plain `Text` item row, mirroring `Heading`'s existing role for a header row.
  case Footer
  case KeyHint

/** Layout strategy for text carried by a composed paint box. */
enum SurfacePaintLayout:
  case Plain
  case Split
  case Inline
  case Columns
  // Multiple child boxes in one row, each carrying its own `allocatedWidth` on its `SurfacePaintBox.segments` entry,
  // with an inter-segment `│` glyph drawn where `trailingSeparator` is set -- owned by the gap between segments, not
  // by either one. Mirrors `OverlayRowLayout.Distributed` (issue #819 prep).
  case Distributed

/** One clipped paint box. Interactive boxes carry the same identity and rectangle as their hit region. */
final case class SurfacePaintBox(
    kind: SurfacePaintKind,
    rect: LogicalPixelRect,
    text: Option[String] = None,
    focusId: Option[SurfaceFocusId] = None,
    actionId: Option[SurfaceActionId] = None,
    semanticLabel: Option[String] = None,
    selected: Boolean = false,
    cursorOffset: Option[Int] = None,
    segments: List[OverlaySegment] = Nil,
    layout: SurfacePaintLayout = SurfacePaintLayout.Plain
)

/** One semantic pointer target emitted from the same box used for painting. */
final case class SurfaceHitRegion(
    rect: LogicalPixelRect,
    focusId: SurfaceFocusId,
    actionId: Option[SurfaceActionId],
    semanticLabel: String
)

/** Pure, immutable surface layout output shared by rendering, focus traversal, and pointer hit testing. */
final case class ResolvedSurfaceComposition(
    bounds: LogicalPixelRect,
    intrinsicSize: SurfaceIntrinsicSize,
    paintBoxes: List[SurfacePaintBox],
    hitRegions: List[SurfaceHitRegion],
    focusOrder: List[SurfaceFocusId],
    // Set only by `RowsSurfaceComposition.forResolved` (issue #1683). `RowsSurfaceComposition.contentRowSlots`
    // recovers row slots by assuming every non-chrome paint box is a plain `Item`, which holds only for that generic
    // adapter's own tagging -- a bespoke composition (e.g. `CommandRunnerSurfaceComposition`, `OutlineSurfaceComposition`)
    // paints real rows through its own paint-box shape, and reverse-engineering slots from those would misreport
    // content that was never meant to be read back that way.
    builtByRowsAdapter: Boolean = false
):

  /** Resolve the topmost semantic hit at a logical-pixel position. */
  def hitAt(pixelX: Double, pixelY: Double): Option[SurfaceHitRegion] =
    hitRegions.reverse.find(_.rect.contains(pixelX, pixelY))
