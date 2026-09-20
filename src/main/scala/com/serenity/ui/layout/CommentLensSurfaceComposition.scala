package com.serenity.ui.layout

import com.serenity.state.models.{CommentLensState, SurfaceContent}

/** Declarative composition plan for the floating comment lens (issue #819, slice 3) -- the read-only/editable draft
  * body opened by clicking a highlighted comment range. Mirrors `ContextMenuSurfaceComposition`'s
  * `frameHeight`/`forLens` split.
  *
  * Row *content* stays sourced from `SurfaceContentResolver.commentLensRows` -- the same, separately-tested row builder
  * the pre-migration plain-rows path used -- so this object owns row *position* and *frame sizing* only.
  *
  * Unlike the other migrated surfaces, this composition emits no hit regions: the lens body is one click-anywhere
  * target (`CommentLensMouseHitTesting.handleCommentLensMouseClick` flips a read-only lens to editable on any click
  * inside its bounds), not a set of per-row targets, so there is nothing for `hitAt` to distinguish between rows for.
  */
object CommentLensSurfaceComposition extends RowCompositionSupport:

  private val Title = "comment"

  /** The comment lens's frame height: one header row plus one row per draft line, with the pre-migration [4, 8] clamp
    * preserved exactly (`FloatingSurfaceLayout`'s `CommentLens` case used to compute this same value inline).
    */
  def frameHeight(lens: CommentLensState): Int =
    math.max(
      4,
      math.min(
        8,
        SurfaceFrameLayout.frameHeightForItemRows(
          itemRows = SurfaceContentResolver.commentLensRows(lens).length,
          hasHeader = true,
          hasFooter = false
        )
      )
    )

  def forLens(lens: CommentLensState, frameRect: LayoutRect): ResolvedSurfaceComposition =
    val content     = SurfaceContent.CommentLens(lens)
    val borderCells = SurfaceFrameLayout.borderCellsFor(content)
    val contentRect = SurfaceFrameLayout(frameRect, borderCells).contentRect
    val bounds      = logicalRect(contentRect.x, contentRect.y, contentRect.width, contentRect.height)
    val rows        = SurfaceContentResolver.commentLensRows(lens)

    val slots = SurfaceFrameLayout.contentRowSlotsFor(
      contentRect,
      rows.size,
      hasHeader = true,
      hasFooter = false
    )

    val headerBoxes = slots.collect {
      case SurfaceContentRowSlot(SurfaceContentRowKind.Header, y) =>
        textBox(Title, rowRect(bounds, y - contentRect.y))
    }

    val rowBoxes = slots.collect {
      case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), y) if rows.isDefinedAt(index) =>
        toRowBox(rows(index), rowRect(bounds, y - contentRect.y))
    }

    plan(bounds, headerBoxes ++ rowBoxes)

  private def plan(bounds: LogicalPixelRect, boxes: List[SurfacePaintBox]): ResolvedSurfaceComposition =
    val clipped = boxes.flatMap(box => box.rect.intersection(bounds).map(rect => box.copy(rect = rect)))
    ResolvedSurfaceComposition(
      bounds = bounds,
      intrinsicSize = SurfaceIntrinsicSize(bounds.width, bounds.height),
      paintBoxes = clipped,
      hitRegions = Nil,
      focusOrder = Nil
    )

  private def textBox(text: String, rect: LogicalPixelRect): SurfacePaintBox =
    SurfacePaintBox(SurfacePaintKind.Text, rect, text = Some(text))

  private def toRowBox(row: OverlayRow, rect: LogicalPixelRect): SurfacePaintBox =
    SurfacePaintBox(
      kind = SurfacePaintKind.Text,
      rect = rect,
      text = Some(row.plainText),
      selected = row.selected,
      cursorOffset = row.cursorColumn,
      segments = row.segments,
      layout = row.layout match
        case OverlayRowLayout.Distributed     => SurfacePaintLayout.Distributed
        case OverlayRowLayout.Plain           => SurfacePaintLayout.Plain
        case OverlayRowLayout.Split           => SurfacePaintLayout.Split
        case OverlayRowLayout.Columns         => SurfacePaintLayout.Columns
        case OverlayRowLayout.PriorityColumns => SurfacePaintLayout.Plain
    )
