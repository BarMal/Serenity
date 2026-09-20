package com.serenity.ui.layout

/** Shared row-geometry and hit-region-plan helpers for the flat, per-row pinned/floating surface compositions (issue
  * #819 slices 4-5: `OutlineSurfaceComposition`, `DiagnosticsSurfaceComposition`, `DirectoryTreeSurfaceComposition`,
  * `CommentsSurfaceComposition`, `CommentLensSurfaceComposition`) -- each resolves one row list into paint boxes at
  * row-height slots inside `bounds`, then clips them to it. Pulled out once this trio was duplicated identically across
  * all five objects, so a future fix to row/clip geometry is made in one place instead of five.
  *
  * `CommentLensSurfaceComposition` uses `rowRect`/`logicalRect` but not `planWithRowHits`: its lens body is one
  * click-anywhere target rather than per-row hit targets, so it keeps its own no-hits `plan`.
  */
private[layout] trait RowCompositionSupport:

  protected def logicalRect(x: Int, y: Int, width: Int, height: Int): LogicalPixelRect =
    LogicalPixelRect(x.toDouble, y.toDouble, width.toDouble, height.toDouble)

  protected def rowRect(bounds: LogicalPixelRect, row: Int): LogicalPixelRect =
    LogicalPixelRect(
      bounds.x,
      bounds.y + row,
      bounds.width,
      math.min(1.0, math.max(0.0, bounds.bottom - bounds.y - row))
    )

  /** Clips `boxes` to `bounds` and turns each surviving box that carries a focus id and semantic label into a hit
    * region keyed by that same id -- the shared "one row, one paint box, one hit region" plan every row-list surface
    * but `CommentLensSurfaceComposition` uses.
    */
  protected def planWithRowHits(bounds: LogicalPixelRect, boxes: List[SurfacePaintBox]): ResolvedSurfaceComposition =
    val clipped = boxes.flatMap(box => box.rect.intersection(bounds).map(rect => box.copy(rect = rect)))
    val hits = clipped.flatMap { box =>
      for
        focusId <- box.focusId
        label   <- box.semanticLabel
      yield SurfaceHitRegion(box.rect, focusId, box.actionId, label)
    }
    ResolvedSurfaceComposition(
      bounds = bounds,
      intrinsicSize = SurfaceIntrinsicSize(bounds.width, bounds.height),
      paintBoxes = clipped,
      hitRegions = hits,
      focusOrder = hits.map(_.focusId)
    )
