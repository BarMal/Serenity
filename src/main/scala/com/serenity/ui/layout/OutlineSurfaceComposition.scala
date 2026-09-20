package com.serenity.ui.layout

import com.serenity.state.models.SurfaceContent

/** Declarative composition plan for the pinned/expanded outline panel (issue #819, slice 4). Mirrors
  * `DirectoryTreeSurfaceComposition`'s pattern: one resolved plan produces both the paint boxes and the hit regions,
  * from the exact same `SurfaceFrameLayout.contentRowSlotsFor` row positions `EditorLayoutContract.pinnedGeometry`
  * already derives, so painting and mouse hit-testing can never disagree about where a row sits.
  *
  * Row *content* stays sourced from `PanelContentResolver.outlineRowViews` -- the same, separately-tested row builder
  * the pre-migration plain-rows path used -- so this object owns row *position*, *clipping*, and *hit addressing* only.
  * A row is only given a hit region when `outlineRowViews` marks it addressable (`Vertical`/`Square` geometry's
  * per-symbol rows) -- the `Horizontal`/`Compact` summary rows stay non-interactive, matching
  * `PinnedPanelMouseHitTesting.pinnedOutlineMouseHitAt`'s pre-migration behaviour.
  */
object OutlineSurfaceComposition extends RowCompositionSupport:

  def forOutline(
    symbols: List[Symbol],
    activeLocation: Option[Location],
    frameRect: LayoutRect
  ): ResolvedSurfaceComposition =
    val content     = SurfaceContent.Outline(symbols, activeLocation)
    val contentRect = SurfaceFrameLayout.forContent(frameRect, content).contentRect
    val bounds      = logicalRect(contentRect.x, contentRect.y, contentRect.width, contentRect.height)
    val rowViews    = PanelContentResolver.outlineRowViews(frameRect, symbols, activeLocation)

    val slots = SurfaceFrameLayout.contentRowSlotsFor(
      contentRect,
      rowViews.size,
      hasHeader = false,
      hasFooter = false
    )

    val boxes = slots.collect {
      case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), y) if rowViews.isDefinedAt(index) =>
        toRowBox(rowViews(index), rowRect(bounds, y - contentRect.y))
    }

    planWithRowHits(bounds, boxes)

  private def toRowBox(
    view: PanelContentResolver.OutlineRowView,
    rect: LogicalPixelRect
  ): SurfacePaintBox =
    view.symbolIndex match
      case Some(index) =>
        SurfacePaintBox(
          kind = SurfacePaintKind.Text,
          rect = rect,
          text = Some(view.row.plainText),
          focusId = Some(SurfaceFocusId(s"outline-symbol-$index")),
          actionId = Some(SurfaceActionId(s"outline-symbol-$index")),
          semanticLabel = Some(view.row.plainText),
          selected = view.row.selected
        )
      case None =>
        SurfacePaintBox(
          kind = SurfacePaintKind.Text,
          rect = rect,
          text = Some(view.row.plainText),
          selected = view.row.selected
        )
