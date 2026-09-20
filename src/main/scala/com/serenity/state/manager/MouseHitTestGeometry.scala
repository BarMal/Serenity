package com.serenity.state.manager

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*

/** Pure pixel/cell geometry shared by every mouse hit-testing module: mapping a mouse event's column/row (and, when
  * available, its precise pixel position) onto floating-surface frames and their item rows. Holds no state and calls
  * back into no capability, so every hit-testing module can depend on it without widening its own port.
  */
private[manager] object MouseHitTestGeometry:

  def floatingCellMetrics(state: AppState): CellMetrics =
    CellMetrics.fromFont(FontLoader.previewCodeFont(state.persisted.config.editorConfig.fontConfig))

  /** Multi-column e-reader layout (issue #1338, Phase 2 / slice 5): which page column a click at `xCellsFromContent`
    * (cells right of the pane's content-rect left edge) lands in, given the same [[ColumnSnapshotPlacement]]s slice 1
    * computed for painting -- so hit-testing and paint agree on where each column's band is. Each placement owns the
    * half-open band `[xOffsetCells, xOffsetCells + columnWidthCells)`; a click inside a band picks that column. The
    * `columnGap` cells between two columns are a dead-zone that belongs to neither band, so a click there (or off
    * either end of the page) resolves to the *nearest* band by distance to its interval -- clamping to the nearer
    * column's own edge rather than falling through to column 0. Ties (a click exactly midway in a gap) resolve to the
    * left column, matching left-to-right reading order. `None` only when there are no placements at all (a non-column
    * pane), where the caller keeps its single-snapshot path.
    */
  def columnPlacementForX(
    placements: Vector[ColumnSnapshotPlacement],
    xCellsFromContent: Int
  ): Option[ColumnSnapshotPlacement] =
    def distanceToBand(placement: ColumnSnapshotPlacement): Int =
      val left  = placement.xOffsetCells
      val right = placement.xOffsetCells + math.max(0, placement.columnWidthCells)
      if xCellsFromContent < left then left - xCellsFromContent
      else if xCellsFromContent >= right then xCellsFromContent - right + 1
      else 0
    placements.minByOption(distanceToBand)

  /** `visibleFloatingSurfaces`, not `floatingSurfaces`: the cursor info bar is derived per frame rather than stored,
    * and it floats over the document right where the caret is -- so reading the stored list alone let every click on
    * the bar fall through to the hidden text it was covering (#1292).
    */
  def isInsideFloatingSurface(event: MouseInputEvent, state: AppState): Boolean =
    state.runtime.viewportSize.exists { viewportSize =>
      state.visibleFloatingSurfaces.exists(insideFloatingSurface(event, state, viewportSize, _))
    }

  /** Whether `event` lands inside a single floating surface's frame -- the per-surface primitive
    * [[isInsideFloatingSurface]] applies across every floating surface, and that a caller with one specific surface
    * already in hand (e.g. the comment lens) can use directly instead of re-deriving it from `state.floatingSurfaces`.
    */
  def insideFloatingSurface(
    event: MouseInputEvent,
    state: AppState,
    viewportSize: ViewportSize,
    surface: UiSurface
  ): Boolean =
    val scene    = AuthoritativeUiScene.forState(state, viewportSize)
    val layout   = scene.calculatedLayout
    val contract = scene.editorContract
    val metrics  = floatingCellMetrics(state)
    contract.overlayRect(surface.id).exists { rect =>
      val geometry = FloatingSurfaceGeometry
        .fromCells(
          rect,
          metrics,
          borderCells = 0,
          itemCount = 0,
          hasHeader = false,
          hasFooter = false,
          itemGapRows = 0.0
        )
        .translated(
          0.0,
          FloatingSurfaceGeometry.signedRowOffsetPixels(
            layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0),
            metrics
          )
        )
      (event.pixelX, event.pixelY) match
        case (Some(pixelX), Some(pixelY)) => geometry.frame.contains(pixelX, pixelY)
        case _                            => rect.contains(event.col, event.row)
    }

  def overlayItemIndex(
    event: MouseInputEvent,
    state: AppState,
    floatingOffsetRows: Double,
    contentRect: LayoutRect,
    rowSlots: List[SurfaceContentRowSlot],
    itemCount: Int,
    selectedIndex: Int,
    hasHeader: Boolean,
    hasFooter: Boolean,
    reservedContentRows: Int = 0,
    itemGapRows: Double = 0.0,
    itemTargetRows: Int = 1
  ): Option[Int] =
    val itemWindow = SurfaceFrameLayout(contentRect, borderCells = 0).itemWindow(
      itemCount,
      selectedIndex,
      hasHeader,
      hasFooter,
      reservedContentRows,
      itemGapRows,
      itemTargetRows
    )
    val pixelSelection = for
      pixelX <- event.pixelX
      pixelY <- event.pixelY
      metrics = floatingCellMetrics(state)
      geometry = FloatingSurfaceGeometry
        .fromCells(
          contentRect,
          metrics,
          borderCells = 0,
          itemCount = itemCount,
          hasHeader = hasHeader,
          hasFooter = hasFooter,
          itemGapRows = itemGapRows,
          itemTargetRows = itemTargetRows
        )
        .translated(0.0, FloatingSurfaceGeometry.signedRowOffsetPixels(floatingOffsetRows, metrics))
      displayedIndex <- geometry.itemIndexAt(pixelX, pixelY)
      absoluteIndex  <- itemWindow.absoluteIndexAt(displayedIndex)
    yield absoluteIndex
    if event.pixelX.isDefined && event.pixelY.isDefined then pixelSelection
    else overlayDisplayedRowIndexAt(event, contentRect, rowSlots, itemTargetRows).flatMap(itemWindow.absoluteIndexAt)

  def overlayDisplayedRowIndexAt(
    event: MouseInputEvent,
    contentRect: LayoutRect,
    rowSlots: List[SurfaceContentRowSlot],
    itemTargetRows: Int = 1
  ): Option[Int] =
    val insideColumns = event.col >= contentRect.x && event.col < contentRect.right
    Option
      .when(insideColumns)(())
      .flatMap(_ =>
        rowSlots.collectFirst {
          case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), y)
              if event.row >= y && event.row < y + math.max(1, itemTargetRows) =>
            index
        }
      )
