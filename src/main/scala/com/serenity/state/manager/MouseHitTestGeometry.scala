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
