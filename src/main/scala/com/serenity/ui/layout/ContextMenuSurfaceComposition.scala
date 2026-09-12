package com.serenity.ui.layout

import com.serenity.state.models.ContextMenu

/** Declarative composition plan for the editor's right-click context menu (issue #819, slice 2). Mirrors
  * `ModalSurfaceComposition`'s pattern: one resolved plan produces both the paint boxes and the hit regions, from the
  * exact same `SurfaceFrameLayout.contentRowSlotsFor` row positions the pre-migration renderer and
  * `EditorContextMenuHitTesting` previously called independently -- so painting and mouse hit-testing can no longer
  * drift apart on where a row sits.
  */
object ContextMenuSurfaceComposition:

  def forMenu(
    menu: ContextMenu,
    frameRect: LayoutRect,
    itemGapRows: Double = 0.0,
    itemTargetRows: Int = 1
  ): ResolvedSurfaceComposition =
    val content   = SurfaceFrameLayout(frameRect).contentRect
    val bounds    = logicalRect(content.x, content.y, content.width, content.height)
    val hasFooter = menu.items.nonEmpty
    val itemWindow = SurfaceFrameLayout(frameRect).itemWindow(
      itemCount = menu.items.size,
      selectedIndex = menu.selectedIndex,
      hasHeader = true,
      hasFooter = hasFooter,
      itemGapRows = itemGapRows,
      itemTargetRows = itemTargetRows
    )
    val visibleItems = itemWindow.slice(menu.items)
    val slots = SurfaceFrameLayout.contentRowSlotsFor(
      content,
      itemWindow.rowCount,
      hasHeader = true,
      hasFooter = hasFooter,
      itemGapRows = itemGapRows,
      itemTargetRows = itemTargetRows
    )

    val headerBoxes = slots.collect {
      case SurfaceContentRowSlot(SurfaceContentRowKind.Header, y) =>
        textBox(menu.title, rowRect(bounds, y - content.y))
    }

    val itemBoxes = slots.collect {
      case SurfaceContentRowSlot(SurfaceContentRowKind.Item(index), y) if visibleItems.isDefinedAt(index) =>
        val absoluteIndex = index + itemWindow.offset
        val item          = visibleItems(index)
        actionBox(
          item.label,
          SurfaceActionId(s"context-menu-item-$absoluteIndex"),
          SurfaceFocusId(s"context-menu-item-$absoluteIndex"),
          selected = absoluteIndex == menu.selectedIndex,
          rowRect(bounds, y - content.y)
        )
    }

    val footerBoxes = slots.collect {
      case SurfaceContentRowSlot(SurfaceContentRowKind.Footer, y) =>
        textBox(s"${menu.selectedIndex + 1}/${menu.items.length}", rowRect(bounds, y - content.y))
    }

    plan(bounds, headerBoxes ++ itemBoxes ++ footerBoxes)

  private def plan(bounds: LogicalPixelRect, boxes: List[SurfacePaintBox]): ResolvedSurfaceComposition =
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

  private def textBox(text: String, rect: LogicalPixelRect): SurfacePaintBox =
    SurfacePaintBox(SurfacePaintKind.Text, rect, text = Some(text), semanticLabel = Some(text))

  private def actionBox(
    label: String,
    actionId: SurfaceActionId,
    focusId: SurfaceFocusId,
    selected: Boolean,
    rect: LogicalPixelRect
  ): SurfacePaintBox =
    SurfacePaintBox(
      kind = SurfacePaintKind.ActionItem,
      rect = rect,
      text = Some(label),
      focusId = Some(focusId),
      actionId = Some(actionId),
      semanticLabel = Some(label),
      selected = selected
    )

  private def rowRect(bounds: LogicalPixelRect, row: Int): LogicalPixelRect =
    LogicalPixelRect(bounds.x, bounds.y + row, bounds.width, math.min(1.0, math.max(0.0, bounds.bottom - bounds.y - row)))

  private def logicalRect(x: Int, y: Int, width: Int, height: Int): LogicalPixelRect =
    LogicalPixelRect(x.toDouble, y.toDouble, width.toDouble, height.toDouble)
