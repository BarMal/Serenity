package com.serenity.ui.layout

import java.nio.file.Path

import com.serenity.state.models.SurfaceContent

/** Declarative composition plan for the pinned/expanded directory tree panel (issue #819, slice 4). Mirrors
  * `ContextMenuSurfaceComposition`'s pattern: one resolved plan produces both the paint boxes and the hit regions, from
  * the exact same `SurfaceFrameLayout.contentRowSlotsFor` row positions `EditorLayoutContract.pinnedGeometry` already
  * derives, so painting and mouse hit-testing can never disagree about where a row sits.
  *
  * Row *content* stays sourced from `PanelContentResolver.directoryTreeRowViews` -- the same, separately-tested row
  * builder the pre-migration plain-rows path used -- so this object owns row *position*, *clipping*, and *hit
  * addressing* only. Each row's hit region is addressed by the filesystem path it represents (its `SurfaceActionId`),
  * rather than by row index, since the visible row set is a windowed, depth-indented view over the tree rather than a
  * flat, stably-indexed item list.
  */
object DirectoryTreeSurfaceComposition extends RowCompositionSupport:

  def forTree(
    tree: DirectoryTreeData,
    selectedPath: Option[Path],
    frameRect: LayoutRect
  ): ResolvedSurfaceComposition =
    val content     = SurfaceContent.DirectoryTree(tree, selectedPath)
    val contentRect = SurfaceFrameLayout.forContent(frameRect, content).contentRect
    val bounds      = logicalRect(contentRect.x, contentRect.y, contentRect.width, contentRect.height)
    val rowViews    = PanelContentResolver.directoryTreeRowViews(frameRect, tree, selectedPath)

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
    view: PanelContentResolver.DirectoryTreeRowView,
    rect: LogicalPixelRect
  ): SurfacePaintBox =
    val pathText = view.path.toString
    SurfacePaintBox(
      kind = SurfacePaintKind.Text,
      rect = rect,
      text = Some(view.row.plainText),
      focusId = Some(SurfaceFocusId(s"directory-tree-row-$pathText")),
      actionId = Some(SurfaceActionId(pathText)),
      semanticLabel = Some(view.row.plainText),
      selected = view.row.selected
    )
