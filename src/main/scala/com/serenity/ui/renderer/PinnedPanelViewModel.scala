package com.serenity.ui.renderer

import com.serenity.state.models.{SurfacePresentation, UiSurface}
import com.serenity.ui.layout.*

case class TextPanelRow(
    plainText: String,
    selected: Boolean = false
)

case class TextPanelView(
    rect: LayoutRect,
    title: String,
    rows: List[TextPanelRow]
):
  def lines: List[String] = rows.map(_.plainText)

object PinnedPanelViewModel:

  def fromLayout(layout: CalculatedLayout, surfaces: List[UiSurface]): List[TextPanelView] =
    surfaces.flatMap {
      case surface @ UiSurface(_, _, SurfacePresentation.Pinned(position, _), _) =>
        layout.pinnedPanelRects.get(position).map(rect => resolve(surface, rect))
      case _ =>
        None
    }

  def resolve(surface: UiSurface, rect: LayoutRect): TextPanelView =
    val resolved = SurfaceContentResolver.resolve(surface.content, rect, SurfaceRenderMode.Pinned)
    val rows = resolved.header.toList.map(toPanelRow) ++ resolved.rows.map(toPanelRow) ++ resolved.footer.toList.map(toPanelRow)
    TextPanelView(
      rect = rect,
      title = resolved.title.getOrElse(""),
      rows = rows
    )

  private def toPanelRow(row: OverlayRow): TextPanelRow =
    TextPanelRow(
      plainText = row.plainText,
      selected = row.selected
    )
