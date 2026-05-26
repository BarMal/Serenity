package com.serenity.ui.renderer

import com.serenity.state.models.{SurfacePresentation, UiSurface}
import com.serenity.ui.layout.*

case class TextPanelView(
    rect: LayoutRect,
    title: String,
    lines: List[String]
)

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
    val lines = resolved.header.toList.map(_.plainText) ++ resolved.rows.map(_.plainText) ++ resolved.footer.toList.map(_.plainText)
    TextPanelView(
      rect = rect,
      title = resolved.title.getOrElse(""),
      lines = lines
    )
