package com.serenity.ui.renderer

import com.serenity.document.DocumentNavigation
import com.serenity.state.models.*
import com.serenity.ui.layout.*

case class TextPanelRow(
    plainText: String,
    selected: Boolean = false
)

case class TextPanelView(
    rect: LayoutRect,
    contentRect: Option[LayoutRect] = None,
    title: String,
    rows: List[TextPanelRow],
    header: Option[TextPanelRow] = None,
    footer: Option[TextPanelRow] = None
):
  def lines: List[String] = (header.toList ++ rows ++ footer.toList).map(_.plainText)

  def resolvedContentRect: LayoutRect =
    contentRect.getOrElse(SurfaceFrameLayout(rect).contentRect)

  def contentRowSlots: List[SurfaceContentRowSlot] =
    SurfaceFrameLayout.contentRowSlotsFor(
      resolvedContentRect,
      rows.length,
      header.nonEmpty,
      footer.nonEmpty
    )

object PinnedPanelViewModel:

  def fromState(state: AppState, layout: CalculatedLayout): List[TextPanelView] =
    (state.pinnedSurfaces ++ state.uiSurfaces.filter {
      _.presentation match
        case SurfacePresentation.Expanded(_, _) => true
        case _                                  => false
    }).flatMap {
      case surface @ UiSurface(_, _, SurfacePresentation.Pinned(position, _), _) =>
        pinnedRect(surface, position, layout).map(rect => resolve(surface, rect, Some(state)))
      case surface @ UiSurface(_, _, SurfacePresentation.Expanded(_, _), _) =>
        layout.expandedPanelRect.map(rect => resolve(surface, rect, Some(state)))
      case _ =>
        None
    }

  def fromLayout(layout: CalculatedLayout, surfaces: List[UiSurface]): List[TextPanelView] =
    surfaces.flatMap {
      case surface @ UiSurface(_, _, SurfacePresentation.Pinned(position, _), _) =>
        pinnedRect(surface, position, layout).map(rect => resolve(surface, rect, None))
      case surface @ UiSurface(_, _, SurfacePresentation.Expanded(_, _), _) =>
        layout.expandedPanelRect.map(rect => resolve(surface, rect, None))
      case _ =>
        None
    }

  def resolve(surface: UiSurface, rect: LayoutRect): TextPanelView =
    resolve(surface, rect, None)

  def resolve(surface: UiSurface, rect: LayoutRect, state: AppState): TextPanelView =
    resolve(surface, rect, Some(state))

  private def resolve(surface: UiSurface, rect: LayoutRect, state: Option[AppState]): TextPanelView =
    val resolved =
      surface.content match
        case SurfaceContent.MarkdownPreview(bufferId, title) =>
          val content = state
            .flatMap(_.buffers.get(bufferId))
            .map(_.content.collect())
            .getOrElse("")
          SurfaceContentResolver.resolveMarkdownPreview(title, content, rect, SurfaceRenderMode.Pinned)
        case SurfaceContent.Outline(symbols, activeLocation) =>
          SurfaceContentResolver.resolve(
            SurfaceContent.Outline(symbols, activeOutlineLocation(symbols, activeLocation, state)),
            rect,
            SurfaceRenderMode.Pinned
          )
        case other =>
          SurfaceContentResolver.resolve(other, rect, SurfaceRenderMode.Pinned)
    TextPanelView(
      rect = rect,
      contentRect = Some(SurfaceFrameLayout.forContent(rect, surface.content).contentRect),
      title = resolved.title.getOrElse(""),
      rows = resolved.rows.map(toPanelRow),
      header = resolved.header.map(toPanelRow),
      footer = resolved.footer.map(toPanelRow)
    )

  private def toPanelRow(row: OverlayRow): TextPanelRow =
    TextPanelRow(
      plainText = row.plainText,
      selected = row.selected
    )

  private def activeOutlineLocation(
    symbols: List[Symbol],
    fallback: Option[Location],
    state: Option[AppState]
  ): Option[Location] =
    fallback.orElse {
      state
        .flatMap(_.activeCursorPosition)
        .flatMap(cursor => DocumentNavigation.currentSymbol(symbols, cursor))
        .map(_.location)
    }

  private def pinnedRect(surface: UiSurface, position: PanelPosition, layout: CalculatedLayout): Option[LayoutRect] =
    layout.pinnedSurfaceRects.get(surface.id).orElse(layout.pinnedPanelRects.get(position))
