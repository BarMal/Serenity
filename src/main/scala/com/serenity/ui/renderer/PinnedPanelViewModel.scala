package com.serenity.ui.renderer

import com.serenity.document.DocumentNavigation
import com.serenity.state.models.*
import com.serenity.ui.layout.*

final case class TextPanelRow(
    plainText: String,
    selected: Boolean = false
)

final case class TextPanelView(
    rect: LayoutRect,
    contentRect: Option[LayoutRect] = None,
    title: String,
    rows: List[TextPanelRow],
    header: Option[TextPanelRow] = None,
    footer: Option[TextPanelRow] = None,
    surfaceId: Option[SurfaceId] = None
):
  def lines: List[String] = (header.toList ++ rows ++ footer.toList).map(_.plainText)

  def resolvedContentRect: LayoutRect =
    contentRect.getOrElse(SurfaceFrameLayout(rect).contentRect)

  def titleRect: LayoutRect =
    val content = resolvedContentRect
    LayoutRect(content.x, rect.y, content.width, 1)

  def contentRowSlots: List[SurfaceContentRowSlot] =
    SurfaceFrameLayout.contentRowSlotsFor(
      resolvedContentRect,
      rows.length,
      header.nonEmpty,
      footer.nonEmpty
    )

object PinnedPanelViewModel:

  /** Every docked surface's panel view, including the currently-expanded one -- `state.pinnedSurfaces` (issue #817)
    * already covers it: expansion is a `Layout.maximizedWorkspaceNodeId` overlay on an otherwise still-docked surface,
    * not a separate presentation that would exclude it, so no second, expanded-only surface list is needed here.
    */
  def fromState(state: AppState, layout: CalculatedLayout): List[TextPanelView] =
    state.pinnedSurfaces.flatMap { surface =>
      EditorLayoutContract.panelRectFor(surface, state, layout).map(rect => resolve(surface, rect, Some(state)))
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
            .flatMap(_.persisted.buffers.get(bufferId))
            .map(_.document.content.collect())
            .getOrElse("")
          SurfaceContentResolver.resolveMarkdownPreview(title, content, rect, SurfaceRenderMode.Pinned)
        case SurfaceContent.Outline(symbols, activeLocation) =>
          SurfaceContentResolver.resolve(
            SurfaceContent.Outline(symbols, activeSymbolLocation(symbols, activeLocation, state)),
            rect,
            SurfaceRenderMode.Pinned
          )
        case SurfaceContent.Comments(symbols, activeLocation) =>
          SurfaceContentResolver.resolve(
            SurfaceContent.Comments(symbols, activeSymbolLocation(symbols, activeLocation, state)),
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
      footer = resolved.footer.map(toPanelRow),
      surfaceId = Some(surface.id)
    )

  private def toPanelRow(row: OverlayRow): TextPanelRow =
    TextPanelRow(
      plainText = row.plainText,
      selected = row.selected
    )

  private def activeSymbolLocation(
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
