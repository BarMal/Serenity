package com.serenity.ui.renderer

import com.serenity.document.DocumentNavigation
import com.serenity.markdown.MarkdownPreviewCache
import com.serenity.state.models.*
import com.serenity.ui.layout.*

final case class TextPanelRow(
    plainText: String,
    selected: Boolean = false
)

/** A docked panel's resolved paint plan. `composition` is the panel's *only* stored content representation (issue
  * #1683) -- there is no second, independently-settable `rows`/`header`/`footer` that could disagree with it and need a
  * precedence rule, the way this type used to carry both. `rows`/`header`/`footer` below are read-only views derived
  * from `composition.paintBoxes`, kept only so existing call sites (and `PinnedPanelViewModel.resolve`'s own tests) can
  * still ask "what text did this panel resolve to" without reaching into paint-box internals.
  *
  * The companion's `apply` also still accepts `rows`/`header`/`footer` (as `TextPanelRow`s) for construction: a plain
  * informational panel with no bespoke `*SurfaceComposition` of its own is built from them, via
  * [[RowsSurfaceComposition]], rather than every caller having to build a `ResolvedSurfaceComposition` by hand.
  */
final case class TextPanelView private[renderer] (
    rect: LayoutRect,
    contentRect: Option[LayoutRect],
    title: String,
    surfaceId: Option[SurfaceId],
    composition: ResolvedSurfaceComposition
):
  def rows: List[TextPanelRow] =
    composition.paintBoxes.collect { case box if box.kind == SurfacePaintKind.Text => toPanelRow(box) }

  def header: Option[TextPanelRow] =
    composition.paintBoxes.collectFirst { case box if box.kind == SurfacePaintKind.Heading => toPanelRow(box) }

  def footer: Option[TextPanelRow] =
    composition.paintBoxes.collectFirst { case box if box.kind == SurfacePaintKind.Footer => toPanelRow(box) }

  def lines: List[String] = (header.toList ++ rows ++ footer.toList).map(_.plainText)

  def contentRowSlots: List[SurfaceContentRowSlot] = RowsSurfaceComposition.contentRowSlots(composition)

  def resolvedContentRect: LayoutRect =
    contentRect.getOrElse(SurfaceFrameLayout(rect).contentRect)

  def titleRect: LayoutRect =
    val content = resolvedContentRect
    LayoutRect(content.x, rect.y, content.width, 1)

  private def toPanelRow(box: SurfacePaintBox): TextPanelRow =
    TextPanelRow(plainText = box.text.getOrElse(""), selected = box.selected)

object TextPanelView:

  /** `composition`, when given, wins outright -- there is no dual-path precedence rule to apply, only a choice of which
    * single composition to store: a caller building a panel for content with no bespoke `*SurfaceComposition` passes
    * `rows`/`header`/`footer` instead, and this builds the generic one via [[RowsSurfaceComposition]].
    */
  def apply(
    rect: LayoutRect,
    contentRect: Option[LayoutRect] = None,
    title: String,
    rows: List[TextPanelRow] = Nil,
    header: Option[TextPanelRow] = None,
    footer: Option[TextPanelRow] = None,
    surfaceId: Option[SurfaceId] = None,
    composition: Option[ResolvedSurfaceComposition] = None
  ): TextPanelView =
    val resolvedComposition = composition.getOrElse(
      RowsSurfaceComposition.forResolved(
        ResolvedSurfaceContent(
          header = header.map(fromPanelRow),
          rows = rows.map(fromPanelRow),
          footer = footer.map(fromPanelRow)
        ),
        frameRect = rect,
        contentRectOverride = contentRect
      )
    )
    new TextPanelView(rect, contentRect, title, surfaceId, resolvedComposition)

  private def fromPanelRow(row: TextPanelRow): OverlayRow =
    OverlayRow(plainText = row.plainText, selected = row.selected)

object PinnedPanelViewModel:

  /** Every docked surface's panel view, including the currently-expanded one -- `state.pinnedSurfaces` (issue #817)
    * already covers it: expansion is a `Layout.maximizedWorkspaceNodeId` overlay on an otherwise still-docked surface,
    * not a separate presentation that would exclude it, so no second, expanded-only surface list is needed here.
    */
  def fromState(state: AppState, layout: CalculatedLayout): List[TextPanelView] =
    state.pinnedSurfaces.flatMap { surface =>
      EditorLayoutContract
        .panelRectFor(surface, state, layout)
        .map(rect => resolve(surface, rect, Some(state), MarkdownPreviewCache()))
    }

  def resolve(
    surface: UiSurface,
    rect: LayoutRect,
    cache: MarkdownPreviewCache = MarkdownPreviewCache()
  ): TextPanelView =
    resolve(surface, rect, None, cache)

  def resolve(
    surface: UiSurface,
    rect: LayoutRect,
    state: AppState,
    cache: MarkdownPreviewCache
  ): TextPanelView =
    resolve(surface, rect, Some(state), cache)

  private def resolve(
    surface: UiSurface,
    rect: LayoutRect,
    state: Option[AppState],
    cache: MarkdownPreviewCache
  ): TextPanelView =
    val resolved =
      surface.content match
        case SurfaceContent.MarkdownPreview(bufferId, title) =>
          val content = state.flatMap(_.persisted.buffers.get(bufferId)).map(_.document.content)
          SurfaceContentResolver.resolveBufferMarkdownPreview(title, content, rect, SurfaceRenderMode.Pinned, cache)
        case SurfaceContent.Outline(symbols, activeLocation, scroll) =>
          SurfaceContentResolver.resolve(
            SurfaceContent.Outline(symbols, activeSymbolLocation(symbols, activeLocation, state), scroll),
            rect,
            SurfaceRenderMode.Pinned
          )
        case SurfaceContent.Comments(symbols, activeLocation, scroll) =>
          SurfaceContentResolver.resolve(
            SurfaceContent.Comments(symbols, activeSymbolLocation(symbols, activeLocation, state), scroll),
            rect,
            SurfaceRenderMode.Pinned
          )
        case other =>
          SurfaceContentResolver.resolve(other, rect, SurfaceRenderMode.Pinned)
    TextPanelView(
      rect = rect,
      contentRect = Some(SurfaceFrameLayout.forContent(rect, surface.content).contentRect),
      title = resolved.title.getOrElse(""),
      surfaceId = Some(surface.id),
      composition = Some(compositionFor(surface, rect, resolved, state))
    )

  /** Every docked content kind's composition, bespoke where one already exists, otherwise the generic
    * [[RowsSurfaceComposition]] built from `resolved` -- so a pinned panel is always painted from one composed plan,
    * never a plain-rows fallback with its own, separately derived geometry (issue #1683).
    */
  private def compositionFor(
    surface: UiSurface,
    rect: LayoutRect,
    resolved: ResolvedSurfaceContent,
    state: Option[AppState]
  ): ResolvedSurfaceComposition =
    surface.content match
      case SurfaceContent.DirectoryTree(tree, selectedPath, scroll) =>
        DirectoryTreeSurfaceComposition.forTree(tree, selectedPath, scroll, rect)
      case SurfaceContent.Outline(symbols, activeLocation, scroll) =>
        OutlineSurfaceComposition.forOutline(
          symbols,
          activeSymbolLocation(symbols, activeLocation, state),
          rect,
          scroll
        )
      case SurfaceContent.Diagnostics(issues, activeLocation, scroll) =>
        DiagnosticsSurfaceComposition.forDiagnostics(issues, activeLocation, rect, scroll)
      case SurfaceContent.Comments(symbols, activeLocation, scroll) =>
        CommentsSurfaceComposition.forComments(
          symbols,
          activeSymbolLocation(symbols, activeLocation, state),
          rect,
          scroll
        )
      case content =>
        RowsSurfaceComposition.forResolved(resolved, rect, SurfaceFrameLayout.borderCellsFor(content))

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
