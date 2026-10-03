package com.serenity.ui.renderer

import com.serenity.config.AppConfigOps.*
import com.serenity.document.DocumentNavigation
import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** A floating surface's resolved paint plan. `composition` is the surface's *only* stored content representation (issue
  * #1683) -- there is no second, independently-settable `rows`/`header`/`footer`/`keyHintRow` that could disagree with
  * it and need a precedence rule to resolve, the way this type used to carry both (`TextOverlayRenderer` used to pick
  * `composition` over `rows` whenever it was set).
  *
  * The companion's `apply` still accepts `header`/`rows`/`footer`/`keyHintRow` for construction: a plain informational
  * surface with no bespoke `*SurfaceComposition` of its own is built from them, via [[RowsSurfaceComposition]], rather
  * than every caller having to build a `ResolvedSurfaceComposition` by hand.
  */
final case class TextOverlayView private (
    rect: LayoutRect,
    contentRect: Option[LayoutRect],
    borderCells: Int,
    alphaMultiplier: Float,
    title: Option[String],
    itemGapRows: Double,
    itemTargetRows: Int,
    verticalOffsetRows: Double,
    surfaceId: Option[SurfaceId],
    composition: Option[ResolvedSurfaceComposition]
):

  def resolvedContentRect: LayoutRect =
    contentRect.getOrElse(com.serenity.ui.layout.SurfaceFrameLayout(rect, borderCells).contentRect)

  def contentRowSlots: List[SurfaceContentRowSlot] =
    composition.fold(List.empty[SurfaceContentRowSlot])(RowsSurfaceComposition.contentRowSlots)

object TextOverlayView:

  /** `composition`, when given, wins outright -- there is no dual-path precedence rule to apply, only a choice of which
    * single composition to store: a caller building an overlay for content with no bespoke `*SurfaceComposition` passes
    * `header`/`rows`/`footer`/`keyHintRow` instead, and this builds the generic one via [[RowsSurfaceComposition]].
    */
  def apply(
    rect: LayoutRect,
    contentRect: Option[LayoutRect] = None,
    borderCells: Int = 1,
    alphaMultiplier: Float = 1.0f,
    title: Option[String] = None,
    header: Option[OverlayRow] = None,
    rows: List[OverlayRow] = Nil,
    footer: Option[OverlayRow] = None,
    keyHintRow: Option[OverlayRow] = None,
    itemGapRows: Double = 0.0,
    itemTargetRows: Int = 1,
    verticalOffsetRows: Double = 0.0,
    surfaceId: Option[SurfaceId] = None,
    composition: Option[ResolvedSurfaceComposition] = None
  ): TextOverlayView =
    val resolvedComposition = composition.orElse(
      Option.when(header.nonEmpty || rows.nonEmpty || footer.nonEmpty || keyHintRow.nonEmpty)(
        RowsSurfaceComposition.forResolved(
          ResolvedSurfaceContent(header = header, rows = rows, footer = footer, keyHintRow = keyHintRow),
          frameRect = rect,
          borderCells = borderCells,
          itemGapRows = itemGapRows,
          itemTargetRows = itemTargetRows,
          contentRectOverride = contentRect
        )
      )
    )
    new TextOverlayView(
      rect,
      contentRect,
      borderCells,
      alphaMultiplier,
      title,
      itemGapRows,
      itemTargetRows,
      verticalOffsetRows,
      surfaceId,
      resolvedComposition
    )

final case class OverlayViews(
    aboveCursor: Option[TextOverlayView] = None,
    aboveCursorStack: List[TextOverlayView] = Nil,
    belowCursor: Option[TextOverlayView] = None,
    belowCursorStack: List[TextOverlayView] = Nil,
    modal: List[TextOverlayView] = Nil,
    tabBar: Option[TextOverlayView] = None
)

object OverlayViewModel:
  private val InactiveFloatingPanelAlphaMultiplier = 0.68f

  def fromState(state: AppState, layout: CalculatedLayout): OverlayViews =
    fromState(state, layout, None)

  /** Build overlay views from the frame scene so rendering shares the snapshot used by hit testing. */
  def fromState(state: AppState, scene: UiSceneSnapshot): OverlayViews =
    fromState(state, scene.calculatedLayout, Some(scene))

  private def fromState(
    state: AppState,
    layout: CalculatedLayout,
    scene: Option[UiSceneSnapshot]
  ): OverlayViews =
    val aboveCursorStack = stackViews(aboveCursorSurfaceIds(state, layout), state, layout, scene)
    val aboveCursor      = aboveCursorStack.headOption

    val belowCursorStack = stackViews(layout.belowCursorOverlayStack.map(_._1), state, layout, scene)
    val belowCursor      = belowCursorStack.headOption
    val modal = scene.toList.flatMap(_.modal).flatMap {
      case node @ SceneNode(SceneNodeId.Surface(surfaceId), _, _, _, _, _) =>
        state.surfaceById(surfaceId) match
          case Some(surface) => buildView(surface, state, Some(node.frameRect), false, 0.0)
          case None          =>
            // A blocking ModalDialog (#814) -- not a UiSurface, but shares the SceneNodeId.Surface id scheme.
            state.runtime.modalStack.find(_.id == surfaceId).flatMap(buildModalView(_, state, node.frameRect))
      case _ => None
    }

    val tabBar = state.tabBarSurface.flatMap(surface =>
      buildView(surface, state, layout.tabBarRect, collapsed = false, verticalOffsetRows = 0.0)
    )

    OverlayViews(
      aboveCursor = aboveCursor,
      aboveCursorStack = aboveCursorStack,
      belowCursor = belowCursor,
      belowCursorStack = belowCursorStack,
      modal = modal,
      tabBar = tabBar
    )

  private def overlayRect(
    surfaceId: SurfaceId,
    layout: CalculatedLayout,
    scene: Option[UiSceneSnapshot]
  ): Option[LayoutRect] =
    scene.flatMap(_.floatingRect(surfaceId)).orElse(EditorLayoutContract.overlayRectFor(surfaceId, layout))

  private def buildView(
    surface: com.serenity.state.models.UiSurface,
    state: AppState,
    layoutRect: Option[LayoutRect],
    collapsed: Boolean,
    verticalOffsetRows: Double
  ): Option[TextOverlayView] =
    surface.content match
      case content =>
        layoutRect.flatMap { rect =>
          contentView(content, state, rect, collapsed).map { resolved =>
            TextOverlayView(
              rect = rect,
              contentRect = Some(com.serenity.ui.layout.SurfaceFrameLayout.forContent(rect, content).contentRect),
              borderCells = com.serenity.ui.layout.SurfaceFrameLayout.borderCellsFor(content),
              alphaMultiplier = alphaMultiplierFor(surface, state),
              title = resolved.title,
              header = resolved.header,
              rows = resolved.rows,
              footer = resolved.footer,
              keyHintRow = resolved.keyHintRow,
              itemGapRows = itemGapRowsFor(content, state),
              itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity),
              verticalOffsetRows = verticalOffsetRows,
              surfaceId = Some(surface.id),
              // A collapsed (stacked-behind) surface renders `resolved`'s one-line summary (`collapsedContentView`),
              // never the full composition -- composition painting always wins over `rows` when present, so it must
              // not be set here while collapsed.
              composition = if collapsed then None else compositionFor(content, rect, state)
            )
          }
        }

  private def buildModalView(dialog: ModalDialog, state: AppState, rect: LayoutRect): Option[TextOverlayView] =
    val content = SurfaceContent.ModalWorkflow(dialog.modal)
    contentView(content, state, rect).map { resolved =>
      TextOverlayView(
        rect = rect,
        contentRect = Some(com.serenity.ui.layout.SurfaceFrameLayout.forContent(rect, content).contentRect),
        borderCells = com.serenity.ui.layout.SurfaceFrameLayout.borderCellsFor(content),
        alphaMultiplier = 1.0f,
        title = resolved.title,
        header = resolved.header,
        rows = resolved.rows,
        footer = resolved.footer,
        keyHintRow = resolved.keyHintRow,
        itemGapRows = itemGapRowsFor(content, state),
        itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity),
        verticalOffsetRows = 0.0,
        surfaceId = Some(dialog.id),
        composition = compositionFor(content, rect, state)
      )
    }

  /** A layout built by hand with only the legacy single `aboveCursorOverlayRect` names no stack: it shows the focused
    * above-cursor surface, else the first.
    */
  private def aboveCursorSurfaceIds(state: AppState, layout: CalculatedLayout): List[SurfaceId] =
    if layout.aboveCursorOverlayStack.nonEmpty then layout.aboveCursorOverlayStack.map(_._1)
    else
      val aboveCursorIds = state.floatingSurfaces.collect {
        case UiSurface(surfaceId, _, SurfacePresentation.Floating(_, SurfacePlacement.AboveCursor), _) => surfaceId
      }
      val focusedId = state.persisted.focus match
        case Focus.Surface(surfaceId) => aboveCursorIds.find(_ == surfaceId)
        case _                        => None
      layout.aboveCursorOverlayRect.toList.flatMap(_ => focusedId.orElse(aboveCursorIds.headOption))

  private def stackViews(
    surfaceIds: List[SurfaceId],
    state: AppState,
    layout: CalculatedLayout,
    scene: Option[UiSceneSnapshot]
  ): List[TextOverlayView] =
    surfaceIds.flatMap { surfaceId =>
      state
        .surfaceById(surfaceId)
        .flatMap(surface =>
          buildView(
            surface,
            state,
            overlayRect(surfaceId, layout, scene),
            collapsed = layout.collapsedFloatingSurfaceIds.contains(surfaceId),
            verticalOffsetRows = layout.floatingOverlayOffsetRows.getOrElse(surfaceId, 0.0)
          )
        )
    }

  private def contentView(
    content: com.serenity.state.models.SurfaceContent,
    state: AppState,
    rect: LayoutRect,
    collapsed: Boolean = false
  ): Option[ResolvedSurfaceContent] =
    val resolved =
      if collapsed then collapsedContentView(content)
      else
        content match
          // Painted entirely via `ContextMenuSurfaceComposition`/`CommandRunnerSurfaceComposition`/
          // `ContextualToolbarSurfaceComposition`/`CommentLensSurfaceComposition`/`OutlineSurfaceComposition`/
          // `DiagnosticsSurfaceComposition`/`DirectoryTreeSurfaceComposition`/`CommentsSurfaceComposition` (issue
          // #819, slices 2-5; issue #1683) -- `TextOverlayRenderer` ignores `rows` whenever `composition` is set
          // below, which it always is for these, so resolving real rows here would be dead computation on this
          // specific call site. Scoped to just this call site, not `SurfaceContentResolver.resolve`'s own dispatch:
          // `EditorLayoutContract` (`floatingGeometry`) calls that dispatcher independently and genuinely still needs
          // the real, item-count accurate rows/header/footer it produces -- see its own doc comment.
          case SurfaceContent.ContextMenu(_) | SurfaceContent.CommandPalette(_) | SurfaceContent.CommandRunnerPeek(_) |
              SurfaceContent.ContextualToolbar(_) | SurfaceContent.CommentLens(_) | SurfaceContent.Outline(_, _) |
              SurfaceContent.Diagnostics(_, _) | SurfaceContent.DirectoryTree(_, _) | SurfaceContent.Comments(_, _) =>
            ResolvedSurfaceContent()
          case _ =>
            SurfaceContentResolver.resolve(
              content,
              rect,
              SurfaceRenderMode.Floating,
              itemGapRowsFor(content, state),
              SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity),
              showKeyHintsFor(content, state)
            )
    Option.when(
      resolved.header.nonEmpty || resolved.rows.nonEmpty || resolved.footer.nonEmpty ||
        resolved.keyHintRow.nonEmpty || isComposedContent(content)
    )(resolved)

  private def showKeyHintsFor(content: com.serenity.state.models.SurfaceContent, state: AppState): Boolean =
    content match
      case SurfaceContent.CommandPalette(_) => state.persisted.config.surfaceConfig.commandRunnerShowKeyHints
      case _                                => false

  private def isComposedContent(content: SurfaceContent): Boolean =
    content match
      case SurfaceContent.ModalWorkflow(_)     => true
      case SurfaceContent.ContextMenu(_)       => true
      case SurfaceContent.CommandPalette(_)    => true
      case SurfaceContent.CommandRunnerPeek(_) => true
      case SurfaceContent.TabBar(_, _)         => true
      case SurfaceContent.ContextualToolbar(_) => true
      case SurfaceContent.CommentLens(_)       => true
      case SurfaceContent.Outline(_, _)        => true
      case SurfaceContent.Diagnostics(_, _)    => true
      case SurfaceContent.DirectoryTree(_, _)  => true
      case SurfaceContent.Comments(_, _)       => true
      case _                                   => false

  private def collapsedContentView(content: com.serenity.state.models.SurfaceContent): ResolvedSurfaceContent =
    content match
      case SurfaceContent.CommandPalette(runner) =>
        val label = runner.selectedItem match
          case Some(group: com.serenity.command.CommandSurfaceItem.GroupItem) => group.label
          case Some(item)                                                     => item.searchText
          case None                                                           => "commands"
        ResolvedSurfaceContent(rows = List(OverlayRow(label)))
      case other =>
        SurfaceContentResolver.resolve(other, LayoutRect(0, 0, 80, 3), SurfaceRenderMode.Floating)

  private def itemGapRowsFor(content: com.serenity.state.models.SurfaceContent, state: AppState): Double =
    content match
      case com.serenity.state.models.SurfaceContent.CommandPalette(_) |
          com.serenity.state.models.SurfaceContent.CommandRunnerPeek(_) |
          com.serenity.state.models.SurfaceContent.ContextMenu(_) =>
        state.persisted.config.effectiveCommandRunnerItemGapRows
      case com.serenity.state.models.SurfaceContent.ContextualToolbar(_) =>
        state.effectiveUiElementGap
      case _ => 0

  private def compositionFor(
    content: SurfaceContent,
    rect: LayoutRect,
    state: AppState
  ): Option[ResolvedSurfaceComposition] =
    content match
      case SurfaceContent.ModalWorkflow(modal) =>
        ModalSurfaceComposition.forModal(
          modal,
          rect,
          SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity),
          state.persisted.config.inputConfig.focusedKeymapConfig.modal.bindings
        )
      case SurfaceContent.ContextMenu(menu) =>
        Some(
          ContextMenuSurfaceComposition.forMenu(
            menu,
            rect,
            state.persisted.config.effectiveCommandRunnerItemGapRows,
            SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity)
          )
        )
      case SurfaceContent.CommandPalette(runner) =>
        Some(
          CommandRunnerSurfaceComposition.forRunner(
            runner,
            rect,
            itemGapRowsFor(content, state),
            SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity),
            showKeyHintsFor(content, state)
          )
        )
      case SurfaceContent.CommandRunnerPeek(runner) =>
        // Same composition machinery as `CommandPalette` (see `SurfaceContent.CommandRunnerPeek`'s own doc comment on
        // why this is a distinct case rather than the same one reused).
        Some(
          CommandRunnerSurfaceComposition.forRunner(
            runner,
            rect,
            itemGapRowsFor(content, state),
            SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity),
            showKeyHintsFor(content, state)
          )
        )
      case SurfaceContent.TabBar(entries, activeBufferId) =>
        Some(TabBarSurfaceComposition.forTabBar(entries, activeBufferId, rect))
      case SurfaceContent.ContextualToolbar(toolbarState) =>
        Some(ContextualToolbarSurfaceComposition.forToolbar(toolbarState, state, rect))
      case SurfaceContent.CommentLens(lens) =>
        Some(CommentLensSurfaceComposition.forLens(lens, rect))
      case SurfaceContent.DirectoryTree(tree, selectedPath) =>
        Some(DirectoryTreeSurfaceComposition.forTree(tree, selectedPath, rect))
      case SurfaceContent.Outline(symbols, activeLocation) =>
        Some(OutlineSurfaceComposition.forOutline(symbols, activeSymbolLocation(symbols, activeLocation, state), rect))
      case SurfaceContent.Comments(symbols, activeLocation) =>
        Some(
          CommentsSurfaceComposition.forComments(symbols, activeSymbolLocation(symbols, activeLocation, state), rect)
        )
      case SurfaceContent.Diagnostics(issues, activeLocation) =>
        Some(DiagnosticsSurfaceComposition.forDiagnostics(issues, activeLocation, rect))
      case _ => None

  /** Mirrors `PinnedPanelViewModel`'s own private helper of the same name: `Outline`/`Comments` content carries its own
    * `activeLocation`, but falls back to whatever symbol the live cursor position is currently inside, so a floating
    * peek highlights the same row a docked panel would.
    */
  private def activeSymbolLocation(
    symbols: List[Symbol],
    fallback: Option[Location],
    state: AppState
  ): Option[Location] =
    fallback.orElse(
      state.activeCursorPosition.flatMap(cursor => DocumentNavigation.currentSymbol(symbols, cursor)).map(_.location)
    )

  private def alphaMultiplierFor(surface: com.serenity.state.models.UiSurface, state: AppState): Float =
    val focusMultiplier =
      state.persisted.focus match
        case Focus.Surface(focusedId) if focusedId != surface.id && isCommandRunnerSurface(surface.content) =>
          InactiveFloatingPanelAlphaMultiplier
        case _ => 1.0f
    focusMultiplier

  private def isCommandRunnerSurface(content: com.serenity.state.models.SurfaceContent): Boolean =
    content match
      case SurfaceContent.CommandPalette(_) => true
      case _                                => false
