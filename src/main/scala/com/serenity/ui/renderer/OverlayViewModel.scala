package com.serenity.ui.renderer

import com.serenity.animation.AnimationState
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.slf4j.LoggerFactory

case class TextOverlayView(
    rect: LayoutRect,
    contentRect: Option[LayoutRect] = None,
    borderCells: Int = 1,
    animationState: AnimationState = AnimationState.empty,
    alphaMultiplier: Float = 1.0f,
    title: Option[String] = None,
    header: Option[OverlayRow] = None,
    rows: List[OverlayRow] = Nil,
    footer: Option[OverlayRow] = None,
    itemGapRows: Double = 0.0,
    surfaceId: Option[SurfaceId] = None
):

  def resolvedContentRect: LayoutRect =
    contentRect.getOrElse(com.serenity.ui.layout.SurfaceFrameLayout(rect, borderCells).contentRect)

  def contentRowSlots: List[SurfaceContentRowSlot] =
    SurfaceFrameLayout.contentRowSlotsFor(
      resolvedContentRect,
      rows.length,
      header.nonEmpty,
      footer.nonEmpty,
      FloatingSurfaceGeometry.requiredCellRows(itemGapRows)
    )

  /** Pixel geometry is the source of truth for floating-surface paint and pointer targeting. */
  def geometry(metrics: CellMetrics): FloatingSurfaceGeometry =
    FloatingSurfaceGeometry.calculate(
      frame = rect,
      metrics = metrics,
      borderCells = borderCells,
      itemCount = rows.length,
      itemGapRows = itemGapRows,
      itemOffsetRows = if header.nonEmpty then 1.0 else 0.0
    )

case class OverlayViews(
    aboveCursor: Option[TextOverlayView] = None,
    belowCursor: Option[TextOverlayView] = None,
    belowCursorStack: List[TextOverlayView] = Nil
)

object OverlayViewModel:
  private val logger = LoggerFactory.getLogger("com.serenity.ui.renderer.OverlayViewModel")
  private val InactiveFloatingPanelAlphaMultiplier = 0.68f

  def fromState(state: AppState, layout: CalculatedLayout): OverlayViews =
    val aboveCursor = preferredFloatingSurface(state, SurfacePlacement.AboveCursor)
      .flatMap(surface =>
        buildView(surface, state, EditorLayoutContract.overlayRectFor(surface.id, layout), collapsed = false)
      )

    val belowCursorStack = preferredBelowCursorSurfaces(state, layout)
    val belowCursor      = belowCursorStack.headOption

    OverlayViews(
      aboveCursor = aboveCursor,
      belowCursor = belowCursor,
      belowCursorStack = belowCursorStack
    )

  private def buildView(
    surface: com.serenity.state.models.UiSurface,
    state: AppState,
    layoutRect: Option[LayoutRect],
    collapsed: Boolean
  ): Option[TextOverlayView] =
    val animState = state.surfaceAnimations.get(surface.id).map(_.animationState).getOrElse(AnimationState.empty)
    surface.content match
      case com.serenity.state.models.SurfaceContent.GhostOverlay(originalContent, cachedRect) =>
        contentView(originalContent, state, cachedRect).map { content =>
          TextOverlayView(
            rect = cachedRect,
            contentRect =
              Some(com.serenity.ui.layout.SurfaceFrameLayout.forContent(cachedRect, originalContent).contentRect),
            borderCells = com.serenity.ui.layout.SurfaceFrameLayout.borderCellsFor(originalContent),
            animationState = animState,
            alphaMultiplier = 1.0f,
            title = content.title,
            header = content.header,
            rows = content.rows,
            footer = content.footer,
            itemGapRows = itemGapRowsFor(originalContent, state),
            surfaceId = Some(surface.id)
          )
        }
      case content =>
        layoutRect.flatMap { rect =>
          contentView(content, state, rect, collapsed).map { resolved =>
            TextOverlayView(
              rect = rect,
              contentRect = Some(com.serenity.ui.layout.SurfaceFrameLayout.forContent(rect, content).contentRect),
              borderCells = com.serenity.ui.layout.SurfaceFrameLayout.borderCellsFor(content),
              animationState = animState,
              alphaMultiplier = alphaMultiplierFor(surface, state),
              title = resolved.title,
              header = resolved.header,
              rows = resolved.rows,
              footer = resolved.footer,
              itemGapRows = itemGapRowsFor(content, state),
              surfaceId = Some(surface.id)
            )
          }
        }

  private def preferredFloatingSurface(
    state: AppState,
    placement: SurfacePlacement
  ): Option[com.serenity.state.models.UiSurface] =
    val matchingSurfaces = state.uiSurfaces.filter { surface =>
      val phase = state.surfaceAnimations.get(surface.id).map(_.phase).getOrElse(SurfacePhase.Visible)
      phase != SurfacePhase.BufferFadingOut &&
      (surface match
        case com.serenity.state.models.UiSurface(_, _, SurfacePresentation.Floating(_, currentPlacement), _) =>
          currentPlacement == placement
        case _ => false)
    }

    val selectedSurface = state.focus match
      case com.serenity.state.models.Focus.Surface(surfaceId) =>
        matchingSurfaces.find(_.id == surfaceId).orElse(matchingSurfaces.headOption)
      case _ =>
        matchingSurfaces.headOption

    selectedSurface.foreach { surface =>
      logger.info(
        s"[OVERLAY SELECTED] placement=$placement focus=${state.focus} surfaceId=${surface.id} " +
          s"content=${surface.content.getClass.getSimpleName}"
      )
    }

    selectedSurface

  private def preferredBelowCursorSurfaces(state: AppState, layout: CalculatedLayout): List[TextOverlayView] =
    layout.belowCursorOverlayStack.map(_._1).flatMap { surfaceId =>
      state
        .surfaceById(surfaceId)
        .flatMap(surface =>
          buildView(
            surface,
            state,
            EditorLayoutContract.overlayRectFor(surfaceId, layout),
            collapsed = layout.collapsedFloatingSurfaceIds.contains(surfaceId)
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
          case SurfaceContent.ContextualToolbar(toolbarState) =>
            SurfaceContentResolver.resolveContextualToolbar(toolbarState, state, rect, SurfaceRenderMode.Floating)
          case _ =>
            SurfaceContentResolver.resolve(content, rect, SurfaceRenderMode.Floating, itemGapRowsFor(content, state))
    Option.when(resolved.header.nonEmpty || resolved.rows.nonEmpty || resolved.footer.nonEmpty)(resolved)

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
          com.serenity.state.models.SurfaceContent.CommandPaletteSubmenu(_, _, _) |
          com.serenity.state.models.SurfaceContent.ContextMenu(_) =>
        state.config.commandRunnerItemGapRows
      case com.serenity.state.models.SurfaceContent.ContextualToolbar(_) =>
        state.config.uiElementGap
      case _ => 0.0

  private def alphaMultiplierFor(surface: com.serenity.state.models.UiSurface, state: AppState): Float =
    val focusMultiplier =
      state.focus match
        case Focus.Surface(focusedId) if focusedId != surface.id && isCommandRunnerSurface(surface.content) =>
          InactiveFloatingPanelAlphaMultiplier
        case _ => 1.0f
    surface.content match
      case SurfaceContent.CommandPaletteSubmenu(_, _, previewOnly) if previewOnly => 0.55f
      case _                                                                      => focusMultiplier

  private def isCommandRunnerSurface(content: com.serenity.state.models.SurfaceContent): Boolean =
    content match
      case SurfaceContent.CommandPalette(_)                => true
      case SurfaceContent.CommandPaletteSubmenu(_, _, _)   => true
      case SurfaceContent.GhostOverlay(originalContent, _) => isCommandRunnerSurface(originalContent)
      case _                                               => false
