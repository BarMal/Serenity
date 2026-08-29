package com.serenity.ui.renderer

import com.serenity.animation.AnimationState
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.slf4j.LoggerFactory

final case class TextOverlayView(
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
    itemTargetRows: Int = 1,
    verticalOffsetRows: Double = 0.0,
    surfaceId: Option[SurfaceId] = None,
    composition: Option[ResolvedSurfaceComposition] = None
):

  def resolvedContentRect: LayoutRect =
    contentRect.getOrElse(com.serenity.ui.layout.SurfaceFrameLayout(rect, borderCells).contentRect)

  def contentRowSlots: List[SurfaceContentRowSlot] =
    SurfaceFrameLayout.contentRowSlotsFor(
      resolvedContentRect,
      rows.length,
      header.nonEmpty,
      footer.nonEmpty,
      itemGapRows,
      itemTargetRows
    )

final case class OverlayViews(
    aboveCursor: Option[TextOverlayView] = None,
    belowCursor: Option[TextOverlayView] = None,
    belowCursorStack: List[TextOverlayView] = Nil,
    modal: List[TextOverlayView] = Nil
)

object OverlayViewModel:
  private val logger = LoggerFactory.getLogger("com.serenity.ui.renderer.OverlayViewModel")
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
    val aboveCursor = preferredFloatingSurface(state, SurfacePlacement.AboveCursor)
      .flatMap(surface =>
        buildView(
          surface,
          state,
          overlayRect(surface.id, layout, scene),
          collapsed = false,
          verticalOffsetRows = layout.floatingOverlayOffsetRows.getOrElse(surface.id, 0.0)
        )
      )

    val belowCursorStack = preferredBelowCursorSurfaces(state, layout, scene)
    val belowCursor      = belowCursorStack.headOption
    val modal = scene.toList.flatMap(_.modal).flatMap {
      case node @ SceneNode(SceneNodeId.Surface(surfaceId), _, _, _, _, _) =>
        state.surfaceById(surfaceId).flatMap(surface => buildView(surface, state, Some(node.frameRect), false, 0.0))
      case _ => None
    }

    OverlayViews(
      aboveCursor = aboveCursor,
      belowCursor = belowCursor,
      belowCursorStack = belowCursorStack,
      modal = modal
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
    val animState =
      state.runtime.surfaceAnimations.get(surface.id).map(_.animationState).getOrElse(AnimationState.empty)
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
            itemTargetRows =
              SurfaceFrameLayout.itemTargetRowsFor(originalContent, state.persisted.config.interfaceDensity),
            verticalOffsetRows = verticalOffsetRows,
            surfaceId = Some(surface.id),
            composition = compositionFor(originalContent, cachedRect, state)
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
              itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity),
              verticalOffsetRows = verticalOffsetRows,
              surfaceId = Some(surface.id),
              composition = compositionFor(content, rect, state)
            )
          }
        }

  private def preferredFloatingSurface(
    state: AppState,
    placement: SurfacePlacement
  ): Option[com.serenity.state.models.UiSurface] =
    val matchingSurfaces = state.runtime.uiSurfaces.filter { surface =>
      val phase = state.runtime.surfaceAnimations.get(surface.id).map(_.phase).getOrElse(SurfacePhase.Visible)
      phase != SurfacePhase.BufferFadingOut &&
      (surface match
        case com.serenity.state.models.UiSurface(_, _, SurfacePresentation.Floating(_, currentPlacement), _) =>
          currentPlacement == placement
        case _ => false)
    }

    val selectedSurface = state.persisted.focus match
      case com.serenity.state.models.Focus.Surface(surfaceId) =>
        matchingSurfaces.find(_.id == surfaceId).orElse(matchingSurfaces.headOption)
      case _ =>
        matchingSurfaces.headOption

    selectedSurface.foreach { surface =>
      logger.info(
        s"[OVERLAY SELECTED] placement=$placement focus=${state.persisted.focus} surfaceId=${surface.id} " +
          s"content=${surface.content.getClass.getSimpleName}"
      )
    }

    selectedSurface

  private def preferredBelowCursorSurfaces(
    state: AppState,
    layout: CalculatedLayout,
    scene: Option[UiSceneSnapshot]
  ): List[TextOverlayView] =
    layout.belowCursorOverlayStack.map(_._1).flatMap { surfaceId =>
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
          case SurfaceContent.ContextualToolbar(toolbarState) =>
            SurfaceContentResolver.resolveContextualToolbar(toolbarState, state, rect, SurfaceRenderMode.Floating)
          case _ =>
            SurfaceContentResolver.resolve(
              content,
              rect,
              SurfaceRenderMode.Floating,
              itemGapRowsFor(content, state),
              SurfaceFrameLayout.itemTargetRowsFor(content, state.persisted.config.interfaceDensity)
            )
    Option.when(
      resolved.header.nonEmpty || resolved.rows.nonEmpty || resolved.footer.nonEmpty || isComposedContent(content)
    )(resolved)

  private def isComposedContent(content: SurfaceContent): Boolean =
    content match
      case SurfaceContent.ModalWorkflow(_) => true
      case _                               => false

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
          com.serenity.state.models.SurfaceContent.ContextMenu(_) =>
        state.persisted.config.surfaceConfig.commandRunnerItemGapRows
      case com.serenity.state.models.SurfaceContent.ContextualToolbar(_) =>
        state.persisted.config.uiElementGap
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
          SurfaceFrameLayout.minimumTargetRows(state.persisted.config.interfaceDensity)
        )
      case _ => None

  private def alphaMultiplierFor(surface: com.serenity.state.models.UiSurface, state: AppState): Float =
    val focusMultiplier =
      state.persisted.focus match
        case Focus.Surface(focusedId) if focusedId != surface.id && isCommandRunnerSurface(surface.content) =>
          InactiveFloatingPanelAlphaMultiplier
        case _ => 1.0f
    focusMultiplier

  private def isCommandRunnerSurface(content: com.serenity.state.models.SurfaceContent): Boolean =
    content match
      case SurfaceContent.CommandPalette(_)                => true
      case SurfaceContent.GhostOverlay(originalContent, _) => isCommandRunnerSurface(originalContent)
      case _                                               => false
