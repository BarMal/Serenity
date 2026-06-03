package com.serenity.ui.renderer

import com.serenity.animation.AnimationState
import com.serenity.state.models.*
import com.serenity.ui.layout.{CalculatedLayout, LayoutRect}
import org.slf4j.LoggerFactory

case class TextOverlayView(
    rect: LayoutRect,
    animationState: AnimationState = AnimationState.empty,
    alphaMultiplier: Float = 1.0f,
    title: Option[String] = None,
    header: Option[OverlayRow] = None,
    rows: List[OverlayRow] = Nil,
    footer: Option[OverlayRow] = None
)

case class OverlayViews(
    aboveCursor: Option[TextOverlayView] = None,
    belowCursor: Option[TextOverlayView] = None,
    belowCursorStack: List[TextOverlayView] = Nil
)

object OverlayViewModel:
  private val logger = LoggerFactory.getLogger("com.serenity.ui.renderer.OverlayViewModel")

  def fromState(state: AppState, layout: CalculatedLayout): OverlayViews =
    val aboveCursor = preferredFloatingSurface(state, SurfacePlacement.AboveCursor)
      .flatMap(surface => buildView(surface, state, layout.aboveCursorOverlayRect, collapsed = false))

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
        contentView(originalContent, cachedRect).map { content =>
          TextOverlayView(
            rect = cachedRect,
            animationState = animState,
            alphaMultiplier = 1.0f,
            title = content.title,
            header = content.header,
            rows = content.rows,
            footer = content.footer
          )
        }
      case content =>
        layoutRect.flatMap { rect =>
          contentView(content, rect, collapsed).map { resolved =>
            TextOverlayView(
              rect = rect,
              animationState = animState,
              alphaMultiplier = alphaMultiplierFor(content),
              title = resolved.title,
              header = resolved.header,
              rows = resolved.rows,
              footer = resolved.footer
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
    layout.belowCursorOverlayStack.flatMap { case (surfaceId, rect) =>
      state.surfaceById(surfaceId).flatMap(surface =>
        buildView(
          surface,
          state,
          Some(rect),
          collapsed = layout.collapsedFloatingSurfaceIds.contains(surfaceId)
        )
      )
    }

  private def contentView(
    content: com.serenity.state.models.SurfaceContent,
    rect: LayoutRect,
    collapsed: Boolean = false
  ): Option[ResolvedSurfaceContent] =
    val resolved =
      if collapsed then collapsedContentView(content)
      else SurfaceContentResolver.resolve(content, rect, SurfaceRenderMode.Floating)
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

  private def alphaMultiplierFor(content: com.serenity.state.models.SurfaceContent): Float =
    content match
      case SurfaceContent.CommandPaletteSubmenu(_, _, previewOnly) if previewOnly => 0.55f
      case _                                                                      => 1.0f
