package com.serenity.ui.renderer

import com.serenity.state.models.{AppState, SurfacePlacement, SurfacePresentation}
import com.serenity.ui.layout.{CalculatedLayout, LayoutRect}
import org.slf4j.LoggerFactory

case class TextOverlayView(
    rect: LayoutRect,
    title: Option[String] = None,
    header: Option[OverlayRow] = None,
    rows: List[OverlayRow] = Nil,
    footer: Option[OverlayRow] = None
)

case class OverlayViews(
    aboveCursor: Option[TextOverlayView] = None,
    belowCursor: Option[TextOverlayView] = None
)

object OverlayViewModel:
  private val logger = LoggerFactory.getLogger("com.serenity.ui.renderer.OverlayViewModel")

  def fromState(state: AppState, layout: CalculatedLayout): OverlayViews =
    val aboveCursor = preferredFloatingSurface(state, SurfacePlacement.AboveCursor)
      .flatMap(surface =>
        layout.aboveCursorOverlayRect.flatMap(rect => contentView(surface.content, rect).map(content => TextOverlayView(
          rect = rect,
          title = content.title,
          header = content.header,
          rows = content.rows,
          footer = content.footer
        )))
      )

    val belowCursor = preferredFloatingSurface(state, SurfacePlacement.BelowCursor)
      .flatMap(surface =>
        layout.belowCursorOverlayRect.flatMap(rect => contentView(surface.content, rect).map(content => TextOverlayView(
          rect = rect,
          title = content.title,
          header = content.header,
          rows = content.rows,
          footer = content.footer
        )))
      )

    OverlayViews(
      aboveCursor = aboveCursor,
      belowCursor = belowCursor
    )

  private def preferredFloatingSurface(state: AppState, placement: SurfacePlacement): Option[com.serenity.state.models.UiSurface] =
    val matchingSurfaces = state.uiSurfaces.filter {
      case com.serenity.state.models.UiSurface(_, _, SurfacePresentation.Floating(_, currentPlacement), _) =>
        currentPlacement == placement
      case _ =>
        false
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

  private def contentView(content: com.serenity.state.models.SurfaceContent, rect: LayoutRect): Option[ResolvedSurfaceContent] =
    val resolved = SurfaceContentResolver.resolve(content, rect, SurfaceRenderMode.Floating)
    Option.when(resolved.header.nonEmpty || resolved.rows.nonEmpty || resolved.footer.nonEmpty)(resolved)
