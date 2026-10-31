package com.serenity.ui.renderer

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Paints the chrome that belongs to one editor pane rather than to the document rows inside it: its header and its top
  * and bottom spacers. Both are pure functions of state a frame can tell has not changed, so [[RendererFramePlanner]]
  * repaints them only when [[ChromeRepaint]] says so.
  */
private[renderer] object RendererPaneChrome:

  def renderHeader(
    pane: EditorPane,
    buffer: Option[Buffer],
    paneLayout: EditorPaneLayout,
    state: AppState,
    context: RenderContext,
    contract: EditorLayoutContract
  ): Unit =
    val surface    = context.surface
    val headerRect = contract.paneHeaderRect(pane.id).getOrElse(paneLayout.headerRect)
    if headerRect.height > 0 then
      context.surface.text.setFont(FontSpec.fromAwt(context.uiFont))
      val isActive  = state.persisted.layout.activeEditorPaneId.contains(pane.id)
      val titleRect = contract.paneTitleRect(pane.id).getOrElse(paneLayout.titleRect)

      if isActive then
        surface.setBackgroundColor(state.persisted.theme.highlighted.background)
        surface.setForegroundColor(state.persisted.theme.highlighted.foreground)
      else
        surface.setBackgroundColor(state.persisted.theme.panel.background)
        surface.setForegroundColor(state.persisted.theme.panel.foreground)

      val bufferTitleBase = buffer match
        case Some(buf) =>
          buf.document.filePath match
            case Some(path) =>
              val filename = path.getFileName.toString
              if buf.document.isDirty then s"$filename - unsaved" else filename
            case None =>
              if buf.document.isDirty then s"Buffer ${buf.id.value} - unsaved" else s"Buffer ${buf.id.value}"
        case None =>
          "No Buffer"
      val bufferTitle = bufferTitleBase

      val maxTitleWidth = math.max(1, titleRect.width - 2)
      val displayTitle =
        if bufferTitle.length > maxTitleWidth then bufferTitle.take(maxTitleWidth - 3) + "..."
        else bufferTitle

      val window =
        state.runtime.viewportSize.getOrElse(ViewportSize(surface.viewportWidth, surface.viewportHeight))
      val corners =
        CornerRounding.corners(
          ChromeElement.PaneHeader,
          headerRect,
          window,
          state.persisted.config.dockedCornerStyle,
          state.persisted.config.paneHeaderCorners
        )
      val fillHeader = () => surface.putString(headerRect.x, headerRect.y, " " * headerRect.width)
      surface.panelOutlines.fold(fillHeader()) { outlines =>
        outlines.withRoundRectClip(
          headerRect.x,
          headerRect.y,
          headerRect.width,
          headerRect.height,
          corners,
          state.persisted.config.scaledUiCornerRadiusPx
        )(fillHeader())
      }

      surface.text.fontRenderContext match
        case Some(frc) =>
          val titlePlacement = TextAlignment.placeLine(
            displayTitle,
            TextAreaPx(
              xPx = context.cellMetrics.toPixelX(titleRect.x).toFloat,
              yPx = context.cellMetrics.toPixelY(titleRect.y),
              widthPx = titleRect.width * context.cellMetrics.charWidth.toFloat,
              heightPx = context.cellMetrics.lineHeight
            ),
            context.uiFont,
            context.cellMetrics.lineHeight,
            context.cellMetrics.ascent,
            TextHorizontalAlignment.Center,
            TextVerticalAlignment.Top,
            frc
          )
          surface.text.drawRunPx(
            titlePlacement.xPx,
            titlePlacement.yPx,
            titlePlacement.widthPx,
            titlePlacement.lineHeightPx,
            titlePlacement.ascentPx,
            displayTitle
          )
        case None =>
          val centerX = titleRect.x + math.max(0, (titleRect.width - displayTitle.length) / 2)
          CharacterRenderer.renderString(surface, centerX, titleRect.y, displayTitle)

  def renderVerticalSpacers(paneLayout: EditorPaneLayout, state: AppState, context: RenderContext): Unit =
    context.surface.setBackgroundColor(state.persisted.theme.margin)
    List(paneLayout.topSpacerRect, paneLayout.bottomSpacerRect)
      .filter(rect => rect.width > 0 && rect.height > 0)
      .foreach(rect => context.surface.fillRect(rect.x, rect.y, rect.width, rect.height, ' '))
