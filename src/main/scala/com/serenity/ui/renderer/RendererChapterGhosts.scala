package com.serenity.ui.renderer

import com.serenity.state.models.*
import com.serenity.ui.layout.*

/** Paints a chapter note's overview, faded, on the blank rows under an empty chapter. A separate pass drawn after each
  * pane's own content, so it replaces nothing: the rows are blank in the document and the ghost exists only in the
  * frame. It reuses [[RendererPaneContent]]'s row arithmetic so a ghost sits exactly where the row's own text would.
  *
  * Single-column plain panes only: the inline Markdown lens and the multi-column page layout paint their own rows, and
  * `BufferRenderAnnotations.ghostsByLine` is empty for the former.
  */
object RendererChapterGhosts:

  def render(state: AppState, context: RenderContext, renderPlan: EditorPaneRenderPlan): Unit =
    state.persisted.layout.editorPanes.foreach { (paneId, pane) =>
      for
        paneLayout <- renderPlan.paneLayouts.get(paneId)
        snapshot   <- renderPlan.snapshots.get(paneId)
        bufferId   <- pane.bufferId
        buffer     <- state.persisted.buffers.get(bufferId)
        ghosts     <- renderPlan.annotations.get(bufferId).map(_.ghostsByLine).filter(_.nonEmpty)
        if renderPlan.columnSnapshotsFor(paneId).isEmpty
      do renderPane(buffer, paneLayout.contentRect, snapshot, ghosts, state, context)
    }

  private def renderPane(
    buffer: Buffer,
    rect: LayoutRect,
    snapshot: TextLayoutSnapshot,
    ghosts: Map[Int, String],
    state: AppState,
    context: RenderContext
  ): Unit =
    context.surface.text.setFont(context.fontForBuffer(buffer))
    snapshot.visualLines.zipWithIndex.foreach { (visualLine, index) =>
      val ghost = ghosts.get(visualLine.bufferLine).filter(_ => visualLine.startColumn == 0 && visualLine.text.isBlank)
      val screenX = rect.x + RendererPaneContent.visualLineCellOffset(visualLine, context)
      val visible =
        RendererPaneContent.visualLineFits(rect, index, context, snapshot) &&
          RendererPaneContent.visualLineVisible(rect, index, context, snapshot) &&
          screenX >= 0 && screenX < rect.right
      if visible then
        ghost.foreach(paintLine(_, rect, screenX, index, visualLine, snapshot, state.persisted.theme, context))
    }

  private def paintLine(
    ghost: String,
    rect: LayoutRect,
    screenX: Int,
    index: Int,
    visualLine: TextVisualLine,
    snapshot: TextLayoutSnapshot,
    theme: com.serenity.ui.theme.Theme,
    context: RenderContext
  ): Unit =
    val text = ghost.take(math.max(0, rect.right - screenX))
    if text.nonEmpty then
      val surface = context.surface
      surface.setForegroundColor(RenderColor.fromAwt(theme.placeholder))
      surface.setBackgroundColor(RenderColor.fromAwt(theme.background))
      if RendererPaneSetup.usesMeasuredDrawing(snapshot, context) then
        val xOriginPx       = context.cellMetrics.toPixelX(rect.x).toFloat
        val contentRightXPx = context.cellMetrics.toPixelX(rect.right).toFloat
        surface.text.drawRunPx(
          xOriginPx,
          RendererPaneContent.visualLineTopPx(rect, index, context, snapshot),
          contentRightXPx - xOriginPx,
          RendererPaneContent.rowHeightPxFor(visualLine, snapshot),
          RendererPaneContent.rowAscentPxFor(visualLine, snapshot),
          text
        )
      else CharacterRenderer.renderString(surface, screenX, rect.y + index, text)
      surface.setForegroundColor(RenderColor.fromAwt(theme.foreground))
