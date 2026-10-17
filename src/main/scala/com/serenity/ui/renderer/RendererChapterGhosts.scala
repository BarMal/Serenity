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

  /** Paints the ghosts on the rows `dirtyRowsByPane` names for each pane; a pane it does not name paints every row. */
  def render(
    state: AppState,
    context: RenderContext,
    renderPlan: EditorPaneRenderPlan,
    dirtyRowsByPane: Map[PaneId, Set[Int]]
  ): Unit =
    state.persisted.layout.editorPanes.foreach { (paneId, pane) =>
      for
        paneLayout <- renderPlan.paneLayouts.get(paneId)
        snapshot   <- renderPlan.snapshots.get(paneId)
        bufferId   <- pane.bufferId
        buffer     <- state.persisted.buffers.get(bufferId)
        ghosts     <- renderPlan.annotations.get(bufferId).map(_.ghostsByLine).filter(_.nonEmpty)
        if renderPlan.columnSnapshotsFor(paneId).isEmpty
      do renderPane(buffer, paneLayout.contentRect, snapshot, ghosts, dirtyRowsByPane.get(paneId), state, context)
    }

  /** The ghost each of `snapshot`'s rows paints, if any: a ghost sits on the first row of a blank line. */
  def rowGhosts(snapshot: TextLayoutSnapshot, ghosts: Map[Int, String]): Vector[Option[String]] =
    if ghosts.isEmpty then Vector.empty
    else
      snapshot.visualLines.map(visualLine =>
        ghosts.get(visualLine.bufferLine).filter(_ => visualLine.startColumn == 0 && visualLine.text.isBlank)
      )

  /** The rows whose ghost differs between two frames' [[rowGhosts]], a row past either end having none. */
  def changedRows(previous: Vector[Option[String]], current: Vector[Option[String]]): Set[Int] =
    (0 until math.max(previous.size, current.size)).filter { row =>
      previous.lift(row).flatten != current.lift(row).flatten
    }.toSet

  /** Whether `bufferId` has a chapter note a ghost is derived from. A ghost depends on a note's text and on which
    * chapters are empty, which no buffer damage reports.
    */
  def paneShowsGhosts(state: AppState, bufferId: BufferId): Boolean =
    state.runtime.chapterGhostsVisible &&
      state.persisted.buffers
        .get(bufferId)
        .exists(buffer => !buffer.hidden && buffer.annotations.notes.keys.exists(hasChapterKey))

  private def hasChapterKey(key: NoteKey): Boolean =
    key match
      case NoteKey.Chapter(_) => true
      case NoteKey.Keyword(_) => false

  private def renderPane(
    buffer: Buffer,
    rect: LayoutRect,
    snapshot: TextLayoutSnapshot,
    ghosts: Map[Int, String],
    dirtyRows: Option[Set[Int]],
    state: AppState,
    context: RenderContext
  ): Unit =
    context.surface.text.setFont(FontSpec.fromAwt(context.fontForBuffer(buffer)))
    val ghostOfRow = rowGhosts(snapshot, ghosts)
    snapshot.visualLines.zipWithIndex.foreach { (visualLine, index) =>
      val ghost   = ghostOfRow.lift(index).flatten.filter(_ => dirtyRows.forall(_.contains(index)))
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
      surface.setForegroundColor(theme.placeholder)
      surface.setBackgroundColor(theme.background)
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
      else
        CharacterRenderer.renderString(
          surface,
          screenX,
          RendererPaneContent.textRowMetrics(rect, context, snapshot).screenRow(index),
          text
        )
      surface.setForegroundColor(theme.foreground)
