package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.text.TextEditing
import com.serenity.ui.layout.*

/** State the event pipeline exposes for resolving a mouse event to an editor pane, buffer, and cursor position, as a
  * capability record rather than a trait -- nothing here breaks a construction-order cycle (#1389), so mockability is
  * the only reason this needs an interface at all, and a record fakes trivially without one (#1017).
  */
final private[manager] case class EditorMouseTargetingPort(
    stateRef: Ref[IO, AppState],
    mouseTargetCacheRef: Ref[IO, Option[MouseTargetCache]]
)

/** Resolves a mouse event's screen position to an editor pane/buffer/cursor via the shared [[MouseTargetCache]],
  * derives word/line/range selections from a resolved cursor, and tracks the currently hovered editor target. Every
  * other mouse-hit-testing module that needs an editor click target goes through here so the cache stays the single
  * source of truth for the prepared scene shared with rendering.
  */
final private[manager] class EditorMouseTargeting(port: EditorMouseTargetingPort):
  import port.*

  def resolveMouseTarget(
    click: MouseInputEvent,
    state: AppState
  ): IO[Option[(PaneId, Buffer, CursorPosition)]] =
    state.runtime.viewportSize match
      case None => IO.pure(None)
      case Some(tSize) =>
        mouseTargetLayout(state, tSize).flatMap { cache =>
          cache.scene.paneLayouts.find {
            case (_, paneLayout) =>
              paneLayout.contentRect.contains(click.col, click.row)
          } match
            case Some((paneId, paneLayout)) =>
              state.persisted.layout.editorPanes
                .get(paneId)
                .flatMap(pane => pane.bufferId.flatMap(state.persisted.buffers.get)) match
                case Some(buffer) =>
                  val contentRect = paneLayout.contentRect
                  val vp          = buffer.viewport
                  val visualRow   = (click.row - contentRect.y).max(0)
                  mouseTargetSnapshot(cache, paneId).map { snapshot =>
                    val cellWidthPx =
                      if contentRect.width > 0 then snapshot.panelWidthPx.toFloat / contentRect.width.toFloat else 1.0f
                    val xPx = click.pixelX match
                      case Some(pixelX) => pixelX.toFloat - (contentRect.x * cellWidthPx)
                      case None         => (click.col - contentRect.x).max(0) * cellWidthPx
                    val clickedCursor = snapshot
                      .cursorForVisualRowAndXPx(visualRow, xPx.max(0.0f))
                      .orElse {
                        val bufferLine  = (vp.topLine + visualRow).max(0)
                        val bufferCol   = (vp.leftColumn + (click.col - contentRect.x)).max(0)
                        val clampedLine = bufferLine.min(math.max(0, buffer.document.content.lineCount - 1))
                        val lineLen     = buffer.document.content.getLine(clampedLine).getOrElse("").length
                        Some(CursorPosition(clampedLine, bufferCol.min(lineLen)))
                      }
                    clickedCursor.map(cursor => (paneId, buffer, cursor))
                  }
                case None =>
                  IO.pure(None)
            case None => IO.pure(None)
        }

  def updateEditorHoverTarget(move: MouseMove, state: AppState): IO[Unit] =
    resolveMouseTarget(move, state).flatMap {
      case Some((paneId, buffer, cursor)) =>
        stateRef.update(s =>
          s.copy(runtime = s.runtime.copy(hoveredEditorTarget = Some(HoveredEditorTarget(paneId, buffer.id, cursor))))
        )
      case None =>
        clearEditorHoverTarget
    }

  def clearEditorHoverTarget: IO[Unit] =
    stateRef.update(s => s.copy(runtime = s.runtime.copy(hoveredEditorTarget = None)))

  def wordSelectionAtCursor(buffer: Buffer, cursor: CursorPosition): Option[Selection] =
    val source        = RopeCharacterSource(buffer.document.content)
    val clickedOffset = buffer.document.content.lineColumnToOffset(cursor.line, cursor.column)
    if source.length == 0 then None
    else
      val probeOffset =
        if clickedOffset >= source.length then source.length - 1
        else if source.charAt(clickedOffset).isWhitespace && clickedOffset > 0 && !source
              .charAt(clickedOffset - 1)
              .isWhitespace
        then clickedOffset - 1
        else clickedOffset
      if probeOffset < 0 || source.charAt(probeOffset).isWhitespace then None
      else
        val start = TextEditing.previousWordBoundary(source, probeOffset)
        @annotation.tailrec
        def wordEndFrom(offset: Int): Int =
          if offset < source.length && !source.charAt(offset).isWhitespace then wordEndFrom(offset + 1)
          else offset
        val end = wordEndFrom(probeOffset)
        Some(
          Selection(
            offsetToCursorPosition(buffer.document.content, start),
            offsetToCursorPosition(buffer.document.content, end)
          )
        )

  def lineSelectionAtCursor(buffer: Buffer, cursor: CursorPosition): Option[Selection] =
    val lineText = buffer.document.content.getLine(cursor.line).getOrElse("")
    Some(
      Selection(
        CursorPosition(cursor.line, 0),
        CursorPosition(cursor.line, lineText.length)
      )
    )

  def rangeSelectionFromAnchor(buffer: Buffer, focus: CursorPosition): Option[Selection] =
    val anchor = buffer.primarySelection.map(_.anchor).orElse(buffer.editing.cursors.headOption).getOrElse(focus)
    Option.when(anchor != focus)(Selection(anchor, focus))

  private def offsetToCursorPosition(content: com.serenity.rope.Rope, offset: Int): CursorPosition =
    val (line, column) = content.offsetToLineColumn(offset)
    CursorPosition(line, column)

  private def mouseTargetLayout(state: AppState, viewportSize: ViewportSize): IO[MouseTargetCache] =
    val key = MouseTargetLayoutKey.from(state, viewportSize)
    mouseTargetCacheRef.modify {
      case Some(cache) if cache.layoutKey == key =>
        val scene = AuthoritativeUiScene.forState(state, viewportSize)
        val next  = if cache.scene eq scene then cache else cache.copy(scene = scene)
        Some(next) -> next
      case _ =>
        val next = MouseTargetCache.fromState(state, viewportSize)
        Some(next) -> next
    }

  private def mouseTargetSnapshot(
    cache: MouseTargetCache,
    paneId: PaneId
  ): IO[TextLayoutSnapshot] =
    cache.scene.textSnapshot(paneId) match
      case Some(snapshot) => IO.pure(snapshot)
      case None           => IO.raiseError(new IllegalStateException(s"missing text snapshot for pane $paneId"))

  final private case class RopeCharacterSource(content: com.serenity.rope.Rope) extends TextEditing.CharacterSource:
    override def length: Int =
      content.weight

    override def charAt(index: Int): Char =
      content.index(index).getOrElse('\u0000')
