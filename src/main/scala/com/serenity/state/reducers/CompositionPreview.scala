package com.serenity.state.reducers

import com.serenity.state.models.*

/** What is drawn while an input method composes (#2008): the buffer as it would read with the pre-edit committed, so
  * layout, wrapping and carets are the real ones. The real state is never touched; this is a copy made for the frame.
  *
  * The copy gets a content revision no real buffer has, and a new one for each pre-edit shown, so nothing that caches
  * by revision can mistake it for the text the buffer really holds, or one pre-edit's layout for the next one's.
  */
object CompositionPreview:

  /** The pre-edit's place in the displayed buffer: one line, `startColumn` to `endColumn`. */
  final case class Span(line: Int, startColumn: Int, endColumn: Int, clauses: List[CompositionClause])

  def displayed(state: AppState, revisions: CompositionRevisions): AppState =
    state.runtime.composition.flatMap(showable(state, _)) match
      case Some((composition, buffer)) =>
        val shown = displayBuffer(buffer, composition, revisions)
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (buffer.id -> shown)))
      case None => state

  def spanOf(state: AppState, buffer: Buffer): Option[Span] =
    state.runtime.composition
      .filter(composition => composition.bufferId == buffer.id && isDisplayRevision(buffer.document.contentVersion))
      .map { composition =>
        Span(
          line = composition.anchor.line,
          startColumn = composition.anchor.column,
          endColumn = composition.anchor.column + composition.text.length,
          clauses = composition.clauses
        )
      }

  private def isDisplayRevision(version: Long): Boolean = version >= CompositionRevisions.First

  /** A pre-edit is shown only against the text it began in, and only if it fits on one line. */
  private def showable(state: AppState, composition: Composition): Option[(Composition, Buffer)] =
    Option
      .when(composition.text.nonEmpty && !composition.text.contains('\n'))(composition)
      .flatMap(_ => state.persisted.buffers.get(composition.bufferId))
      .filter(_.document.contentVersion == composition.bufferVersion)
      .map(composition -> _)

  private def displayBuffer(buffer: Buffer, composition: Composition, revisions: CompositionRevisions): Buffer =
    val anchor = composition.anchor
    val offset = buffer.document.content.lineColumnToOffset(anchor.line, anchor.column)
    val (edited, _) = EditorEditSupport.applyTrackedEdits(
      buffer,
      List(offset),
      List(EditorEditSupport.MultiCursorEdit(0, offset, offset, composition.text))
    )
    val revision = revisions.of(composition)
    edited.copy(
      document = edited.document.copy(
        contentVersion = revision,
        changes = ChangeLog.at(revision),
        headingIndex = new com.serenity.document.HeadingIndexCell,
        isDirty = buffer.document.isDirty
      ),
      richText = edited.richText.copy(richTextSyncedVersion = edited.richText.richTextSyncedVersion.map(_ => revision)),
      editing = displayCursors(buffer, composition)
    )

  /** The cursors as the text now reads: those after the anchor on its line move along, and the primary one sits at the
    * pre-edit's caret.
    */
  private def displayCursors(buffer: Buffer, composition: Composition): EditingState =
    val anchor = composition.anchor
    val length = composition.text.length
    def shifted(position: CursorPosition): CursorPosition =
      if position.line == anchor.line && position.column >= anchor.column then
        position.copy(column = position.column + length)
      else position
    val caret = CursorPosition(anchor.line, anchor.column + composition.caret.getOrElse(length).max(0).min(length))
    val others = buffer.cursorList.tail.map(cursor =>
      cursor.copy(position = shifted(cursor.position), selectionAnchor = cursor.selectionAnchor.map(shifted))
    )
    EditingState.fromCursors(Cursor(caret) :: others)

/** Hands out the content revisions displayed buffers carry: above any a real buffer reaches, a new one for each
  * pre-edit shown, and the same one again for a pre-edit shown twice running, so frames that show it unchanged (a
  * blinking caret) look unchanged to a renderer comparing them. Owned by the render caches, whose entries it keys.
  */
final class CompositionRevisions:
  private val next = new java.util.concurrent.atomic.AtomicLong(CompositionRevisions.First)
  private val last = new java.util.concurrent.atomic.AtomicReference[Option[(Composition, Long)]](None)

  def of(composition: Composition): Long =
    last.get().filter(_._1 == composition).map(_._2).getOrElse {
      val fresh = next.incrementAndGet()
      last.set(Some(composition -> fresh))
      fresh
    }

object CompositionRevisions:
  val First: Long = Long.MaxValue / 2
