package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.state.models.*

/** Input-method composition in an editor pane (#2008). Pre-edit text is held in `Runtime.composition` and touches
  * nothing the buffer owns; only a commit edits, and it does so as an `InsertText`, so it is one edit at every cursor
  * and groups for undo like typed text.
  */
private[reducers] object ImeReducer:

  def reduce(event: ImeEvent, paneId: PaneId, pane: EditorPane, state: AppState)(using
    balance: com.serenity.rope.Balance
  ): ReducerResult =
    event match
      case ImeComposing(text, clauses, caret) => composing(text, clauses, caret, paneId, pane, state)
      case ImeCommit(text)                    => commit(text, paneId, pane, state)
      case ImeCancelled                       => ReducerResult.noEffects(ended(state))

  private def composing(
    text: String,
    clauses: List[CompositionClause],
    caret: Option[Int],
    paneId: PaneId,
    pane: EditorPane,
    state: AppState
  ): ReducerResult =
    if state.runtime.compositionRefused then ReducerResult.noEffects(state)
    else
      pane.bufferId.flatMap(state.persisted.buffers.get) match
        case None                    => ReducerResult.noEffects(state)
        case Some(_) if text.isEmpty => ReducerResult.noEffects(withComposition(state, None))
        case Some(buffer) =>
          state.runtime.composition.filter(continues(_, paneId, buffer)) match
            case Some(current) =>
              ReducerResult.noEffects(
                withComposition(state, Some(current.copy(text = text, clauses = clauses, caret = caret)))
              )
            case None if OpaqueBlockGuard.cursorOnBlock(buffer) =>
              ReducerResult.withEffect(
                state.copy(runtime = state.runtime.copy(composition = None, compositionRefused = true)),
                AppEffect.Ime(ImeEffect.Refused)
              )
            case None =>
              val started = Composition(
                paneId = paneId,
                bufferId = buffer.id,
                anchor = buffer.cursorList.head.position,
                bufferVersion = buffer.document.contentVersion,
                text = text,
                clauses = clauses,
                caret = caret
              )
              ReducerResult.noEffects(withComposition(state, Some(started)))

  /** A commit that ends a refused composition is dropped with it: the input method was told to stop, and the text it
    * commits as it does is text for a place that takes none.
    */
  private def commit(text: String, paneId: PaneId, pane: EditorPane, state: AppState)(using
    balance: com.serenity.rope.Balance
  ): ReducerResult =
    val recorded = state.runtime.composition
    val finished = ended(state)
    if state.runtime.compositionRefused || text.isEmpty then ReducerResult.noEffects(finished)
    else
      val aimed = pane.bufferId.flatMap(finished.persisted.buffers.get) match
        case Some(buffer) =>
          recorded.filter(continues(_, paneId, buffer)).fold(finished)(movedToAnchor(finished, buffer, _))
        case None => finished
      EditorEventReducer.reduce(InsertText(text), paneId, aimed)

  private def continues(composition: Composition, paneId: PaneId, buffer: Buffer): Boolean =
    composition.paneId == paneId && composition.bufferId == buffer.id

  /** The primary cursor put back where the composition began, if the buffer is exactly as it was then. */
  private def movedToAnchor(state: AppState, buffer: Buffer, composition: Composition): AppState =
    if composition.bufferVersion != buffer.document.contentVersion then state
    else
      val moved = buffer.copy(editing = buffer.editing.withPrimary(Cursor(composition.anchor, None)))
      state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers + (buffer.id -> moved)))

  private def ended(state: AppState): AppState =
    state.copy(runtime = state.runtime.copy(composition = None, compositionRefused = false))

  private def withComposition(state: AppState, composition: Option[Composition]): AppState =
    state.copy(runtime = state.runtime.copy(composition = composition))
