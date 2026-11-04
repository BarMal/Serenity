package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.state.manager.{CursorViewport, EditorGeometryProducer, ScrollBoundary}
import com.serenity.state.models.*
import com.serenity.state.reducers.{EditorEventReducer, ReducerResult}
import com.serenity.ui.layout.WrappedLineCache

class EditorPaneComponent(
    paneId: PaneId,
    wrapCache: WrappedLineCache = WrappedLineCache.Uncached
)(using balance: com.serenity.rope.Balance)
    extends TypedFocusedComponent[TextEntryEvent]:

  protected def decodeEvent(event: Event): Option[TextEntryEvent] =
    event match
      case ToggleSyntaxHighlighting  => None
      case textEvent: TextEntryEvent => Some(textEvent)
      case _                         => None

  protected def processTypedEvent(event: TextEntryEvent, currentState: AppState): ComponentResult =
    currentState.persisted.layout.editorPanes.get(paneId) match
      case Some(_) => processEventForPane(event, currentState)
      case None    => ComponentResult.noChange

  override protected def processFallbackEvent(event: Event, currentState: AppState): ComponentResult =
    event match
      case ToggleSyntaxHighlighting =>
        ComponentResult.updateState { state =>
          state.copy(persisted =
            state.persisted.copy(config =
              state.persisted.config.withSyntaxHighlighting(!state.syntaxHighlightingEnabled)
            )
          )
        }
      case _ =>
        ComponentResult.noChange

  private def processEventForPane(
    event: TextEntryEvent,
    currentState: AppState
  ): ComponentResult =
    event match
      case ScrollDown(lines) if ScrollBoundary.scrollsByRows(currentState) =>
        rowScrolled(currentState, lines)
      case ScrollUp(lines) if ScrollBoundary.scrollsByRows(currentState) =>
        rowScrolled(currentState, -lines)
      case _: ScrollDown => pageTurned(currentState, direction = 1)
      case _: ScrollUp   => pageTurned(currentState, direction = -1)
      case _ =>
        // Measured once here, at the effect boundary, rather than by the reducer reaching for
        // `EditorGeometryProducer` itself (#1676) -- `reduce` stays a pure function of state and this geometry.
        val geometry     = EditorGeometryProducer.forEvent(event, currentState, paneId, wrapCache)
        val reduced      = EditorEventReducer.reduce(event, paneId, currentState, geometry)
        val keyedIn      = currentState.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId)
        val visibleState = CursorViewport.ensureVisibleCursors(currentState, reduced.state, wrapCache, keyedIn)
        ComponentResult.reducerResult(ReducerResult(visibleState, reduced.effects))

  /** A wheel step counts on-screen rows (#2479): the reducer's step of whole lines cannot move through a document of
    * one long wrapped line.
    */
  private def rowScrolled(currentState: AppState, rows: Int): ComponentResult =
    ComponentResult.reducerResult(
      ReducerResult(ScrollBoundary.scrollRows(currentState, paneId, rows, wrapCache), Nil)
    )

  /** A notch turns a whole page in column mode whatever its line count (#2570): a fast flick is still one turn. */
  private def pageTurned(currentState: AppState, direction: Int): ComponentResult =
    ComponentResult.reducerResult(
      ReducerResult(ScrollBoundary.turnPage(currentState, paneId, direction, wrapCache), Nil)
    )

  /** The edit or caret step `event` makes with the cursor left where centring found it, for a caller that centres a
    * whole run of them once (#1985, #1879). `None` for any other event, including a key whose reduction would need the
    * pane's geometry.
    */
  def typedWithoutCentring(event: Event, currentState: AppState): Option[ReducerResult] =
    event match
      case step: TextEntryEvent
          if EditorPaneComponent.joinsRun(step) && currentState.persisted.layout.editorPanes.contains(paneId) =>
        EditorEventReducer.geometryRequirement(step, currentState) match
          case EditorEventReducer.GeometryRequirement.NotNeeded =>
            Some(EditorEventReducer.reduce(step, paneId, currentState))
          case _ => None
      case _ => None

object EditorPaneComponent:

  /** The keys a held key or a fast typist repeats that reduce to text edits or one-character caret steps without
    * reading the settled viewport, so a batch may apply a run of them and centre once at its end.
    */
  def joinsRun(event: TextEntryEvent): Boolean =
    event match
      case _: InsertChar | _: InsertText | DeleteBackward | DeleteForward | DeleteWordBackward | DeleteWordForward |
          DeleteToLineStart | DeleteToLineEnd | Enter | NewLine | MoveLeft | MoveRight =>
        true
      case _ => false
