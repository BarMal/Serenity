package com.serenity.state.components

import com.serenity.keystroke.events.*
import com.serenity.state.manager.{CursorViewport, EditorGeometryProducer}
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
    // Measured once here, at the effect boundary, rather than by the reducer reaching for
    // `EditorGeometryProducer` itself (#1676) -- `reduce` stays a pure function of state and this geometry.
    val geometry     = EditorGeometryProducer.forEvent(event, currentState, paneId, wrapCache)
    val reduced      = EditorEventReducer.reduce(event, paneId, currentState, geometry)
    val visibleState = CursorViewport.ensureVisibleCursors(currentState, reduced.state, wrapCache)
    ComponentResult.reducerResult(ReducerResult(visibleState, reduced.effects))

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
      case _: InsertChar | DeleteBackward | DeleteForward | DeleteWordBackward | DeleteWordForward | DeleteToLineStart |
          DeleteToLineEnd | Enter | NewLine | MoveLeft | MoveRight =>
        true
      case _ => false
