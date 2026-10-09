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

  /** A typed character's edit with the cursor left where centring found it, for a caller that centres a whole run of
    * them once (#1985). `None` for anything else, including a key whose reduction would need the pane's geometry.
    */
  def typedWithoutCentring(event: Event, currentState: AppState): Option[ReducerResult] =
    event match
      case typed: InsertChar if currentState.persisted.layout.editorPanes.contains(paneId) =>
        EditorEventReducer.geometryRequirement(typed, currentState) match
          case EditorEventReducer.GeometryRequirement.NotNeeded =>
            Some(EditorEventReducer.reduce(typed, paneId, currentState))
          case _ => None
      case _ => None
