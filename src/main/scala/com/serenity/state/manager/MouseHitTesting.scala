package com.serenity.state.manager

import cats.effect.IO
import com.serenity.config.CommentDisplayMode
import com.serenity.document.CommentRendering
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{ReducerResult, Transition}

/** State the event pipeline exposes for applying a resolved editor click/press/drag target to buffer selection, as a
  * capability record rather than a trait -- nothing here breaks a construction-order cycle (#1389), so mockability is
  * the only reason this needs an interface at all, and a record fakes trivially without one (#1017).
  */
final private[manager] case class MouseHitTestingPort(
    currentState: IO[AppState],
    applyReducerResult: (ReducerResult, AppState) => IO[Unit]
)

/** Routes primary/secondary mouse click, press, drag, and move events to the editor, the context menu, the contextual
  * toolbar, the command palette, pinned panels, and the startup page, in the same precedence order the pipeline
  * dispatched them in before this extraction. Falls through to resolving an editor click/press/drag target (via
  * [[EditorMouseTargeting]]) only once every overlay above the editor has declined the event.
  */
final private[manager] class MouseHitTesting(
    port: MouseHitTestingPort,
    editorTargeting: EditorMouseTargeting,
    contextMenu: EditorContextMenuHitTesting,
    contextualToolbar: ContextualToolbarHitTesting,
    commandRunner: CommandRunnerMouseHitTesting,
    pinnedPanel: PinnedPanelMouseHitTesting,
    startupPage: StartupPageMouseHitTesting,
    commentLens: CommentLensMouseHitTesting,
    tabBarDrag: TabBarDragHitTesting
)(using balance: com.serenity.rope.Balance):

  private def commit[A](transition: Transition[A]): IO[A] =
    MouseTransition.commit(port.currentState, port.applyReducerResult)(transition)

  def handleMouseClick(click: MouseClick, state: AppState): IO[Unit] =
    click.button match
      case MouseButton.Secondary =>
        if MouseHitTestGeometry.isInsideFloatingSurface(click, state) then IO.unit
        else contextMenu.openEditorContextMenu(click, state)
      case MouseButton.Primary =>
        startupPage.handleStartupPageMouseClick(click, state).flatMap {
          case true => IO.unit
          case false =>
            contextMenu.handleContextMenuMouseClick(click, state).flatMap {
              case true => IO.unit
              case false =>
                contextualToolbar.handleContextualToolbarMouseClick(click, state).flatMap {
                  case true => IO.unit
                  case false =>
                    commandRunner.handleCommandRunnerMouseClick(click, state).flatMap {
                      case true => IO.unit
                      case false =>
                        commentLens.handleCommentLensMouseClick(click, state).flatMap {
                          case true => IO.unit
                          case false =>
                            commit(TabBarMouseHitTesting.click(click, state)).flatMap {
                              case true => IO.unit
                              case false =>
                                if MouseHitTestGeometry.isInsideFloatingSurface(click, state) then IO.unit
                                else
                                  pinnedPanel.handlePinnedPanelMouseClick(click, state).flatMap {
                                    case true => IO.unit
                                    case false =>
                                      pinnedPanel.handlePinnedPanelLocationClick(click, state).flatMap {
                                        case true => IO.unit
                                        case false =>
                                          editorTargeting
                                            .resolveMouseTarget(click, state)
                                            .flatMap(target => commit(MouseHitTesting.editorClick(click, target)))
                                      }
                                  }
                            }
                        }
                    }
                }
            }
        }
      case _ =>
        IO.unit

  def handleMousePress(press: MousePress, state: AppState): IO[Unit] =
    if press.button != MouseButton.Primary then IO.unit
    else
      contextualToolbar.handleContextualToolbarMouseHover(press, state).flatMap {
        case true => IO.unit
        case false =>
          commandRunner.handleCommandRunnerMouseHover(press, state).flatMap {
            case true => IO.unit
            case false =>
              if MouseHitTestGeometry.isInsideFloatingSurface(press, state) then IO.unit
              else
                tabBarDrag.handleTabBarPress(press, state).flatMap {
                  case true => IO.unit
                  case false =>
                    pinnedPanel.handlePinnedPanelMouseSelect(press, state, focusPanel = true).flatMap {
                      case true => IO.unit
                      case false =>
                        editorTargeting
                          .resolveMouseTarget(press, state)
                          .flatMap(target => commit(MouseHitTesting.editorPress(press, target)))
                    }
                }
          }
      }

  def handleMouseDrag(drag: MouseDrag, state: AppState): IO[Unit] =
    if drag.button != MouseButton.Primary then IO.unit
    else
      pinnedPanel.handleTextAreaResizeDrag(drag, state).flatMap {
        case true => IO.unit
        case false =>
          pinnedPanel.handlePinnedPanelResizeDrag(drag, state).flatMap {
            case true => IO.unit
            case false =>
              tabBarDrag.handleTabBarDrag(drag, state).flatMap {
                case true => IO.unit
                case false =>
                  if MouseHitTestGeometry.isInsideFloatingSurface(drag, state) then IO.unit
                  else
                    editorTargeting
                      .resolveMouseTarget(drag, state)
                      .flatMap(target => commit(MouseHitTesting.editorDrag(target)))
              }
          }
      }

  def handleMouseMove(move: MouseMove, state: AppState): IO[Unit] =
    val clearHover = commit(EditorMouseTargeting.hover(None))
    contextMenu.handleContextMenuMouseHover(move, state).flatMap {
      case true => clearHover
      case false =>
        contextualToolbar.handleContextualToolbarMouseHover(move, state).flatMap {
          case true => clearHover
          case false =>
            commandRunner.handleCommandRunnerMouseHover(move, state).flatMap {
              case true => clearHover
              case false =>
                if MouseHitTestGeometry.isInsideFloatingSurface(move, state) then clearHover
                else
                  pinnedPanel.handlePinnedPanelMouseHover(move, state).flatMap {
                    case true => clearHover
                    case false =>
                      editorTargeting
                        .resolveMouseTarget(move, state)
                        .flatMap(target => commit(EditorMouseTargeting.hover(target)))
                  }
            }
        }
    }

private[manager] object MouseHitTesting:

  /** A click on an editor target moves the cursor there (a double/triple click selects the word/line, a shift-click
    * extends the selection) and dismisses any open context menu; a click on no editor target only dismisses the menu.
    */
  def editorClick(click: MouseClick, target: Option[(PaneId, Buffer, CursorPosition)]): Transition[Unit] =
    target.fold(EditorContextMenuHitTesting.dismissIfOpen) { (paneId, buffer, clickedCursor) =>
      Transition.modify(applyEditorClick(_, click, paneId, buffer.id, clickedCursor))
    }

  /** A press moves the cursor (a shift-press extends the selection), so a following drag selects from there. */
  def editorPress(press: MousePress, target: Option[(PaneId, Buffer, CursorPosition)]): Transition[Unit] =
    target.fold(Transition.unit) { (paneId, buffer, pressedCursor) =>
      Transition.modify { state =>
        state.persisted.buffers.get(buffer.id).fold(state) { current =>
          val selection =
            Option.when(press.shiftDown)(EditorMouseTargeting.rangeSelectionFromAnchor(current, pressedCursor)).flatten
          placeCursor(state, paneId, current, selection.map(_.focus).getOrElse(pressedCursor), selection.map(_.anchor))
        }
      }
    }

  /** A drag extends the selection from wherever the preceding press left the cursor to the dragged-over position. */
  def editorDrag(target: Option[(PaneId, Buffer, CursorPosition)]): Transition[Unit] =
    target.fold(Transition.unit) { (paneId, buffer, draggedCursor) =>
      Transition.modify { state =>
        state.persisted.buffers.get(buffer.id).fold(state) { current =>
          val anchor =
            current.primarySelection
              .map(_.anchor)
              .orElse(Some(current.editing.cursors.head.position))
              .getOrElse(draggedCursor)
          val selection = Option.when(anchor != draggedCursor)(Selection(anchor, draggedCursor))
          placeCursor(state, paneId, current, draggedCursor, selection.map(_.anchor))
        }
      }
    }

  /** Applies a resolved editor click's cursor/selection to its buffer, dismisses any open context menu, and -- in
    * floating display mode, for a plain click inside a highlighted comment range -- layers the read-only floating lens
    * on top (#1222).
    */
  private def applyEditorClick(
    s: AppState,
    click: MouseClick,
    paneId: PaneId,
    bufferId: BufferId,
    clickedCursor: CursorPosition
  ): AppState =
    s.persisted.buffers.get(bufferId) match
      case None => EditorContextMenuHitTesting.dismissContextMenu(s)
      case Some(current) =>
        val selection =
          if click.shiftDown then EditorMouseTargeting.rangeSelectionFromAnchor(current, clickedCursor)
          else if click.clickCount >= 3 then EditorMouseTargeting.lineSelectionAtCursor(current, clickedCursor)
          else if click.clickCount >= 2 then EditorMouseTargeting.wordSelectionAtCursor(current, clickedCursor)
          else None
        val focusCursor = selection.map(_.focus).getOrElse(clickedCursor)
        val withCursor = EditorContextMenuHitTesting.dismissContextMenu(
          placeCursor(s, paneId, current, focusCursor, selection.map(_.anchor))
        )
        if opensFloatingCommentLens(click, s, current, clickedCursor) then
          CommentRendering.openLensAtCursor(withCursor, CommentLensMode.ReadOnly)
        else withCursor

  private def placeCursor(
    state: AppState,
    paneId: PaneId,
    buffer: Buffer,
    focusCursor: CursorPosition,
    anchor: Option[CursorPosition]
  ): AppState =
    state.copy(persisted =
      state.persisted.copy(
        buffers = state.persisted.buffers.updated(
          buffer.id,
          buffer.copy(editing = EditingState.fromCursors(List(Cursor(focusCursor, anchor))))
        ),
        focus = Focus.EditorPane(paneId),
        layout = state.persisted.layout.copy(activeEditorPaneId = Some(paneId))
      )
    )

  /** A plain (unmodified, single) click landing inside a highlighted `DocumentComment` range opens the read-only
    * floating lens in floating display mode (#1222) -- a double/triple click or shift-click is a word/line/range
    * selection gesture instead, and margin display mode already shows every comment persistently, so neither opens the
    * floating lens here.
    */
  private def opensFloatingCommentLens(
    click: MouseClick,
    state: AppState,
    buffer: Buffer,
    clickedCursor: CursorPosition
  ): Boolean =
    click.clickCount <= 1 && !click.shiftDown &&
      state.persisted.config.surfaceConfig.commentDisplayMode == CommentDisplayMode.Floating &&
      buffer.annotations.documentComments.exists(_.contains(clickedCursor))
