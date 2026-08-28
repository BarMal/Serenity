package com.serenity.state.manager

import cats.effect.{IO, Ref}
import com.serenity.keystroke.events.*
import com.serenity.state.models.*

/** State the event pipeline exposes for applying a resolved editor click/press/drag target to buffer selection. */
private[manager] trait MouseHitTestingPort:
  def stateRef: Ref[IO, AppState]

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
    startupPage: StartupPageMouseHitTesting
):
  import port.*

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
                        if MouseHitTestGeometry.isInsideFloatingSurface(click, state) then IO.unit
                        else
                          pinnedPanel.handlePinnedPanelMouseClick(click, state).flatMap {
                            case true => IO.unit
                            case false =>
                              pinnedPanel.handlePinnedPanelLocationClick(click, state).flatMap {
                                case true => IO.unit
                                case false =>
                                  editorTargeting.resolveMouseTarget(click, state).flatMap {
                                    _.fold(contextMenu.dismissContextMenuIfOpen(state)) {
                                      (paneId, buffer, clickedCursor) =>
                                        stateRef.update { s =>
                                          s.persisted.buffers.get(buffer.id) match
                                            case Some(current) =>
                                              val selection =
                                                if click.shiftDown then
                                                  editorTargeting.rangeSelectionFromAnchor(current, clickedCursor)
                                                else if click.clickCount >= 3 then
                                                  editorTargeting.lineSelectionAtCursor(current, clickedCursor)
                                                else if click.clickCount >= 2 then
                                                  editorTargeting.wordSelectionAtCursor(current, clickedCursor)
                                                else None
                                              val focusCursor = selection.map(_.focus).getOrElse(clickedCursor)
                                              contextMenu.dismissContextMenu(
                                                s.copy(persisted =
                                                  s.persisted.copy(
                                                    buffers = s.persisted.buffers.updated(
                                                      buffer.id,
                                                      current.copy(editing =
                                                        current.editing.copy(
                                                          cursors = List(focusCursor),
                                                          selection = selection,
                                                          selections = Nil,
                                                          preferredColumn = Some(focusCursor.column),
                                                          preferredXPx = None,
                                                          multiCursorVerticalStates = Nil
                                                        )
                                                      )
                                                    ),
                                                    focus = Focus.EditorPane(paneId),
                                                    layout = s.persisted.layout.copy(activeEditorPaneId = Some(paneId))
                                                  )
                                                )
                                              )
                                            case None => contextMenu.dismissContextMenu(s)
                                        }
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
                pinnedPanel.handlePinnedPanelMouseSelect(press, state, focusPanel = true).flatMap {
                  case true => IO.unit
                  case false =>
                    editorTargeting.resolveMouseTarget(press, state).flatMap {
                      _.fold(IO.unit) { (paneId, buffer, pressedCursor) =>
                        stateRef.update { s =>
                          s.persisted.buffers.get(buffer.id) match
                            case Some(current) =>
                              val selection =
                                Option
                                  .when(press.shiftDown)(
                                    editorTargeting.rangeSelectionFromAnchor(current, pressedCursor)
                                  )
                                  .flatten
                              val focusCursor = selection.map(_.focus).getOrElse(pressedCursor)
                              s.copy(persisted =
                                s.persisted.copy(
                                  buffers = s.persisted.buffers.updated(
                                    buffer.id,
                                    current.copy(editing =
                                      current.editing.copy(
                                        cursors = List(focusCursor),
                                        selection = selection,
                                        selections = Nil,
                                        preferredColumn = Some(focusCursor.column),
                                        preferredXPx = None,
                                        multiCursorVerticalStates = Nil
                                      )
                                    )
                                  ),
                                  focus = Focus.EditorPane(paneId),
                                  layout = s.persisted.layout.copy(activeEditorPaneId = Some(paneId))
                                )
                              )
                            case None => s
                        }
                      }
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
              if MouseHitTestGeometry.isInsideFloatingSurface(drag, state) then IO.unit
              else
                editorTargeting.resolveMouseTarget(drag, state).flatMap {
                  _.fold(IO.unit) { (paneId, buffer, draggedCursor) =>
                    stateRef.update { s =>
                      s.persisted.buffers.get(buffer.id) match
                        case Some(current) =>
                          val anchor =
                            current.primarySelection
                              .map(_.anchor)
                              .orElse(current.editing.cursors.headOption)
                              .getOrElse(draggedCursor)
                          val selection =
                            Option.when(anchor != draggedCursor)(Selection(anchor, draggedCursor))
                          s.copy(persisted =
                            s.persisted.copy(
                              buffers = s.persisted.buffers.updated(
                                buffer.id,
                                current.copy(editing =
                                  current.editing.copy(
                                    cursors = List(draggedCursor),
                                    selection = selection,
                                    selections = Nil,
                                    preferredColumn = Some(draggedCursor.column),
                                    preferredXPx = None,
                                    multiCursorVerticalStates = Nil
                                  )
                                )
                              ),
                              focus = Focus.EditorPane(paneId),
                              layout = s.persisted.layout.copy(activeEditorPaneId = Some(paneId))
                            )
                          )
                        case None => s
                    }
                  }
                }
          }
      }

  def handleMouseMove(move: MouseMove, state: AppState): IO[Unit] =
    contextMenu.handleContextMenuMouseHover(move, state).flatMap {
      case true => editorTargeting.clearEditorHoverTarget
      case false =>
        contextualToolbar.handleContextualToolbarMouseHover(move, state).flatMap {
          case true => editorTargeting.clearEditorHoverTarget
          case false =>
            commandRunner.handleCommandRunnerMouseHover(move, state).flatMap {
              case true => editorTargeting.clearEditorHoverTarget
              case false =>
                if MouseHitTestGeometry.isInsideFloatingSurface(move, state) then editorTargeting.clearEditorHoverTarget
                else
                  pinnedPanel.handlePinnedPanelMouseHover(move, state).flatMap {
                    case true  => editorTargeting.clearEditorHoverTarget
                    case false => editorTargeting.updateEditorHoverTarget(move, state)
                  }
            }
        }
    }
