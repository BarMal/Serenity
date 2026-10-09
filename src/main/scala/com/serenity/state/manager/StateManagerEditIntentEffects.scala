package com.serenity.state.manager

import cats.effect.IO
import com.serenity.command.*
import com.serenity.keystroke.events.Event
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.widget.TextField
import org.typelevel.log4cats.Logger

final private[manager] class StateManagerEditIntentEffects(
    logger: Logger[IO],
    enqueueEvent: Event => IO[Unit],
    updateModelValidated: (Model => Option[Model]) => IO[Unit],
    activeEditorBufferId: AppState => Option[BufferId]
):

  def interpret(intent: EditIntent): IO[Unit] =
    intent match
      case EditIntent.FindInCurrentFile =>
        showModalValidated(findModalForState)
      case EditIntent.FindAllInCurrentFile =>
        showModalValidated(findModalForState)
      case EditIntent.ReplaceInCurrentFile =>
        showModalValidated(_ => Modal.ReplaceWorkflow(ReplaceWorkflowState()))
      case EditIntent.ReplaceAllInCurrentFile =>
        showModalValidated(_ =>
          Modal.ReplaceWorkflow(ReplaceWorkflowState(selectedAction = ReplaceWorkflowAction.ReplaceAll))
        )
      case EditIntent.Copy =>
        enqueueEvent(com.serenity.keystroke.events.Copy)
      case EditIntent.Cut =>
        enqueueEvent(com.serenity.keystroke.events.Cut)
      case EditIntent.Paste =>
        enqueueEvent(com.serenity.keystroke.events.Paste)
      case EditIntent.ChoosePasteFromHistory =>
        showModalValidated(ClipboardHistoryPicker.modalFor)
      case EditIntent.PasteFromHistory(entry) =>
        enqueueEvent(com.serenity.keystroke.events.PasteFromHistory(entry))
      case EditIntent.SelectAll =>
        enqueueEvent(com.serenity.keystroke.events.SelectAll)
      case EditIntent.Undo =>
        enqueueEvent(com.serenity.keystroke.events.Undo)
      case EditIntent.Redo =>
        enqueueEvent(com.serenity.keystroke.events.Redo)
      case EditIntent.FormatCurrentFile =>
        logger.debug("[CMD] Format command requested")

  // Read inside the validated model write rather than from a snapshot: a command can run off the dispatcher.
  private def showModalValidated(modalFor: AppState => Modal): IO[Unit] =
    updateModelValidated(model => Some(model.copy(app = ModalStateReducer.show(modalFor(model.app), model.app).state)))

  private def findModalForState(state: AppState): Modal =
    activeEditorBufferId(state)
      .flatMap(state.persisted.buffers.get)
      .flatMap { buffer =>
        buffer.findState.filter(_.query.nonEmpty).map { found =>
          val caret     = buffer.editing.cursors.head.position
          val content   = buffer.document.content
          val anchor    = content.lineColumnToOffset(caret.line, caret.column)
          val matches   = FindSearch.search(content, found.query, found.options, anchor)
          val resultSet = FindResultSet.normalized(found.query, matches.results, found.currentIndex, matches.capped)
          Modal.Find(
            TextField.of(resultSet.query),
            resultSet.results,
            resultSet.currentIndex,
            found.options,
            resultSet.capped
          )
        }
      }
      .getOrElse(Modal.Find(TextField(), Vector.empty, 0))
