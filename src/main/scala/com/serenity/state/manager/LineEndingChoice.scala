package com.serenity.state.manager

import cats.effect.IO
import com.serenity.command.LineEndingCommands
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import com.serenity.state.undo.HistoryEntry
import com.serenity.text.{LineEnding, LineEndingCounts}

/** The decisions behind line endings (#1964): the picker, applying a choice, and telling the user when a file they
  * opened is mixed, since saving writes one ending throughout.
  */
object LineEndingChoice:

  val PickerTitle: String = "Change Line Ending"

  /** A picker of every line ending for the focused buffer, or `None` when nothing is focused. */
  def withPickerOpened(state: AppState): Option[AppState] =
    state.focusedBufferId.flatMap(state.persisted.buffers.get).map { buffer =>
      val choices = LineEnding.values.toList.map(ending =>
        ListChoice(
          ending.label,
          Option.when(ending == buffer.document.lineEnding)("current"),
          LineEndingCommands.set(buffer.id, ending)
        )
      )
      ModalStateReducer.show(Modal.ListPicker(ListPicker.of(PickerTitle, choices, "No line endings")), state).state
    }

  /** `bufferId` saving with `ending`, and dirty if that changes what a save writes. */
  def withLineEnding(state: AppState, bufferId: BufferId, ending: LineEnding): AppState =
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updatedWith(bufferId)(
          _.map(buffer => buffer.copy(document = buffer.document.withLineEnding(ending)))
        )
      )
    )

  /** [[withLineEnding]] on the model, recorded as one undo step when it changes anything. */
  def changed(model: Model, bufferId: BufferId, ending: LineEnding): Model =
    model.app.persisted.buffers
      .get(bufferId)
      .filter(before => before.document.withLineEnding(ending) ne before.document)
      .fold(model) { before =>
        val undo = UndoRecording.recorded(model.undo, HistoryEntry.LineEndingChange.capture(before), groupable = false)
        model.copy(app = withLineEnding(model.app, bufferId, ending), undo = undo)
      }

  /** The mixed-line-endings prompt for the first buffer still owed one. Held back while a modal has the focus, so the
    * notice is not lost behind it: the next commit after it closes shows it.
    */
  def withPendingNotice(state: AppState): AppState =
    if state.isModalFocus then state
    else
      state.persisted.buffers.values
        .find(buffer => buffer.document.mixedNoticePending && buffer.document.mixedLineEndings.isDefined)
        .fold(state) { buffer =>
          val told = buffer.copy(document = buffer.document.copy(mixedNoticePending = false))
          val withTold =
            state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(buffer.id, told)))
          buffer.document.mixedLineEndings.fold(withTold) { counts =>
            val prompt = ConfirmPrompt.mixedLineEndings(buffer.id, label(buffer), counts)
            ModalStateReducer.show(Modal.Confirm(prompt), withTold).state
          }
        }

  /** After a save that wrote `written` over a mixed file the user never chose an ending for: what changed. */
  def withSavedMixedNotice(
    state: AppState,
    bufferId: BufferId,
    written: LineEnding,
    counts: LineEndingCounts
  ): AppState =
    state.persisted.buffers.get(bufferId).fold(state) { buffer =>
      val prompt = ConfirmPrompt.savedMixedLineEndings(label(buffer), written, counts)
      ModalStateReducer.show(Modal.Confirm(prompt), state).state
    }

  private def label(buffer: Buffer): String =
    buffer.document.filePath
      .map(path => Option(path.getFileName).fold(path.toString)(_.toString))
      .getOrElse(s"Buffer ${buffer.id.value}")

final private[manager] class LineEndingEffects(
    currentState: IO[AppState],
    commitState: (AppState, AppState) => IO[Unit],
    updateModelValidated: (Model => Option[Model]) => IO[Unit]
):

  def chooseLineEnding: IO[Unit] =
    currentState.flatMap(current => LineEndingChoice.withPickerOpened(current).fold(IO.unit)(commitState(_, current)))

  def setLineEnding(bufferId: BufferId, ending: LineEnding): IO[Unit] =
    updateModelValidated(model => Some(LineEndingChoice.changed(model, bufferId, ending)))
