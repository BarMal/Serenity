package com.serenity.state.manager

import cats.effect.IO
import com.serenity.command.LineEndingCommands
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import com.serenity.text.LineEnding

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

  /** The mixed-line-endings prompt for `bufferId`, when it was loaded mixed and the user has not chosen an ending. */
  def withMixedNoticeIfNeeded(state: AppState, bufferId: BufferId): AppState =
    state.persisted.buffers
      .get(bufferId)
      .flatMap(buffer => buffer.document.mixedLineEndings.map(buffer -> _))
      .fold(state) { (buffer, counts) =>
        val prompt = ConfirmPrompt.mixedLineEndings(bufferId, label(buffer), counts)
        ModalStateReducer.show(Modal.Confirm(prompt), state).state
      }

  private def label(buffer: Buffer): String =
    buffer.document.filePath
      .map(path => Option(path.getFileName).fold(path.toString)(_.toString))
      .getOrElse(s"Buffer ${buffer.id.value}")

final private[manager] class LineEndingEffects(
    currentState: IO[AppState],
    commitState: (AppState, AppState) => IO[Unit]
):

  def chooseLineEnding: IO[Unit] =
    currentState.flatMap(current => LineEndingChoice.withPickerOpened(current).fold(IO.unit)(commitState(_, current)))

  def setLineEnding(bufferId: BufferId, ending: LineEnding): IO[Unit] =
    currentState.flatMap(current => commitState(LineEndingChoice.withLineEnding(current, bufferId, ending), current))
