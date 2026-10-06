package com.serenity.state.manager

import cats.effect.IO
import com.serenity.command.ReopenWithEncodingCommands
import com.serenity.io.FileManager
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import com.serenity.text.TextEncoding

/** The decisions behind "Reopen with Encoding" (#1627): which encodings to offer, when to ask before unsaved edits are
  * thrown away, and what to say when the file's bytes don't fit the one picked.
  */
object ReopenWithEncoding:

  val PickerTitle: String = "Reopen with Encoding"

  /** A picker of every encoding for the focused buffer, or `None` when it has no file to read again. */
  def withPickerOpened(state: AppState): Option[AppState] =
    state.focusedBufferId.flatMap(state.persisted.buffers.get).filter(_.document.filePath.isDefined).map { buffer =>
      val choices = TextEncoding.values.toList.map(encoding =>
        ListChoice(
          encoding.configKey,
          Option.when(encoding == buffer.document.encoding)("current"),
          ReopenWithEncodingCommands.reopen(buffer.id, encoding)
        )
      )
      ModalStateReducer.show(Modal.ListPicker(ListPicker.of(PickerTitle, choices, "No encodings")), state).state
    }

  /** The prompt asking before unsaved edits are lost, or `None` when the reopen can go ahead. */
  def withDiscardConfirmation(
    state: AppState,
    bufferId: BufferId,
    encoding: TextEncoding,
    discardEdits: Boolean
  ): Option[AppState] =
    state.persisted.buffers.get(bufferId).filter(buffer => !discardEdits && buffer.hasUnsavedChanges).map { buffer =>
      val prompt = ConfirmPrompt.reopenDiscardingEdits(bufferId, label(buffer), encoding)
      ModalStateReducer.show(Modal.Confirm(prompt), state).state
    }

  def withFailureShown(state: AppState, bufferId: BufferId, encoding: TextEncoding): AppState =
    state.persisted.buffers.get(bufferId).fold(state) { buffer =>
      ModalStateReducer.show(Modal.Confirm(ConfirmPrompt.reopenFailed(label(buffer), encoding)), state).state
    }

  private def label(buffer: Buffer): String =
    buffer.document.filePath
      .map(path => Option(path.getFileName).fold(path.toString)(_.toString))
      .getOrElse(s"Buffer ${buffer.id.value}")

/** Runs a reopen: the prompt when edits would be lost, otherwise the read on the file's lane, whose result -- the
  * reloaded buffer, or the failure -- comes back through the dispatcher like any other reload.
  */
final private[manager] class ReopenWithEncodingEffects(
    currentState: IO[AppState],
    commitState: (AppState, AppState) => IO[Unit],
    lanes: EffectLanePort,
    fileManager: FileManager
):

  def chooseEncoding: IO[Unit] =
    currentState.flatMap(current => ReopenWithEncoding.withPickerOpened(current).fold(IO.unit)(commitState(_, current)))

  def reopen(bufferId: BufferId, encoding: TextEncoding, discardEdits: Boolean): IO[Unit] =
    currentState.flatMap { state =>
      ReopenWithEncoding.withDiscardConfirmation(state, bufferId, encoding, discardEdits) match
        case Some(prompted) => commitState(prompted, state)
        case None =>
          state.persisted.buffers
            .get(bufferId)
            .flatMap(buffer => buffer.document.filePath.map(buffer -> _))
            .fold(IO.unit) { (buffer, path) =>
              lanes.submitEffect(
                StateManagerFilePersistence.fileLane(path),
                fileManager.reloadBufferAs(buffer, encoding).attempt.flatMap { read =>
                  val result = read.fold(
                    _ => EffectResult.FileReopenFailed(bufferId, encoding),
                    disk => EffectResult.FileReloaded(bufferId, path, buffer.document.content, disk)
                  )
                  lanes.dispatchEffectResult(result, _ => IO.unit)
                }
              )
            }
    }
