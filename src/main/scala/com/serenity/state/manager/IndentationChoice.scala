package com.serenity.state.manager

import cats.effect.IO
import com.serenity.command.IndentationCommands
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer

/** Choosing how one buffer is indented (#1996). The choice is the buffer's own and survives a reload; it does not touch
  * the text, so it is neither an edit nor an undo step.
  */
object IndentationChoice:

  val PickerTitle: String = "Change Indentation"

  /** A picker of every way to indent the focused buffer, or `None` when nothing is focused. */
  def withPickerOpened(state: AppState): Option[AppState] =
    state.focusedBufferId.flatMap(state.persisted.buffers.get).map { buffer =>
      val chosen    = buffer.document.indentation.chosen.complete
      val effective = buffer.effectiveIndentation(state.persisted.config.indentation)
      val automatic = IndentationCommands.useAutomatic(buffer.id)
      val explicit =
        for
          insertSpaces <- List(true, false)
          tabSize      <- (IndentationSettings.MinTabSize to IndentationSettings.MaxTabSize).toList
        yield
          val settings = IndentationSettings(tabSize, insertSpaces)
          val command  = IndentationCommands.set(buffer.id, settings)
          ListChoice(command.label, Option.when(chosen.contains(settings))("current"), command)
      val choices =
        ListChoice(
          automatic.label,
          Some(if chosen.isEmpty then s"current, ${effective.statusLabel}" else effective.statusLabel),
          automatic
        ) :: explicit
      ModalStateReducer.show(Modal.ListPicker(ListPicker.filterable(PickerTitle, choices)), state).state
    }

  /** `bufferId` indented with `settings`, or as its text and the global settings say when `None`. */
  def withIndentation(state: AppState, bufferId: BufferId, settings: Option[IndentationSettings]): AppState =
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updatedWith(bufferId)(
          _.map(buffer =>
            buffer.copy(document = buffer.document.copy(indentation = buffer.document.indentation.withChosen(settings)))
          )
        )
      )
    )

final private[manager] class IndentationEffects(
    currentState: IO[AppState],
    commitState: (AppState, AppState) => IO[Unit]
):

  def chooseIndentation: IO[Unit] =
    currentState.flatMap(current => IndentationChoice.withPickerOpened(current).fold(IO.unit)(commitState(_, current)))

  def setIndentation(bufferId: BufferId, settings: Option[IndentationSettings]): IO[Unit] =
    currentState.flatMap(current =>
      commitState(IndentationChoice.withIndentation(current, bufferId, settings), current)
    )
