package com.serenity.state.manager

import com.serenity.command.ManuscriptExportCommands
import com.serenity.manuscript.ManuscriptFileFormat
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer

/** The format question behind "Export Manuscript...": a picker of the formats a manuscript can be written in. */
object ManuscriptExportPicker:

  val PickerTitle: String = "Export Manuscript"

  /** The picker, or `None` when there is no focused document to export. */
  def withPickerOpened(state: AppState): Option[AppState] =
    state.focusedBufferId.flatMap(state.persisted.buffers.get).map { _ =>
      val choices = ManuscriptFileFormat.values.toList.map(format =>
        ListChoice(format.label, Some(s".${format.extension}"), ManuscriptExportCommands.exportAs(format))
      )
      ModalStateReducer.show(Modal.ListPicker(ListPicker.of(PickerTitle, choices, "No formats")), state).state
    }
