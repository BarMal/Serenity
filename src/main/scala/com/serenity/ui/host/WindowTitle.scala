package com.serenity.ui.host

import com.serenity.state.models.{AppState, Buffer}

/** The window title, in the GUI and the terminal alike: the active buffer's name, a `*` while it has unsaved changes,
  * and the application name. It is read off the document's identity only -- never its text -- because the title is
  * handed to the window system or terminal emulator, which may show, log or announce it.
  */
object WindowTitle:

  private val ApplicationName = "Serenity"

  def from(state: AppState): String =
    state.activeBuffer.fold(ApplicationName)(buffer => s"${name(buffer)}${dirtyMarker(buffer)} — $ApplicationName")

  private def name(buffer: Buffer): String =
    buffer.document.filePath
      .flatMap(path => Option(path.getFileName))
      .fold("Untitled")(_.toString)

  private def dirtyMarker(buffer: Buffer): String =
    if buffer.document.isDirty then "*" else ""
