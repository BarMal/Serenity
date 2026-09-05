package com.serenity.state.models

/** Builds the mode/tab corner widget's tab list (issue #1307) from the same buffer-order and focus data the editor
  * itself uses -- there is no separate "open tabs" state to keep in sync.
  */
object TabListContent:

  def build(state: AppState): SurfaceContent.TabList =
    val entries = state.persisted.bufferOrder.flatMap { bufferId =>
      state.persisted.buffers
        .get(bufferId)
        .map(buffer => TabListEntry(bufferId, titleFor(buffer), buffer.document.isDirty))
    }
    SurfaceContent.TabList(entries, state.focusedBufferId)

  /** Mirrors `Renderer.renderBufferHeader`'s title, minus its own "- unsaved" text suffix: the widget carries dirty
    * state as its own field, rendered as a persistent glyph rather than appended text (issue #1307's own ask).
    */
  private def titleFor(buffer: Buffer): String =
    buffer.document.filePath.map(_.getFileName.toString).getOrElse(s"Buffer ${buffer.id.value}")
