package com.serenity.app

import com.serenity.state.models.AppState

private[serenity] object AppRuntimeDiagnostics:

  private[serenity] def describeStateForDiagnostics(state: AppState): String =
    val viewport   = state.runtime.viewportSize.map(size => s"${size.width}x${size.height}").getOrElse("unknown")
    val activePane = state.persisted.layout.activeEditorPaneId
    val activeBuffer =
      activePane.flatMap(paneId =>
        state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId).flatMap(state.persisted.buffers.get)
      )
    val activeBufferSummary = activeBuffer match
      case Some(buffer) =>
        val language = buffer.document.language.map(_.id).getOrElse("plaintext")
        val cursor   = buffer.editing.cursorPositions.headOption.map(c => s"${c.line}:${c.column}").getOrElse("none")
        List(
          s"activeBuffer=${buffer.id}",
          s"chars=${buffer.document.content.weight}",
          s"lines=${buffer.document.content.lineCount}",
          s"dirty=${buffer.document.isDirty}",
          s"language=$language",
          s"cursor=$cursor"
        ).mkString(" ")
      case None =>
        "activeBuffer=none"
    List(
      s"focus=${state.persisted.focus}",
      s"viewport=$viewport",
      s"buffers=${state.persisted.buffers.size}",
      s"panes=${state.persisted.layout.editorPanes.size}",
      s"surfaces=${state.runtime.uiSurfaces.size}",
      s"activePane=${activePane.map(_.toString).getOrElse("none")}",
      activeBufferSummary
    ).mkString(" ")
