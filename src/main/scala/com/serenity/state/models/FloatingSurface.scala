package com.serenity.state.models

import com.serenity.command.CommandRunner
import com.serenity.ui.layout.PeekContent

enum FloatingSurfacePlacement:
  case AboveCursor
  case BelowCursor

enum FloatingSurfaceContent:
  case Peek(content: PeekContent, dismissOnMove: Boolean)
  case CommandPalette(runner: CommandRunner)
  case ModalWorkflow(modal: Modal)

case class FloatingSurface(
    anchor: Option[CursorPosition],
    placement: FloatingSurfacePlacement,
    content: FloatingSurfaceContent
)
