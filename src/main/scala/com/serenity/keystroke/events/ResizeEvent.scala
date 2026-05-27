package com.serenity.keystroke.events

import com.serenity.ui.layout.TerminalSize

case class ResizeEvent(newSize: TerminalSize) extends SystemEvent
