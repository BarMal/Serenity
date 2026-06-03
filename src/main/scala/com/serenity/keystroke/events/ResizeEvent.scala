package com.serenity.keystroke.events

import com.serenity.ui.layout.ViewportSize

case class ResizeEvent(newSize: ViewportSize) extends SystemEvent
