package com.serenity.keystroke.events

case class MouseClick(
    col: Int,
    row: Int,
    pixelX: Option[Int] = None,
    pixelY: Option[Int] = None
) extends Event
