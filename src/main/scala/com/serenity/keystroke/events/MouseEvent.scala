package com.serenity.keystroke.events

sealed trait MouseInputEvent extends Event:
  def col: Int
  def row: Int
  def pixelX: Option[Int]
  def pixelY: Option[Int]
  def shiftDown: Boolean
  def button: MouseButton

enum MouseButton:
  case Primary, Secondary, Middle, Other

case class MouseClick(
    col: Int,
    row: Int,
    pixelX: Option[Int] = None,
    pixelY: Option[Int] = None,
    clickCount: Int = 1,
    shiftDown: Boolean = false,
    button: MouseButton = MouseButton.Primary
) extends MouseInputEvent

case class MousePress(
    col: Int,
    row: Int,
    pixelX: Option[Int] = None,
    pixelY: Option[Int] = None,
    shiftDown: Boolean = false,
    button: MouseButton = MouseButton.Primary
) extends MouseInputEvent

case class MouseDrag(
    col: Int,
    row: Int,
    pixelX: Option[Int] = None,
    pixelY: Option[Int] = None,
    shiftDown: Boolean = false,
    button: MouseButton = MouseButton.Primary
) extends MouseInputEvent

case class MouseMove(
    col: Int,
    row: Int,
    pixelX: Option[Int] = None,
    pixelY: Option[Int] = None,
    shiftDown: Boolean = false,
    button: MouseButton = MouseButton.Primary
) extends MouseInputEvent
