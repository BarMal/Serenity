package com.serenity.keystroke.events

import com.serenity.ui.layout.CellMetrics

sealed trait MouseInputEvent extends Event:
  def col: Int
  def row: Int
  def pixelX: Option[Int]
  def pixelY: Option[Int]
  def shiftDown: Boolean
  def button: MouseButton

enum MouseButton:
  case Primary, Secondary, Middle, Other

/** Metrics used by the renderer when this mouse input was captured. */
final case class MouseRenderMetrics(code: CellMetrics, ui: CellMetrics)

final case class MouseClick(
    col: Int,
    row: Int,
    pixelX: Option[Int] = None,
    pixelY: Option[Int] = None,
    clickCount: Int = 1,
    shiftDown: Boolean = false,
    button: MouseButton = MouseButton.Primary,
    renderMetrics: Option[MouseRenderMetrics] = None
) extends MouseInputEvent

final case class MousePress(
    col: Int,
    row: Int,
    pixelX: Option[Int] = None,
    pixelY: Option[Int] = None,
    shiftDown: Boolean = false,
    button: MouseButton = MouseButton.Primary
) extends MouseInputEvent

final case class MouseDrag(
    col: Int,
    row: Int,
    pixelX: Option[Int] = None,
    pixelY: Option[Int] = None,
    shiftDown: Boolean = false,
    button: MouseButton = MouseButton.Primary
) extends MouseInputEvent

final case class MouseMove(
    col: Int,
    row: Int,
    pixelX: Option[Int] = None,
    pixelY: Option[Int] = None,
    shiftDown: Boolean = false,
    button: MouseButton = MouseButton.Primary
) extends MouseInputEvent
