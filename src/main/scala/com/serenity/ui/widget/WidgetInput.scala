package com.serenity.ui.widget

import com.serenity.keystroke.events.{
  DeleteBackward as DeleteBackwardEvent,
  DeleteForward as DeleteForwardEvent,
  DeleteWordBackward as DeleteWordBackwardEvent,
  DeleteWordForward as DeleteWordForwardEvent,
  Enter,
  Escape,
  Event,
  InsertChar,
  MoveDown,
  MoveLeft,
  MoveRight,
  MoveToEnd,
  MoveToEndOfFile,
  MoveToStart,
  MoveToStartOfFile,
  MoveUp,
  MoveWordLeft,
  MoveWordRight,
  NewLine,
  PageDown as PageDownEvent,
  PageUp as PageUpEvent,
  ScrollDown,
  ScrollUp,
  SelectAll as SelectAllEvent
}

/** What a user asked a focused widget to do, independent of which widget it is. Each widget decides what an input means
  * to it and ignores the rest: a list ignores `Insert`, a text field ignores `Toggle`.
  */
enum WidgetInput:
  case Up
  case Down
  case Left
  case Right
  case PageUp
  case PageDown
  case First
  case Last
  case WordLeft
  case WordRight
  case Activate
  case Toggle
  case Dismiss
  case Insert(char: Char)
  case InsertText(text: String)
  case DeleteBackward
  case DeleteForward
  case DeleteWordBackward
  case DeleteWordForward
  case SelectAll

  /** The mouse wheel: positive scrolls towards the end. */
  case Scroll(lines: Int)

  /** A click on the item at `index`, counting a double-click as `clicks = 2`. */
  case Click(index: Int, clicks: Int)

  /** The pointer resting over the item at `index`, or over no item. */
  case Hover(index: Option[Int])

object WidgetInput:

  /** Space arrives as a typed character; a widget that toggles reads it as `Toggle`, one that edits text as `Insert`.
    */
  def fromEvent(event: Event): Option[WidgetInput] =
    event match
      case MoveUp                          => Some(Up)
      case MoveDown                        => Some(Down)
      case MoveLeft                        => Some(Left)
      case MoveRight                       => Some(Right)
      case PageUpEvent                     => Some(PageUp)
      case PageDownEvent                   => Some(PageDown)
      case MoveToStart | MoveToStartOfFile => Some(First)
      case MoveToEnd | MoveToEndOfFile     => Some(Last)
      case MoveWordLeft                    => Some(WordLeft)
      case MoveWordRight                   => Some(WordRight)
      case Enter | NewLine                 => Some(Activate)
      case Escape                          => Some(Dismiss)
      case InsertChar(char)                => Some(Insert(char))
      case DeleteBackwardEvent             => Some(DeleteBackward)
      case DeleteForwardEvent              => Some(DeleteForward)
      case DeleteWordBackwardEvent         => Some(DeleteWordBackward)
      case DeleteWordForwardEvent          => Some(DeleteWordForward)
      case SelectAllEvent                  => Some(SelectAll)
      case ScrollDown(lines)               => Some(Scroll(lines))
      case ScrollUp(lines)                 => Some(Scroll(-lines))
      case _                               => None

  /** Space as a toggle, for widgets that have no text to type it into. */
  def asToggle(input: WidgetInput): WidgetInput =
    input match
      case Insert(' ') => Toggle
      case other       => other
