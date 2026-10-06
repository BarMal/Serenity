package com.serenity.config

import com.serenity.keystroke.events.*

enum ModalKeyAction extends KeymapEventAction[ModalInputEvent]:
  case NavigateUp
  case NavigateDown
  case NavigateLeft
  case NavigateRight
  case DeleteBackward
  case DeleteForward
  case DeleteWordBackward
  case DeleteWordForward
  case NextField
  case PreviousField
  case Submit
  case Dismiss
  case CreateDirectory
  case OpenAsProjectRoot
  case MoveItemUp
  case MoveItemDown
  case LineStart
  case LineEnd
  case First
  case Last
  case PageUp
  case PageDown
  case FindNext
  case FindPrevious
  case ToggleMatchCase
  case ToggleWholeWord
  case ToggleRegex

  def event: ModalInputEvent =
    this match
      case NavigateUp         => ModalNavigate(Direction.Up)
      case NavigateDown       => ModalNavigate(Direction.Down)
      case NavigateLeft       => ModalNavigate(Direction.Left)
      case NavigateRight      => ModalNavigate(Direction.Right)
      case DeleteBackward     => ModalDeleteBackward
      case DeleteForward      => ModalDeleteForward
      case DeleteWordBackward => ModalDeleteWordBackward
      case DeleteWordForward  => ModalDeleteWordForward
      case NextField          => ModalNextField
      case PreviousField      => ModalPreviousField
      case Submit             => ModalSubmit
      case Dismiss            => ModalDismiss
      case CreateDirectory    => ModalCreateDirectory
      case OpenAsProjectRoot  => ModalOpenAsProjectRoot
      case MoveItemUp         => ModalMove(Direction.Up)
      case MoveItemDown       => ModalMove(Direction.Down)
      case LineStart          => ModalLineStart
      case LineEnd            => ModalLineEnd
      case First              => ModalFirst
      case Last               => ModalLast
      case PageUp             => ModalPage(-1)
      case PageDown           => ModalPage(1)
      case FindNext           => ModalFindNext
      case FindPrevious       => ModalFindPrevious
      case ToggleMatchCase    => ModalToggleFindOption(com.serenity.state.models.FindOption.MatchCase)
      case ToggleWholeWord    => ModalToggleFindOption(com.serenity.state.models.FindOption.WholeWord)
      case ToggleRegex        => ModalToggleFindOption(com.serenity.state.models.FindOption.Regex)

object ModalKeyAction:

  val defaultBindings: Map[ModalKeyAction, List[HotkeyTrigger]] = Map(
    ModalKeyAction.NavigateUp     -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowUp, None, Set.empty)),
    ModalKeyAction.NavigateDown   -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowDown, None, Set.empty)),
    ModalKeyAction.NavigateLeft   -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowLeft, None, Set.empty)),
    ModalKeyAction.NavigateRight  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowRight, None, Set.empty)),
    ModalKeyAction.DeleteBackward -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set.empty)),
    ModalKeyAction.DeleteForward  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Delete, None, Set.empty)),
    ModalKeyAction.DeleteWordBackward -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set(com.serenity.keystroke.Modifier.Ctrl)),
      HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set(com.serenity.keystroke.Modifier.Alt))
    ),
    ModalKeyAction.DeleteWordForward -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Delete, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    ModalKeyAction.NextField     -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Tab, None, Set.empty)),
    ModalKeyAction.PreviousField -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ReverseTab, None, Set.empty)),
    ModalKeyAction.Submit        -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Enter, None, Set.empty)),
    ModalKeyAction.Dismiss       -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Escape, None, Set.empty)),
    ModalKeyAction.CreateDirectory -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Character, Some('n'), Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    ModalKeyAction.OpenAsProjectRoot -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Character, Some('r'), Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    ModalKeyAction.MoveItemUp -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowUp, None, Set(com.serenity.keystroke.Modifier.Alt))
    ),
    ModalKeyAction.MoveItemDown -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowDown, None, Set(com.serenity.keystroke.Modifier.Alt))
    ),
    ModalKeyAction.LineStart -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Home, None, Set.empty)),
    ModalKeyAction.LineEnd   -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.End, None, Set.empty)),
    ModalKeyAction.First -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Home, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    ModalKeyAction.Last -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.End, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    ModalKeyAction.PageUp   -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.PageUp, None, Set.empty)),
    ModalKeyAction.PageDown -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.PageDown, None, Set.empty)),
    ModalKeyAction.FindNext -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.F3, None, Set.empty)),
    ModalKeyAction.FindPrevious -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.F3, None, Set(com.serenity.keystroke.Modifier.Shift))
    ),
    // VS Code's find-widget toggles. Alt+letter reaches a terminal as ESC+letter, which `TerminalInputDecoder` reports
    // as Alt, so these work in the TUI too.
    ModalKeyAction.ToggleMatchCase -> List(altKey('c')),
    ModalKeyAction.ToggleWholeWord -> List(altKey('w')),
    ModalKeyAction.ToggleRegex     -> List(altKey('r'))
  )

  private def altKey(key: Char): HotkeyTrigger =
    HotkeyTrigger(com.serenity.keystroke.InputKey.Character, Some(key), Set(com.serenity.keystroke.Modifier.Alt))

  given KeymapActionCodec[ModalKeyAction] with
    def values: List[ModalKeyAction]                              = ModalKeyAction.values.toList
    def defaultBindings: Map[ModalKeyAction, List[HotkeyTrigger]] = ModalKeyAction.defaultBindings
