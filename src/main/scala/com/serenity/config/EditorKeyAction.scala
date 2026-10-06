package com.serenity.config

import com.serenity.keystroke.events.*

enum EditorKeyAction extends KeymapEventAction[EditorEvent]:
  case MoveLeft
  case MoveRight
  case MoveUp
  case MoveDown
  case ExtendSelectionLeft
  case ExtendSelectionRight
  case ExtendSelectionUp
  case ExtendSelectionDown
  case MoveWordLeft
  case MoveWordRight
  case ExtendSelectionWordLeft
  case ExtendSelectionWordRight
  case MoveSubWordLeft
  case MoveSubWordRight
  case ExtendSelectionSubWordLeft
  case ExtendSelectionSubWordRight
  case ExtendSelectionToLineStart
  case ExtendSelectionToLineEnd
  case ExtendSelectionPageUp
  case ExtendSelectionPageDown
  case MoveToStart
  case MoveToEnd
  case MoveToStartOfFile
  case MoveToEndOfFile
  case PageUp
  case PageDown
  case DeleteBackward
  case DeleteForward
  case DeleteWordBackward
  case DeleteWordForward
  case Escape
  case NewLine
  case Tab
  case ReverseTab

  def event: EditorEvent =
    this match
      case MoveLeft            => com.serenity.keystroke.events.MoveLeft
      case MoveRight           => com.serenity.keystroke.events.MoveRight
      case MoveUp              => com.serenity.keystroke.events.MoveUp
      case MoveDown            => com.serenity.keystroke.events.MoveDown
      case ExtendSelectionLeft => com.serenity.keystroke.events.ExtendSelectionLeft
      case ExtendSelectionRight =>
        com.serenity.keystroke.events.ExtendSelectionRight
      case ExtendSelectionUp   => com.serenity.keystroke.events.ExtendSelectionUp
      case ExtendSelectionDown => com.serenity.keystroke.events.ExtendSelectionDown
      case MoveWordLeft        => com.serenity.keystroke.events.MoveWordLeft
      case MoveWordRight       => com.serenity.keystroke.events.MoveWordRight
      case ExtendSelectionWordLeft =>
        com.serenity.keystroke.events.ExtendSelectionWordLeft
      case ExtendSelectionWordRight =>
        com.serenity.keystroke.events.ExtendSelectionWordRight
      case MoveSubWordLeft  => com.serenity.keystroke.events.MoveSubWordLeft
      case MoveSubWordRight => com.serenity.keystroke.events.MoveSubWordRight
      case ExtendSelectionSubWordLeft =>
        com.serenity.keystroke.events.ExtendSelectionSubWordLeft
      case ExtendSelectionSubWordRight =>
        com.serenity.keystroke.events.ExtendSelectionSubWordRight
      case ExtendSelectionToLineStart =>
        com.serenity.keystroke.events.ExtendSelectionToLineStart
      case ExtendSelectionToLineEnd =>
        com.serenity.keystroke.events.ExtendSelectionToLineEnd
      case ExtendSelectionPageUp =>
        com.serenity.keystroke.events.ExtendSelectionPageUp
      case ExtendSelectionPageDown =>
        com.serenity.keystroke.events.ExtendSelectionPageDown
      case MoveToStart        => com.serenity.keystroke.events.MoveToStart
      case MoveToEnd          => com.serenity.keystroke.events.MoveToEnd
      case MoveToStartOfFile  => com.serenity.keystroke.events.MoveToStartOfFile
      case MoveToEndOfFile    => com.serenity.keystroke.events.MoveToEndOfFile
      case PageUp             => com.serenity.keystroke.events.PageUp
      case PageDown           => com.serenity.keystroke.events.PageDown
      case DeleteBackward     => com.serenity.keystroke.events.DeleteBackward
      case DeleteForward      => com.serenity.keystroke.events.DeleteForward
      case DeleteWordBackward => com.serenity.keystroke.events.DeleteWordBackward
      case DeleteWordForward  => com.serenity.keystroke.events.DeleteWordForward
      case Escape             => com.serenity.keystroke.events.Escape
      case NewLine            => com.serenity.keystroke.events.NewLine
      case Tab                => com.serenity.keystroke.events.TabKey
      case ReverseTab         => com.serenity.keystroke.events.ReverseTabKey

object EditorKeyAction:

  val defaultBindings: Map[EditorKeyAction, List[HotkeyTrigger]] = Map(
    EditorKeyAction.MoveLeft  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowLeft, None, Set.empty)),
    EditorKeyAction.MoveRight -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowRight, None, Set.empty)),
    EditorKeyAction.MoveUp    -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowUp, None, Set.empty)),
    EditorKeyAction.MoveDown  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowDown, None, Set.empty)),
    EditorKeyAction.ExtendSelectionLeft -> List(
      HotkeyTrigger(
        com.serenity.keystroke.InputKey.ArrowLeft,
        None,
        Set(com.serenity.keystroke.Modifier.Shift)
      )
    ),
    EditorKeyAction.ExtendSelectionRight -> List(
      HotkeyTrigger(
        com.serenity.keystroke.InputKey.ArrowRight,
        None,
        Set(com.serenity.keystroke.Modifier.Shift)
      )
    ),
    EditorKeyAction.ExtendSelectionUp -> List(
      HotkeyTrigger(
        com.serenity.keystroke.InputKey.ArrowUp,
        None,
        Set(com.serenity.keystroke.Modifier.Shift)
      )
    ),
    EditorKeyAction.ExtendSelectionDown -> List(
      HotkeyTrigger(
        com.serenity.keystroke.InputKey.ArrowDown,
        None,
        Set(com.serenity.keystroke.Modifier.Shift)
      )
    ),
    EditorKeyAction.MoveWordLeft -> List(
      HotkeyTrigger(
        com.serenity.keystroke.InputKey.ArrowLeft,
        None,
        Set(com.serenity.keystroke.Modifier.Ctrl)
      )
    ),
    EditorKeyAction.MoveWordRight -> List(
      HotkeyTrigger(
        com.serenity.keystroke.InputKey.ArrowRight,
        None,
        Set(com.serenity.keystroke.Modifier.Ctrl)
      )
    ),
    EditorKeyAction.ExtendSelectionWordLeft -> List(
      HotkeyTrigger(
        com.serenity.keystroke.InputKey.ArrowLeft,
        None,
        Set(com.serenity.keystroke.Modifier.Ctrl, com.serenity.keystroke.Modifier.Shift)
      )
    ),
    EditorKeyAction.ExtendSelectionWordRight -> List(
      HotkeyTrigger(
        com.serenity.keystroke.InputKey.ArrowRight,
        None,
        Set(com.serenity.keystroke.Modifier.Ctrl, com.serenity.keystroke.Modifier.Shift)
      )
    ),
    EditorKeyAction.MoveSubWordLeft -> List(
      HotkeyTrigger(
        com.serenity.keystroke.InputKey.ArrowLeft,
        None,
        Set(com.serenity.keystroke.Modifier.Ctrl, com.serenity.keystroke.Modifier.Alt)
      )
    ),
    EditorKeyAction.MoveSubWordRight -> List(
      HotkeyTrigger(
        com.serenity.keystroke.InputKey.ArrowRight,
        None,
        Set(com.serenity.keystroke.Modifier.Ctrl, com.serenity.keystroke.Modifier.Alt)
      )
    ),
    EditorKeyAction.ExtendSelectionSubWordLeft -> List(
      HotkeyTrigger(
        com.serenity.keystroke.InputKey.ArrowLeft,
        None,
        Set(
          com.serenity.keystroke.Modifier.Ctrl,
          com.serenity.keystroke.Modifier.Alt,
          com.serenity.keystroke.Modifier.Shift
        )
      )
    ),
    EditorKeyAction.ExtendSelectionSubWordRight -> List(
      HotkeyTrigger(
        com.serenity.keystroke.InputKey.ArrowRight,
        None,
        Set(
          com.serenity.keystroke.Modifier.Ctrl,
          com.serenity.keystroke.Modifier.Alt,
          com.serenity.keystroke.Modifier.Shift
        )
      )
    ),
    EditorKeyAction.MoveToStart -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Home, None, Set.empty)),
    EditorKeyAction.MoveToEnd   -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.End, None, Set.empty)),
    EditorKeyAction.ExtendSelectionToLineStart -> List(
      HotkeyTrigger(
        com.serenity.keystroke.InputKey.Home,
        None,
        Set(com.serenity.keystroke.Modifier.Shift)
      )
    ),
    EditorKeyAction.ExtendSelectionToLineEnd -> List(
      HotkeyTrigger(
        com.serenity.keystroke.InputKey.End,
        None,
        Set(com.serenity.keystroke.Modifier.Shift)
      )
    ),
    EditorKeyAction.MoveToStartOfFile -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Home, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    EditorKeyAction.MoveToEndOfFile -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.End, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    EditorKeyAction.PageUp   -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.PageUp, None, Set.empty)),
    EditorKeyAction.PageDown -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.PageDown, None, Set.empty)),
    EditorKeyAction.ExtendSelectionPageUp -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.PageUp, None, Set(com.serenity.keystroke.Modifier.Shift))
    ),
    EditorKeyAction.ExtendSelectionPageDown -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.PageDown, None, Set(com.serenity.keystroke.Modifier.Shift))
    ),
    EditorKeyAction.DeleteBackward -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set.empty)),
    EditorKeyAction.DeleteForward  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Delete, None, Set.empty)),
    EditorKeyAction.DeleteWordBackward -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set(com.serenity.keystroke.Modifier.Ctrl)),
      // Alt+Backspace as the readline-standard alternate: many legacy terminals (e.g. Git Bash/MSYS) collapse
      // Ctrl+Backspace to a plain Backspace (#1320) but do deliver Alt+Backspace as ESC+DEL.
      HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set(com.serenity.keystroke.Modifier.Alt))
    ),
    EditorKeyAction.DeleteWordForward -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Delete, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    EditorKeyAction.Escape     -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Escape, None, Set.empty)),
    EditorKeyAction.NewLine    -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Enter, None, Set.empty)),
    EditorKeyAction.Tab        -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Tab, None, Set.empty)),
    EditorKeyAction.ReverseTab -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ReverseTab, None, Set.empty))
  )

  given KeymapActionCodec[EditorKeyAction] with
    def values: List[EditorKeyAction]                              = EditorKeyAction.values.toList
    def defaultBindings: Map[EditorKeyAction, List[HotkeyTrigger]] = EditorKeyAction.defaultBindings
