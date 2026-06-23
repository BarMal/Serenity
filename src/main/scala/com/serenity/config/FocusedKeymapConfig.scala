package com.serenity.config

import com.serenity.keystroke.events.*
import io.circe.syntax.given
import io.circe.{Decoder, Encoder}

import HotkeyConfig.given

trait KeymapEventAction[+E <: Event]:
  def configKey: String
  def event: E

enum EditorKeyAction extends KeymapEventAction[TextEntryEvent]:
  case MoveLeft
  case MoveRight
  case MoveUp
  case MoveDown
  case ExtendSelectionLeft
  case ExtendSelectionRight
  case ExtendSelectionUp
  case ExtendSelectionDown
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

  def configKey: String =
    this match
      case MoveLeft            => "move_left"
      case MoveRight           => "move_right"
      case MoveUp              => "move_up"
      case MoveDown            => "move_down"
      case ExtendSelectionLeft => "extend_selection_left"
      case ExtendSelectionRight =>
        "extend_selection_right"
      case ExtendSelectionUp   => "extend_selection_up"
      case ExtendSelectionDown => "extend_selection_down"
      case MoveToStart         => "move_to_start"
      case MoveToEnd           => "move_to_end"
      case MoveToStartOfFile   => "move_to_start_of_file"
      case MoveToEndOfFile     => "move_to_end_of_file"
      case PageUp              => "page_up"
      case PageDown            => "page_down"
      case DeleteBackward      => "delete_backward"
      case DeleteForward       => "delete_forward"
      case DeleteWordBackward  => "delete_word_backward"
      case DeleteWordForward   => "delete_word_forward"
      case Escape              => "escape"
      case NewLine             => "new_line"
      case Tab                 => "tab"
      case ReverseTab          => "reverse_tab"

  def event: TextEntryEvent =
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
      case MoveToStart         => com.serenity.keystroke.events.MoveToStart
      case MoveToEnd           => com.serenity.keystroke.events.MoveToEnd
      case MoveToStartOfFile   => com.serenity.keystroke.events.MoveToStartOfFile
      case MoveToEndOfFile     => com.serenity.keystroke.events.MoveToEndOfFile
      case PageUp              => com.serenity.keystroke.events.PageUp
      case PageDown            => com.serenity.keystroke.events.PageDown
      case DeleteBackward      => com.serenity.keystroke.events.DeleteBackward
      case DeleteForward       => com.serenity.keystroke.events.DeleteForward
      case DeleteWordBackward  => com.serenity.keystroke.events.DeleteWordBackward
      case DeleteWordForward   => com.serenity.keystroke.events.DeleteWordForward
      case Escape              => com.serenity.keystroke.events.Escape
      case NewLine             => com.serenity.keystroke.events.NewLine
      case Tab                 => com.serenity.keystroke.events.TabKey
      case ReverseTab          => com.serenity.keystroke.events.ReverseTabKey

enum CommandRunnerKeyAction extends KeymapEventAction[CommandRunnerEvent]:
  case NavigateUp
  case NavigateDown
  case NavigateLeft
  case NavigateRight
  case DeleteBackward
  case DeleteForward
  case DeleteWordBackward
  case DeleteWordForward
  case NextCategory
  case PreviousCategory
  case Submit
  case Dismiss

  def configKey: String =
    this match
      case NavigateUp         => "navigate_up"
      case NavigateDown       => "navigate_down"
      case NavigateLeft       => "navigate_left"
      case NavigateRight      => "navigate_right"
      case DeleteBackward     => "delete_backward"
      case DeleteForward      => "delete_forward"
      case DeleteWordBackward => "delete_word_backward"
      case DeleteWordForward  => "delete_word_forward"
      case NextCategory       => "next_category"
      case PreviousCategory   => "previous_category"
      case Submit             => "submit"
      case Dismiss            => "dismiss"

  def event: CommandRunnerEvent =
    this match
      case NavigateUp         => RunnerNavigate(Direction.Up)
      case NavigateDown       => RunnerNavigate(Direction.Down)
      case NavigateLeft       => RunnerNavigate(Direction.Left)
      case NavigateRight      => RunnerNavigate(Direction.Right)
      case DeleteBackward     => RunnerDeleteBackward
      case DeleteForward      => RunnerDeleteForward
      case DeleteWordBackward => RunnerDeleteWordBackward
      case DeleteWordForward  => RunnerDeleteWordForward
      case NextCategory       => RunnerNextCategory
      case PreviousCategory   => RunnerPreviousCategory
      case Submit             => RunnerSubmit
      case Dismiss            => RunnerDismiss

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

  def configKey: String =
    this match
      case NavigateUp         => "navigate_up"
      case NavigateDown       => "navigate_down"
      case NavigateLeft       => "navigate_left"
      case NavigateRight      => "navigate_right"
      case DeleteBackward     => "delete_backward"
      case DeleteForward      => "delete_forward"
      case DeleteWordBackward => "delete_word_backward"
      case DeleteWordForward  => "delete_word_forward"
      case NextField          => "next_field"
      case PreviousField      => "previous_field"
      case Submit             => "submit"
      case Dismiss            => "dismiss"

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

enum PanelKeyAction extends KeymapEventAction[PanelInputEvent]:
  case NavigateUp
  case NavigateDown
  case NavigateLeft
  case NavigateRight
  case ReturnFocus
  case Activate

  def configKey: String =
    this match
      case NavigateUp    => "navigate_up"
      case NavigateDown  => "navigate_down"
      case NavigateLeft  => "navigate_left"
      case NavigateRight => "navigate_right"
      case ReturnFocus   => "return_focus"
      case Activate      => "activate"

  def event: PanelInputEvent =
    this match
      case NavigateUp    => PanelInputEvent.Navigate(Direction.Up)
      case NavigateDown  => PanelInputEvent.Navigate(Direction.Down)
      case NavigateLeft  => PanelInputEvent.Navigate(Direction.Left)
      case NavigateRight => PanelInputEvent.Navigate(Direction.Right)
      case ReturnFocus   => PanelInputEvent.ReturnFocus
      case Activate      => PanelInputEvent.Activate

enum PeekKeyAction extends KeymapEventAction[PeekInputEvent]:
  case NavigateUp
  case NavigateDown
  case NavigateLeft
  case NavigateRight
  case Accept
  case Dismiss
  case OtherInput

  def configKey: String =
    this match
      case NavigateUp    => "navigate_up"
      case NavigateDown  => "navigate_down"
      case NavigateLeft  => "navigate_left"
      case NavigateRight => "navigate_right"
      case Accept        => "accept"
      case Dismiss       => "dismiss"
      case OtherInput    => "other_input"

  def event: PeekInputEvent =
    this match
      case NavigateUp    => PeekInputEvent.Navigate(Direction.Up)
      case NavigateDown  => PeekInputEvent.Navigate(Direction.Down)
      case NavigateLeft  => PeekInputEvent.Navigate(Direction.Left)
      case NavigateRight => PeekInputEvent.Navigate(Direction.Right)
      case Accept        => PeekInputEvent.Accept
      case Dismiss       => PeekInputEvent.Dismiss
      case OtherInput    => PeekInputEvent.OtherInput

case class EditorKeymapConfig(
    bindings: Map[EditorKeyAction, List[HotkeyTrigger]] = EditorKeymapConfig.defaultBindings
):
  def bindingsFor(action: EditorKeyAction): List[HotkeyTrigger] =
    bindings.getOrElse(action, Nil)

  def withBinding(action: EditorKeyAction, trigger: HotkeyTrigger): EditorKeymapConfig =
    copy(bindings = bindings + (action -> List(trigger)))

  def withBinding(action: EditorKeyAction, binding: String): EditorKeymapConfig =
    HotkeyTrigger.parse(binding).map(trigger => withBinding(action, trigger)).getOrElse(this)

  def resetBinding(action: EditorKeyAction): EditorKeymapConfig =
    copy(bindings = bindings + (action -> EditorKeymapConfig.defaultBindings.getOrElse(action, Nil)))

object EditorKeymapConfig:

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
    EditorKeyAction.MoveToStart -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Home, None, Set.empty)),
    EditorKeyAction.MoveToEnd   -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.End, None, Set.empty)),
    EditorKeyAction.MoveToStartOfFile -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Home, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    EditorKeyAction.MoveToEndOfFile -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.End, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    EditorKeyAction.PageUp         -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.PageUp, None, Set.empty)),
    EditorKeyAction.PageDown       -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.PageDown, None, Set.empty)),
    EditorKeyAction.DeleteBackward -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set.empty)),
    EditorKeyAction.DeleteForward  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Delete, None, Set.empty)),
    EditorKeyAction.DeleteWordBackward -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    EditorKeyAction.DeleteWordForward -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Delete, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    EditorKeyAction.Escape     -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Escape, None, Set.empty)),
    EditorKeyAction.NewLine    -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Enter, None, Set.empty)),
    EditorKeyAction.Tab        -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Tab, None, Set.empty)),
    EditorKeyAction.ReverseTab -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ReverseTab, None, Set.empty))
  )

  given Encoder[EditorKeyAction] = Encoder.encodeString.contramap(_.configKey)

  given Decoder[EditorKeyAction] = Decoder.decodeString.emap(key =>
    EditorKeyAction.values.find(_.configKey == key).toRight(s"Unknown editor key action: $key")
  )

  given Encoder[EditorKeymapConfig] =
    Encoder.instance(config => KeymapCodecSupport.encodeBindings(config.bindings)(_.configKey))

  given Decoder[EditorKeymapConfig] =
    Decoder
      .decodeMap[String, List[HotkeyTrigger]]
      .emap(bindings =>
        KeymapCodecSupport
          .decodeBindings(bindings, EditorKeyAction.values.toList, _.configKey, defaultBindings)
          .map(EditorKeymapConfig(_))
      )

case class CommandRunnerKeymapConfig(
    bindings: Map[CommandRunnerKeyAction, List[HotkeyTrigger]] = CommandRunnerKeymapConfig.defaultBindings
):
  def bindingsFor(action: CommandRunnerKeyAction): List[HotkeyTrigger] =
    bindings.getOrElse(action, Nil)

  def withBinding(action: CommandRunnerKeyAction, trigger: HotkeyTrigger): CommandRunnerKeymapConfig =
    copy(bindings = bindings + (action -> List(trigger)))

  def withBinding(action: CommandRunnerKeyAction, binding: String): CommandRunnerKeymapConfig =
    HotkeyTrigger.parse(binding).map(trigger => withBinding(action, trigger)).getOrElse(this)

  def resetBinding(action: CommandRunnerKeyAction): CommandRunnerKeymapConfig =
    copy(bindings = bindings + (action -> CommandRunnerKeymapConfig.defaultBindings.getOrElse(action, Nil)))

object CommandRunnerKeymapConfig:

  val defaultBindings: Map[CommandRunnerKeyAction, List[HotkeyTrigger]] = Map(
    CommandRunnerKeyAction.NavigateUp -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowUp, None, Set.empty)),
    CommandRunnerKeyAction.NavigateDown -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowDown, None, Set.empty)
    ),
    CommandRunnerKeyAction.NavigateLeft -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowLeft, None, Set.empty)
    ),
    CommandRunnerKeyAction.NavigateRight -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowRight, None, Set.empty)
    ),
    CommandRunnerKeyAction.DeleteBackward -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set.empty)
    ),
    CommandRunnerKeyAction.DeleteForward -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Delete, None, Set.empty)
    ),
    CommandRunnerKeyAction.DeleteWordBackward -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    CommandRunnerKeyAction.DeleteWordForward -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Delete, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    CommandRunnerKeyAction.NextCategory -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Tab, None, Set.empty)),
    CommandRunnerKeyAction.PreviousCategory -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.ReverseTab, None, Set.empty)
    ),
    CommandRunnerKeyAction.Submit  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Enter, None, Set.empty)),
    CommandRunnerKeyAction.Dismiss -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Escape, None, Set.empty))
  )

  given Encoder[CommandRunnerKeyAction] = Encoder.encodeString.contramap(_.configKey)

  given Decoder[CommandRunnerKeyAction] = Decoder.decodeString.emap(key =>
    CommandRunnerKeyAction.values.find(_.configKey == key).toRight(s"Unknown command runner key action: $key")
  )

  given Encoder[CommandRunnerKeymapConfig] =
    Encoder.instance(config => KeymapCodecSupport.encodeBindings(config.bindings)(_.configKey))

  given Decoder[CommandRunnerKeymapConfig] =
    Decoder
      .decodeMap[String, List[HotkeyTrigger]]
      .emap(bindings =>
        KeymapCodecSupport
          .decodeBindings(bindings, CommandRunnerKeyAction.values.toList, _.configKey, defaultBindings)
          .map(CommandRunnerKeymapConfig(_))
      )

case class ModalKeymapConfig(
    bindings: Map[ModalKeyAction, List[HotkeyTrigger]] = ModalKeymapConfig.defaultBindings
):
  def bindingsFor(action: ModalKeyAction): List[HotkeyTrigger] =
    bindings.getOrElse(action, Nil)

  def withBinding(action: ModalKeyAction, trigger: HotkeyTrigger): ModalKeymapConfig =
    copy(bindings = bindings + (action -> List(trigger)))

  def withBinding(action: ModalKeyAction, binding: String): ModalKeymapConfig =
    HotkeyTrigger.parse(binding).map(trigger => withBinding(action, trigger)).getOrElse(this)

  def resetBinding(action: ModalKeyAction): ModalKeymapConfig =
    copy(bindings = bindings + (action -> ModalKeymapConfig.defaultBindings.getOrElse(action, Nil)))

object ModalKeymapConfig:

  val defaultBindings: Map[ModalKeyAction, List[HotkeyTrigger]] = Map(
    ModalKeyAction.NavigateUp     -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowUp, None, Set.empty)),
    ModalKeyAction.NavigateDown   -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowDown, None, Set.empty)),
    ModalKeyAction.NavigateLeft   -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowLeft, None, Set.empty)),
    ModalKeyAction.NavigateRight  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowRight, None, Set.empty)),
    ModalKeyAction.DeleteBackward -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set.empty)),
    ModalKeyAction.DeleteForward  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Delete, None, Set.empty)),
    ModalKeyAction.DeleteWordBackward -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    ModalKeyAction.DeleteWordForward -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Delete, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    ModalKeyAction.NextField     -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Tab, None, Set.empty)),
    ModalKeyAction.PreviousField -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ReverseTab, None, Set.empty)),
    ModalKeyAction.Submit        -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Enter, None, Set.empty)),
    ModalKeyAction.Dismiss       -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Escape, None, Set.empty))
  )

  given Encoder[ModalKeyAction] = Encoder.encodeString.contramap(_.configKey)

  given Decoder[ModalKeyAction] = Decoder.decodeString.emap(key =>
    ModalKeyAction.values.find(_.configKey == key).toRight(s"Unknown modal key action: $key")
  )

  given Encoder[ModalKeymapConfig] =
    Encoder.instance(config => KeymapCodecSupport.encodeBindings(config.bindings)(_.configKey))

  given Decoder[ModalKeymapConfig] =
    Decoder
      .decodeMap[String, List[HotkeyTrigger]]
      .emap(bindings =>
        KeymapCodecSupport
          .decodeBindings(bindings, ModalKeyAction.values.toList, _.configKey, defaultBindings)
          .map(ModalKeymapConfig(_))
      )

case class PanelKeymapConfig(
    bindings: Map[PanelKeyAction, List[HotkeyTrigger]] = PanelKeymapConfig.defaultBindings
):
  def bindingsFor(action: PanelKeyAction): List[HotkeyTrigger] =
    bindings.getOrElse(action, Nil)

  def withBinding(action: PanelKeyAction, trigger: HotkeyTrigger): PanelKeymapConfig =
    copy(bindings = bindings + (action -> List(trigger)))

  def withBinding(action: PanelKeyAction, binding: String): PanelKeymapConfig =
    HotkeyTrigger.parse(binding).map(trigger => withBinding(action, trigger)).getOrElse(this)

  def resetBinding(action: PanelKeyAction): PanelKeymapConfig =
    copy(bindings = bindings + (action -> PanelKeymapConfig.defaultBindings.getOrElse(action, Nil)))

object PanelKeymapConfig:

  val defaultBindings: Map[PanelKeyAction, List[HotkeyTrigger]] = Map(
    PanelKeyAction.NavigateUp    -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowUp, None, Set.empty)),
    PanelKeyAction.NavigateDown  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowDown, None, Set.empty)),
    PanelKeyAction.NavigateLeft  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowLeft, None, Set.empty)),
    PanelKeyAction.NavigateRight -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowRight, None, Set.empty)),
    PanelKeyAction.ReturnFocus -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set.empty),
      HotkeyTrigger(com.serenity.keystroke.InputKey.Delete, None, Set.empty),
      HotkeyTrigger(com.serenity.keystroke.InputKey.Tab, None, Set.empty),
      HotkeyTrigger(com.serenity.keystroke.InputKey.ReverseTab, None, Set.empty),
      HotkeyTrigger(com.serenity.keystroke.InputKey.Escape, None, Set.empty)
    ),
    PanelKeyAction.Activate -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Enter, None, Set.empty))
  )

  given Encoder[PanelKeyAction] = Encoder.encodeString.contramap(_.configKey)

  given Decoder[PanelKeyAction] = Decoder.decodeString.emap(key =>
    PanelKeyAction.values.find(_.configKey == key).toRight(s"Unknown panel key action: $key")
  )

  given Encoder[PanelKeymapConfig] =
    Encoder.instance(config => KeymapCodecSupport.encodeBindings(config.bindings)(_.configKey))

  given Decoder[PanelKeymapConfig] =
    Decoder
      .decodeMap[String, List[HotkeyTrigger]]
      .emap(bindings =>
        KeymapCodecSupport
          .decodeBindings(bindings, PanelKeyAction.values.toList, _.configKey, defaultBindings)
          .map(PanelKeymapConfig(_))
      )

case class PeekKeymapConfig(
    bindings: Map[PeekKeyAction, List[HotkeyTrigger]] = PeekKeymapConfig.defaultBindings
):
  def bindingsFor(action: PeekKeyAction): List[HotkeyTrigger] =
    bindings.getOrElse(action, Nil)

  def withBinding(action: PeekKeyAction, trigger: HotkeyTrigger): PeekKeymapConfig =
    copy(bindings = bindings + (action -> List(trigger)))

  def withBinding(action: PeekKeyAction, binding: String): PeekKeymapConfig =
    HotkeyTrigger.parse(binding).map(trigger => withBinding(action, trigger)).getOrElse(this)

  def resetBinding(action: PeekKeyAction): PeekKeymapConfig =
    copy(bindings = bindings + (action -> PeekKeymapConfig.defaultBindings.getOrElse(action, Nil)))

object PeekKeymapConfig:

  val defaultBindings: Map[PeekKeyAction, List[HotkeyTrigger]] = Map(
    PeekKeyAction.NavigateUp    -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowUp, None, Set.empty)),
    PeekKeyAction.NavigateDown  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowDown, None, Set.empty)),
    PeekKeyAction.NavigateLeft  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowLeft, None, Set.empty)),
    PeekKeyAction.NavigateRight -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowRight, None, Set.empty)),
    PeekKeyAction.Accept        -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Enter, None, Set.empty)),
    PeekKeyAction.Dismiss       -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Escape, None, Set.empty)),
    PeekKeyAction.OtherInput -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set.empty),
      HotkeyTrigger(com.serenity.keystroke.InputKey.Delete, None, Set.empty),
      HotkeyTrigger(com.serenity.keystroke.InputKey.Tab, None, Set.empty),
      HotkeyTrigger(com.serenity.keystroke.InputKey.ReverseTab, None, Set.empty)
    )
  )

  given Encoder[PeekKeyAction] = Encoder.encodeString.contramap(_.configKey)

  given Decoder[PeekKeyAction] = Decoder.decodeString.emap(key =>
    PeekKeyAction.values.find(_.configKey == key).toRight(s"Unknown peek key action: $key")
  )

  given Encoder[PeekKeymapConfig] =
    Encoder.instance(config => KeymapCodecSupport.encodeBindings(config.bindings)(_.configKey))

  given Decoder[PeekKeymapConfig] =
    Decoder
      .decodeMap[String, List[HotkeyTrigger]]
      .emap(bindings =>
        KeymapCodecSupport
          .decodeBindings(bindings, PeekKeyAction.values.toList, _.configKey, defaultBindings)
          .map(PeekKeymapConfig(_))
      )

case class FocusedKeymapConfig(
    editor: EditorKeymapConfig = EditorKeymapConfig(),
    commandRunner: CommandRunnerKeymapConfig = CommandRunnerKeymapConfig(),
    modal: ModalKeymapConfig = ModalKeymapConfig(),
    panel: PanelKeymapConfig = PanelKeymapConfig(),
    peek: PeekKeymapConfig = PeekKeymapConfig()
):
  def withEditorBinding(action: EditorKeyAction, binding: String): FocusedKeymapConfig =
    copy(editor = editor.withBinding(action, binding))

  def withCommandRunnerBinding(action: CommandRunnerKeyAction, binding: String): FocusedKeymapConfig =
    copy(commandRunner = commandRunner.withBinding(action, binding))

  def withModalBinding(action: ModalKeyAction, binding: String): FocusedKeymapConfig =
    copy(modal = modal.withBinding(action, binding))

  def withPanelBinding(action: PanelKeyAction, binding: String): FocusedKeymapConfig =
    copy(panel = panel.withBinding(action, binding))

  def withPeekBinding(action: PeekKeyAction, binding: String): FocusedKeymapConfig =
    copy(peek = peek.withBinding(action, binding))

  def resetEditorBinding(action: EditorKeyAction): FocusedKeymapConfig =
    copy(editor = editor.resetBinding(action))

  def resetCommandRunnerBinding(action: CommandRunnerKeyAction): FocusedKeymapConfig =
    copy(commandRunner = commandRunner.resetBinding(action))

  def resetModalBinding(action: ModalKeyAction): FocusedKeymapConfig =
    copy(modal = modal.resetBinding(action))

  def resetPanelBinding(action: PanelKeyAction): FocusedKeymapConfig =
    copy(panel = panel.resetBinding(action))

  def resetPeekBinding(action: PeekKeyAction): FocusedKeymapConfig =
    copy(peek = peek.resetBinding(action))

object FocusedKeymapConfig:

  given Encoder[FocusedKeymapConfig] = Encoder.forProduct5(
    "editor",
    "commandRunner",
    "modal",
    "panel",
    "peek"
  )(config => (config.editor, config.commandRunner, config.modal, config.panel, config.peek))

  given Decoder[FocusedKeymapConfig] = Decoder.forProduct5(
    "editor",
    "commandRunner",
    "modal",
    "panel",
    "peek"
  )(FocusedKeymapConfig.apply)

private object KeymapCodecSupport:
  def encodeBindings[A](bindings: Map[A, List[HotkeyTrigger]])(keyOf: A => String): io.circe.Json =
    bindings.map { case (action, triggers) => keyOf(action) -> triggers }.asJson

  def decodeBindings[A](
    bindings: Map[String, List[HotkeyTrigger]],
    values: List[A],
    keyOf: A => String,
    defaults: Map[A, List[HotkeyTrigger]]
  ): Either[String, Map[A, List[HotkeyTrigger]]] =
    val decoded = bindings.toList.map { (key, triggers) =>
      values.find(action => keyOf(action) == key).map(_ -> triggers).toRight(s"Unknown keymap action: $key")
    }
    decoded.collectFirst { case Left(error) => error } match
      case Some(error) => Left(error)
      case None        => Right(defaults ++ decoded.collect { case Right(entry) => entry }.toMap)
