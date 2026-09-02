package com.serenity.config

import com.serenity.keystroke.events.*
import io.circe.syntax.given
import io.circe.{Decoder, Encoder}

import HotkeyConfig.given

trait KeymapEventAction[+E <: Event]:
  /** Set only when a group's on-disk key genuinely diverges from its mechanical snake_case derivation. */
  def configKeyOverride: Option[String] = None

  final def configKey: String = configKeyOverride.getOrElse(KeymapEventAction.deriveConfigKey(toString))

  def event: E

object KeymapEventAction:
  private def deriveConfigKey(caseName: String): String =
    caseName.replaceAll("([a-z0-9])([A-Z])", "$1_$2").toLowerCase

/** The full set of actions and their default bindings for one keymap group's action enum. */
trait KeymapActionCodec[A]:
  def values: List[A]
  def defaultBindings: Map[A, List[HotkeyTrigger]]

private object KeymapBindings:

  def assign[A](bindings: Map[A, List[HotkeyTrigger]], action: A, trigger: HotkeyTrigger): Map[A, List[HotkeyTrigger]] =
    if bindings.exists { case (otherAction, triggers) => otherAction != action && triggers.contains(trigger) } then
      bindings
    else bindings + (action -> List(trigger))

  def assignUnbindingConflicts[A](
    bindings: Map[A, List[HotkeyTrigger]],
    action: A,
    trigger: HotkeyTrigger
  ): Map[A, List[HotkeyTrigger]] =
    bindings.view.mapValues(_.filterNot(_ == trigger)).toMap + (action -> List(trigger))

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
  case ExtendSelectionToLineStart
  case ExtendSelectionToLineEnd
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
      case ExtendSelectionToLineStart =>
        com.serenity.keystroke.events.ExtendSelectionToLineStart
      case ExtendSelectionToLineEnd =>
        com.serenity.keystroke.events.ExtendSelectionToLineEnd
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

  given KeymapActionCodec[EditorKeyAction] with
    def values: List[EditorKeyAction]                              = EditorKeyAction.values.toList
    def defaultBindings: Map[EditorKeyAction, List[HotkeyTrigger]] = EditorKeyAction.defaultBindings

enum CommandRunnerKeyAction extends KeymapEventAction[CommandRunnerEvent]:
  case NavigateUp
  case NavigateDown
  case NavigateLeft
  case NavigateRight
  case DeleteBackward
  case DeleteForward
  case DeleteWordBackward
  case DeleteWordForward
  case Submit
  case Dismiss

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
      case Submit             => RunnerSubmit
      case Dismiss            => RunnerDismiss

object CommandRunnerKeyAction:

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
    CommandRunnerKeyAction.Submit  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Enter, None, Set.empty)),
    CommandRunnerKeyAction.Dismiss -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Escape, None, Set.empty))
  )

  given KeymapActionCodec[CommandRunnerKeyAction] with
    def values: List[CommandRunnerKeyAction]                              = CommandRunnerKeyAction.values.toList
    def defaultBindings: Map[CommandRunnerKeyAction, List[HotkeyTrigger]] = CommandRunnerKeyAction.defaultBindings

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

object ModalKeyAction:

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
    ModalKeyAction.Dismiss       -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Escape, None, Set.empty)),
    ModalKeyAction.CreateDirectory -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.Character, Some('n'), Set(com.serenity.keystroke.Modifier.Ctrl))
    )
  )

  given KeymapActionCodec[ModalKeyAction] with
    def values: List[ModalKeyAction]                              = ModalKeyAction.values.toList
    def defaultBindings: Map[ModalKeyAction, List[HotkeyTrigger]] = ModalKeyAction.defaultBindings

enum PanelKeyAction extends KeymapEventAction[PanelInputEvent]:
  case NavigateUp
  case NavigateDown
  case NavigateLeft
  case NavigateRight
  case ReturnFocus
  case Activate

  def event: PanelInputEvent =
    this match
      case NavigateUp    => PanelInputEvent.Navigate(Direction.Up)
      case NavigateDown  => PanelInputEvent.Navigate(Direction.Down)
      case NavigateLeft  => PanelInputEvent.Navigate(Direction.Left)
      case NavigateRight => PanelInputEvent.Navigate(Direction.Right)
      case ReturnFocus   => PanelInputEvent.ReturnFocus
      case Activate      => PanelInputEvent.Activate

object PanelKeyAction:

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

  given KeymapActionCodec[PanelKeyAction] with
    def values: List[PanelKeyAction]                              = PanelKeyAction.values.toList
    def defaultBindings: Map[PanelKeyAction, List[HotkeyTrigger]] = PanelKeyAction.defaultBindings

enum PeekKeyAction extends KeymapEventAction[PeekInputEvent]:
  case NavigateUp
  case NavigateDown
  case NavigateLeft
  case NavigateRight
  case Accept
  case Dismiss
  case OtherInput

  def event: PeekInputEvent =
    this match
      case NavigateUp    => PeekInputEvent.Navigate(Direction.Up)
      case NavigateDown  => PeekInputEvent.Navigate(Direction.Down)
      case NavigateLeft  => PeekInputEvent.Navigate(Direction.Left)
      case NavigateRight => PeekInputEvent.Navigate(Direction.Right)
      case Accept        => PeekInputEvent.Accept
      case Dismiss       => PeekInputEvent.Dismiss
      case OtherInput    => PeekInputEvent.OtherInput

object PeekKeyAction:

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

  given KeymapActionCodec[PeekKeyAction] with
    def values: List[PeekKeyAction]                              = PeekKeyAction.values.toList
    def defaultBindings: Map[PeekKeyAction, List[HotkeyTrigger]] = PeekKeyAction.defaultBindings

/** One keymap group's bindings: the action-to-trigger map plus the four operations every group supports. */
final case class KeymapGroupConfig[A <: KeymapEventAction[E], E <: Event](
    bindings: Map[A, List[HotkeyTrigger]]
)(using codec: KeymapActionCodec[A]):

  def bindingsFor(action: A): List[HotkeyTrigger] = bindings.getOrElse(action, Nil)

  def withBinding(action: A, trigger: HotkeyTrigger): KeymapGroupConfig[A, E] =
    copy(bindings = KeymapBindings.assign(bindings, action, trigger))

  def withBindingUnbindingConflicts(action: A, trigger: HotkeyTrigger): KeymapGroupConfig[A, E] =
    copy(bindings = KeymapBindings.assignUnbindingConflicts(bindings, action, trigger))

  def withBinding(action: A, binding: String): KeymapGroupConfig[A, E] =
    HotkeyTrigger.parse(binding).map(trigger => withBinding(action, trigger)).getOrElse(this)

  def resetBinding(action: A): KeymapGroupConfig[A, E] =
    copy(bindings = bindings + (action -> codec.defaultBindings.getOrElse(action, Nil)))

object KeymapGroupConfig:

  /** The group populated with its action type's default bindings. */
  def defaults[A <: KeymapEventAction[E], E <: Event](using codec: KeymapActionCodec[A]): KeymapGroupConfig[A, E] =
    KeymapGroupConfig(codec.defaultBindings)

  given [A <: KeymapEventAction[E], E <: Event]: Encoder[KeymapGroupConfig[A, E]] =
    Encoder.instance(config => KeymapCodecSupport.encodeBindings(config.bindings)(_.configKey))

  given [A <: KeymapEventAction[E], E <: Event](using codec: KeymapActionCodec[A]): Decoder[KeymapGroupConfig[A, E]] =
    Decoder
      .decodeMap[String, List[HotkeyTrigger]]
      .emap(bindings =>
        KeymapCodecSupport
          .decodeBindings(bindings, codec.values, (action: A) => action.configKey, codec.defaultBindings)
          .map(KeymapGroupConfig(_))
      )

/** Selects one of [[FocusedKeymapConfig]]'s keymap groups, carrying the lens needed to read and update it. Adding a new
  * keymap group means adding one case here (and the enum/codec pair it points at) — no new methods on
  * [[FocusedKeymapConfig]] or [[AppConfig]] are needed.
  */
enum KeymapGroup[A <: KeymapEventAction[E], E <: Event](
    val get: FocusedKeymapConfig => KeymapGroupConfig[A, E],
    val set: (FocusedKeymapConfig, KeymapGroupConfig[A, E]) => FocusedKeymapConfig
):
  case Editor
      extends KeymapGroup[EditorKeyAction, EditorEvent](_.editor, (config, group) => config.copy(editor = group))

  case CommandRunner
      extends KeymapGroup[CommandRunnerKeyAction, CommandRunnerEvent](
        _.commandRunner,
        (config, group) => config.copy(commandRunner = group)
      )

  case Modal
      extends KeymapGroup[ModalKeyAction, ModalInputEvent](_.modal, (config, group) => config.copy(modal = group))
  case Panel
      extends KeymapGroup[PanelKeyAction, PanelInputEvent](_.panel, (config, group) => config.copy(panel = group))
  case Peek extends KeymapGroup[PeekKeyAction, PeekInputEvent](_.peek, (config, group) => config.copy(peek = group))

final case class FocusedKeymapConfig(
    editor: KeymapGroupConfig[EditorKeyAction, EditorEvent] = KeymapGroupConfig.defaults,
    commandRunner: KeymapGroupConfig[CommandRunnerKeyAction, CommandRunnerEvent] = KeymapGroupConfig.defaults,
    modal: KeymapGroupConfig[ModalKeyAction, ModalInputEvent] = KeymapGroupConfig.defaults,
    panel: KeymapGroupConfig[PanelKeyAction, PanelInputEvent] = KeymapGroupConfig.defaults,
    peek: KeymapGroupConfig[PeekKeyAction, PeekInputEvent] = KeymapGroupConfig.defaults
):

  def withBinding[A <: KeymapEventAction[E], E <: Event](
    group: KeymapGroup[A, E]
  )(action: A, binding: String): FocusedKeymapConfig =
    group.set(this, group.get(this).withBinding(action, binding))

  def withBindingUnbindingConflicts[A <: KeymapEventAction[E], E <: Event](
    group: KeymapGroup[A, E]
  )(action: A, binding: String): FocusedKeymapConfig =
    HotkeyTrigger
      .parse(binding)
      .fold(this)(trigger => group.set(this, group.get(this).withBindingUnbindingConflicts(action, trigger)))

  def resetBinding[A <: KeymapEventAction[E], E <: Event](group: KeymapGroup[A, E])(action: A): FocusedKeymapConfig =
    group.set(this, group.get(this).resetBinding(action))

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
