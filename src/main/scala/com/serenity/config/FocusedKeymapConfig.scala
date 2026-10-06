package com.serenity.config

import com.serenity.keystroke.events.*
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
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
      HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set(com.serenity.keystroke.Modifier.Ctrl)),
      HotkeyTrigger(com.serenity.keystroke.InputKey.Backspace, None, Set(com.serenity.keystroke.Modifier.Alt))
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

enum PanelKeyAction extends KeymapEventAction[PanelInputEvent]:
  case NavigateUp
  case NavigateDown
  case NavigateLeft
  case NavigateRight
  case ReturnFocus
  case Dismiss
  case Activate
  case GrowPanel
  case ShrinkPanel
  case First
  case Last
  case PageUp
  case PageDown

  def event: PanelInputEvent =
    this match
      case NavigateUp    => PanelInputEvent.Navigate(Direction.Up)
      case NavigateDown  => PanelInputEvent.Navigate(Direction.Down)
      case NavigateLeft  => PanelInputEvent.Navigate(Direction.Left)
      case NavigateRight => PanelInputEvent.Navigate(Direction.Right)
      case ReturnFocus   => PanelInputEvent.ReturnFocus
      case Dismiss       => PanelInputEvent.Dismiss
      case Activate      => PanelInputEvent.Activate
      case GrowPanel     => PanelInputEvent.Resize(1)
      case ShrinkPanel   => PanelInputEvent.Resize(-1)
      case First         => PanelInputEvent.First
      case Last          => PanelInputEvent.Last
      case PageUp        => PanelInputEvent.Page(-1)
      case PageDown      => PanelInputEvent.Page(1)

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
      HotkeyTrigger(com.serenity.keystroke.InputKey.ReverseTab, None, Set.empty)
    ),
    PanelKeyAction.Dismiss  -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Escape, None, Set.empty)),
    PanelKeyAction.Activate -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Enter, None, Set.empty)),
    PanelKeyAction.GrowPanel -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowUp, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    PanelKeyAction.ShrinkPanel -> List(
      HotkeyTrigger(com.serenity.keystroke.InputKey.ArrowDown, None, Set(com.serenity.keystroke.Modifier.Ctrl))
    ),
    PanelKeyAction.First    -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.Home, None, Set.empty)),
    PanelKeyAction.Last     -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.End, None, Set.empty)),
    PanelKeyAction.PageUp   -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.PageUp, None, Set.empty)),
    PanelKeyAction.PageDown -> List(HotkeyTrigger(com.serenity.keystroke.InputKey.PageDown, None, Set.empty))
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

  given Encoder[FocusedKeymapConfig] = deriveEncoder
  given Decoder[FocusedKeymapConfig] = deriveDecoder

private object KeymapCodecSupport:
  def encodeBindings[A](bindings: Map[A, List[HotkeyTrigger]])(keyOf: A => String): io.circe.Json =
    bindings.map { case (action, triggers) => keyOf(action) -> triggers }.asJson

  def decodeBindings[A](
    bindings: Map[String, List[HotkeyTrigger]],
    values: List[A],
    keyOf: A => String,
    defaults: Map[A, List[HotkeyTrigger]]
  ): Either[String, Map[A, List[HotkeyTrigger]]] =
    // Skip action keys this build doesn't recognize -- config written by another version (an action since renamed or
    // removed, e.g. a `next_category` binding in an older saved preset) must not discard the whole keymap section and
    // fall the user back to defaults. Recognized bindings are kept, merged onto defaults; unknown keys are dropped.
    val recognized =
      bindings.toList.flatMap((key, triggers) => values.find(action => keyOf(action) == key).map(_ -> triggers))
    Right(defaults ++ recognized.toMap)
