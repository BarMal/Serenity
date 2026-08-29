package com.serenity.config

import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import io.circe.syntax.given
import io.circe.{Decoder, Encoder}

enum HotkeyAction:
  case Save
  case Quit
  case Undo
  case Redo
  case Copy
  case Paste
  case Cut
  case SelectAll
  case ToggleSyntaxHighlighting
  case OpenFile
  case ToggleCommandRunner
  case ToggleContextualToolbar
  case NewTab
  case CloseTab
  case FileSearch
  case NextTab
  case PreviousTab
  case Find
  case Replace
  case GoToLine
  case SaveAs

  def configKey: String =
    this match
      case Save                     => "save"
      case Quit                     => "quit"
      case Undo                     => "undo"
      case Redo                     => "redo"
      case Copy                     => "copy"
      case Paste                    => "paste"
      case Cut                      => "cut"
      case SelectAll                => "select_all"
      case ToggleSyntaxHighlighting => "toggle_syntax_highlighting"
      case OpenFile                 => "open_file"
      case ToggleCommandRunner      => "command_palette"
      case ToggleContextualToolbar  => "contextual_toolbar"
      case NewTab                   => "new_tab"
      case CloseTab                 => "close_tab"
      case FileSearch               => "file_search"
      case NextTab                  => "next_tab"
      case PreviousTab              => "previous_tab"
      case Find                     => "find"
      case Replace                  => "replace"
      case GoToLine                 => "go_to_line"
      case SaveAs                   => "save_as"

final case class HotkeyTrigger(
    keyType: InputKey,
    character: Option[Char],
    modifiers: Set[Modifier]
):

  def matches(info: KeyStrokeInfo): Boolean =
    info.keyType == keyType &&
      info.character == character &&
      info.modifiers == modifiers

  /** True for a double-tap-a-lone-modifier binding (`ctrl+ctrl`, `meta+meta`, ...) -- `keyType` itself is the modifier
    * key, not a regular key held down with modifiers. No CSI-u tier below
    * [[com.serenity.keystroke.KeyboardFidelityTier.Full]] can represent a bare modifier press/release event, so this
    * predicate is what `CommandRunnerReducer`'s tier-fidelity warning keys off.
    */
  def isBareModifierChord: Boolean =
    Set(InputKey.Ctrl, InputKey.Alt, InputKey.Shift, InputKey.Meta).contains(keyType)

  def render: String =
    val modifierParts =
      List(
        Option.when(modifiers.contains(Modifier.Ctrl))("ctrl"),
        Option.when(modifiers.contains(Modifier.Alt))("alt"),
        Option.when(modifiers.contains(Modifier.Meta))("meta"),
        Option.when(modifiers.contains(Modifier.Shift))("shift")
      ).flatten
    val keyPart =
      keyType match
        case InputKey.Character  => character.map(_.toString).getOrElse("")
        case InputKey.Ctrl       => "ctrl"
        case InputKey.Alt        => "alt"
        case InputKey.Shift      => "shift"
        case InputKey.Meta       => "meta"
        case InputKey.Tab        => "tab"
        case InputKey.ReverseTab => "reverse-tab"
        case InputKey.Enter      => "enter"
        case InputKey.Backspace  => "backspace"
        case InputKey.Delete     => "delete"
        case InputKey.Escape     => "escape"
        case InputKey.ArrowUp    => "up"
        case InputKey.ArrowDown  => "down"
        case InputKey.ArrowLeft  => "left"
        case InputKey.ArrowRight => "right"
        case InputKey.Home       => "home"
        case InputKey.End        => "end"
        case InputKey.PageUp     => "pageup"
        case InputKey.PageDown   => "pagedown"
        case InputKey.F1         => "f1"
        case InputKey.F2         => "f2"
        case InputKey.F3         => "f3"
        case InputKey.F4         => "f4"
        case InputKey.F5         => "f5"
        case InputKey.F6         => "f6"
        case InputKey.F7         => "f7"
        case InputKey.F8         => "f8"
        case InputKey.F9         => "f9"
        case InputKey.F10        => "f10"
        case InputKey.F11        => "f11"
        case InputKey.F12        => "f12"
        case InputKey.EOF        => "eof"
        case other               => other.toString.toLowerCase
    if Set(InputKey.Ctrl, InputKey.Alt, InputKey.Shift, InputKey.Meta).contains(keyType) then s"$keyPart+$keyPart"
    else (modifierParts :+ keyPart).mkString("+")

object HotkeyTrigger:

  def parse(input: String): Option[HotkeyTrigger] =
    val parts = input.trim.toLowerCase.split("\\+").toList.map(_.trim).filter(_.nonEmpty)
    parts match
      case key :: second :: Nil if key == second =>
        modifierKey(key)
          .map(inputKey => HotkeyTrigger(inputKey, None, Set.empty))
          .orElse(parseStandard(parts))
      case _ =>
        parseStandard(parts)

  private def parseStandard(parts: List[String]): Option[HotkeyTrigger] =
    val (modifierParts, keyParts) = parts.partition {
      case "ctrl" | "alt" | "shift" | "meta" | "cmd" | "command" => true
      case _                                                     => false
    }

    val modifiers = modifierParts.foldLeft(Set.empty[Modifier]) {
      case (acc, "ctrl")                     => acc + Modifier.Ctrl
      case (acc, "alt")                      => acc + Modifier.Alt
      case (acc, "shift")                    => acc + Modifier.Shift
      case (acc, "meta" | "cmd" | "command") => acc + Modifier.Meta
      case (acc, _)                          => acc
    }

    keyParts match
      case key :: Nil =>
        key match
          case "enter" =>
            Some(HotkeyTrigger(InputKey.Enter, None, modifiers))
          case "backspace" =>
            Some(HotkeyTrigger(InputKey.Backspace, None, modifiers))
          case "delete" =>
            Some(HotkeyTrigger(InputKey.Delete, None, modifiers))
          case "escape" =>
            Some(HotkeyTrigger(InputKey.Escape, None, modifiers))
          case "tab" =>
            Some(HotkeyTrigger(InputKey.Tab, None, modifiers))
          case "reverse-tab" | "reverse_tab" =>
            Some(HotkeyTrigger(InputKey.ReverseTab, None, modifiers))
          case "up" | "arrowup" =>
            Some(HotkeyTrigger(InputKey.ArrowUp, None, modifiers))
          case "down" | "arrowdown" =>
            Some(HotkeyTrigger(InputKey.ArrowDown, None, modifiers))
          case "left" | "arrowleft" =>
            Some(HotkeyTrigger(InputKey.ArrowLeft, None, modifiers))
          case "right" | "arrowright" =>
            Some(HotkeyTrigger(InputKey.ArrowRight, None, modifiers))
          case "home" =>
            Some(HotkeyTrigger(InputKey.Home, None, modifiers))
          case "end" =>
            Some(HotkeyTrigger(InputKey.End, None, modifiers))
          case "pageup" | "page_up" =>
            Some(HotkeyTrigger(InputKey.PageUp, None, modifiers))
          case "pagedown" | "page_down" =>
            Some(HotkeyTrigger(InputKey.PageDown, None, modifiers))
          case "f1" =>
            Some(HotkeyTrigger(InputKey.F1, None, modifiers))
          case "f2" =>
            Some(HotkeyTrigger(InputKey.F2, None, modifiers))
          case "f3" =>
            Some(HotkeyTrigger(InputKey.F3, None, modifiers))
          case "f4" =>
            Some(HotkeyTrigger(InputKey.F4, None, modifiers))
          case "f5" =>
            Some(HotkeyTrigger(InputKey.F5, None, modifiers))
          case "f6" =>
            Some(HotkeyTrigger(InputKey.F6, None, modifiers))
          case "f7" =>
            Some(HotkeyTrigger(InputKey.F7, None, modifiers))
          case "f8" =>
            Some(HotkeyTrigger(InputKey.F8, None, modifiers))
          case "f9" =>
            Some(HotkeyTrigger(InputKey.F9, None, modifiers))
          case "f10" =>
            Some(HotkeyTrigger(InputKey.F10, None, modifiers))
          case "f11" =>
            Some(HotkeyTrigger(InputKey.F11, None, modifiers))
          case "f12" =>
            Some(HotkeyTrigger(InputKey.F12, None, modifiers))
          case "eof" =>
            Some(HotkeyTrigger(InputKey.EOF, None, modifiers))
          case single if single.length == 1 =>
            Some(HotkeyTrigger(InputKey.Character, Some(single.head), modifiers))
          case _ =>
            None
      case _ =>
        None

  private def modifierKey(value: String): Option[InputKey] =
    value match
      case "ctrl"                     => Some(InputKey.Ctrl)
      case "alt"                      => Some(InputKey.Alt)
      case "shift"                    => Some(InputKey.Shift)
      case "meta" | "cmd" | "command" => Some(InputKey.Meta)
      case _                          => None

final case class HotkeyConfig(
    bindings: Map[HotkeyAction, List[HotkeyTrigger]] = HotkeyConfig.defaultBindings
):
  def bindingsFor(action: HotkeyAction): List[HotkeyTrigger] =
    bindings.getOrElse(action, Nil)

  def withBinding(action: HotkeyAction, trigger: HotkeyTrigger): HotkeyConfig =
    HotkeyConfig.fromBindings(bindings + (action -> List(trigger))).fold(_ => this, identity)

  def withBinding(action: HotkeyAction, binding: String): HotkeyConfig =
    HotkeyTrigger.parse(binding).map(trigger => withBinding(action, trigger)).getOrElse(this)

  /** Assign a trigger after removing it from every other global action. */
  def withBindingUnbindingConflicts(action: HotkeyAction, binding: String): HotkeyConfig =
    HotkeyTrigger.parse(binding) match
      case Some(trigger) =>
        val withoutConflict = bindings.view.mapValues(_.filterNot(_ == trigger)).toMap
        HotkeyConfig.fromBindings(withoutConflict + (action -> List(trigger))).fold(_ => this, identity)
      case None => this

  def resetBinding(action: HotkeyAction): HotkeyConfig =
    HotkeyConfig
      .fromBindings(bindings + (action -> HotkeyConfig.defaultBindings.getOrElse(action, Nil)))
      .fold(_ => this, identity)

  /** Rewrites every binding still at the macOS/Cmd-conditioned platform default to the Ctrl-based binding every
    * terminal actually forwards (issue #1213): a real terminal cannot deliver Cmd/Meta as an ordinary keystroke the way
    * AWT does for a focused Swing window -- macOS's own Terminal.app/iTerm2 intercept Cmd+Q as their own "quit the
    * terminal" shortcut before it ever reaches a running program's stdin, and `TerminalInputDecoder` never produces
    * `Modifier.Meta` from a plain keystroke either (only from a kitty-protocol-negotiated terminal actually choosing to
    * report it). An action the user customized away from its platform default is left untouched here -- it was
    * reachable enough for them to have bound it deliberately.
    *
    * Compares against `defaultBindingsFor`'s macOS output specifically -- never `defaultBindings`, which reads the
    * *running* JVM's `os.name` -- so this rewrite behaves identically whether Serenity's TUI is actually running on
    * macOS (the case it exists for) or is merely constructing/testing a mac-flavored `HotkeyConfig` from Linux CI.
    */
  def forTerminalUse: HotkeyConfig =
    val macDefaults = HotkeyConfig.validatedBindings(HotkeyConfig.defaultBindingsFor("Mac OS X"))
    val standard    = HotkeyConfig.terminalDefaultBindings
    HotkeyConfig(bindings.map {
      case (action, triggers) if macDefaults.get(action).contains(triggers) =>
        action -> standard.getOrElse(action, triggers)
      case unchanged => unchanged
    })

object HotkeyConfig:

  def forOs(osName: String): HotkeyConfig =
    HotkeyConfig(validatedBindings(defaultBindingsFor(osName)))

  def defaultBindings: Map[HotkeyAction, List[HotkeyTrigger]] =
    validatedBindings(defaultBindingsFor(System.getProperty("os.name", "")))

  /** The Ctrl-based bindings [[defaultBindingsFor]] resolves to on any non-macOS `osName` -- what
    * [[HotkeyConfig.forTerminalUse]] rewrites a still-at-default macOS/Cmd binding to (issue #1213). Any non-mac string
    * works here; a literal one names the intent rather than relying on `defaultBindingsFor`'s `isMac` check failing on
    * an empty string.
    */
  def terminalDefaultBindings: Map[HotkeyAction, List[HotkeyTrigger]] =
    validatedBindings(defaultBindingsFor("linux"))

  def defaultBindingsFor(osName: String): Map[HotkeyAction, List[HotkeyTrigger]] =
    val isMac           = osName.toLowerCase(java.util.Locale.ROOT).contains("mac")
    val primaryModifier = if isMac then Modifier.Meta else Modifier.Ctrl
    def primary(key: Char, shift: Boolean = false, alt: Boolean = false): HotkeyTrigger =
      HotkeyTrigger(
        InputKey.Character,
        Some(key),
        Set(primaryModifier) ++ Option.when(shift)(Modifier.Shift).toSet ++ Option.when(alt)(Modifier.Alt).toSet
      )
    def primaryKey(key: InputKey, shift: Boolean = false): HotkeyTrigger =
      HotkeyTrigger(key, None, Set(primaryModifier) ++ Option.when(shift)(Modifier.Shift).toSet)
    def primaryDoubleTap: HotkeyTrigger =
      HotkeyTrigger(
        primaryModifier match
          case Modifier.Ctrl => InputKey.Ctrl
          case Modifier.Meta => InputKey.Meta
          case _             => InputKey.Unknown,
        None,
        Set.empty
      )

    Map(
      HotkeyAction.Save -> List(primary('s')),
      HotkeyAction.Quit -> List(
        primary('q'),
        HotkeyTrigger(InputKey.EOF, None, Set.empty)
      ),
      HotkeyAction.Undo      -> List(primary('z')),
      HotkeyAction.Redo      -> List(primary('y')),
      HotkeyAction.Copy      -> List(primary('c')),
      HotkeyAction.Paste     -> List(primary('v')),
      HotkeyAction.Cut       -> List(primary('x')),
      HotkeyAction.SelectAll -> List(primary('a')),
      HotkeyAction.ToggleSyntaxHighlighting -> List(
        primary('h', shift = true)
      ),
      HotkeyAction.OpenFile -> List(primary('o')),
      HotkeyAction.ToggleCommandRunner -> List(
        primary('p'),
        primaryDoubleTap
      ),
      HotkeyAction.ToggleContextualToolbar -> List(
        primary('t', shift = true)
      ),
      HotkeyAction.NewTab   -> List(primary('t')),
      HotkeyAction.CloseTab -> List(primary('w')),
      HotkeyAction.FileSearch -> List(
        primary('f', shift = true)
      ),
      HotkeyAction.NextTab -> List(primaryKey(InputKey.Tab)),
      HotkeyAction.PreviousTab -> List(
        primaryKey(InputKey.Tab, shift = true),
        primaryKey(InputKey.ReverseTab)
      ),
      HotkeyAction.Find     -> List(primary('f')),
      HotkeyAction.Replace  -> List(if isMac then primary('f', alt = true) else primary('h')),
      HotkeyAction.GoToLine -> List(primary('g')),
      HotkeyAction.SaveAs   -> List(primary('s', shift = true))
    )

  def validate(bindings: Map[HotkeyAction, List[HotkeyTrigger]]): Either[String, Unit] =
    bindings.toList
      .flatMap { case (action, triggers) => triggers.map(_ -> action) }
      .groupMap(_._1)(_._2)
      .collectFirst { case (trigger, actions) if actions.distinct.size > 1 => trigger -> actions.distinct }
      .toLeft(())
      .left
      .map {
        case (trigger, actions) =>
          "Conflicting hotkey binding '" + trigger.render + "' for " + actions.map(_.configKey).mkString(", ")
      }

  private[config] def fromBindings(bindings: Map[HotkeyAction, List[HotkeyTrigger]]): Either[String, HotkeyConfig] =
    validate(bindings).map(_ => HotkeyConfig(bindings))

  private def validatedBindings(
    bindings: Map[HotkeyAction, List[HotkeyTrigger]]
  ): Map[HotkeyAction, List[HotkeyTrigger]] =
    fromBindings(bindings).fold(_ => Map.empty, _.bindings)

  private def addNonConflictingDefaults(
    bindings: Map[HotkeyAction, List[HotkeyTrigger]]
  ): Map[HotkeyAction, List[HotkeyTrigger]] =
    defaultBindings.foldLeft(bindings) {
      case (updated, (action, triggers)) =>
        val conflicts = triggers.exists(trigger => updated.valuesIterator.flatten.contains(trigger))
        if updated.contains(action) || conflicts then updated
        else updated + (action -> triggers)
    }

  given Encoder[HotkeyAction] = Encoder.encodeString.contramap(_.configKey)

  given Decoder[HotkeyAction] = Decoder.decodeString.emap { key =>
    HotkeyAction.values.find(_.configKey == key).toRight(s"Unknown hotkey action: $key")
  }

  given Encoder[InputKey] = Encoder.encodeString.contramap(_.toString)
  given Decoder[InputKey] =
    Decoder.decodeString.emap(key => InputKey.values.find(_.toString == key).toRight(s"Unknown input key: $key"))

  given Encoder[Modifier] = Encoder.encodeString.contramap(_.toString)
  given Decoder[Modifier] =
    Decoder.decodeString.emap(key => Modifier.values.find(_.toString == key).toRight(s"Unknown modifier: $key"))

  given Encoder[HotkeyTrigger] = Encoder.forProduct3("keyType", "character", "modifiers")(trigger =>
    (trigger.keyType, trigger.character, trigger.modifiers)
  )

  given Decoder[HotkeyTrigger] = Decoder.forProduct3("keyType", "character", "modifiers")(HotkeyTrigger.apply)

  given Encoder[HotkeyConfig] = Encoder.instance { config =>
    config.bindings.map { case (action, triggers) => action.configKey -> triggers }.asJson
  }

  given Decoder[HotkeyConfig] = Decoder.decodeMap[String, List[HotkeyTrigger]].emap { bindings =>
    val decoded = bindings.toList.map { (key, triggers) =>
      HotkeyAction.values.find(_.configKey == key).map(_ -> triggers).toRight(s"Unknown hotkey action: $key")
    }
    decoded.collectFirst { case Left(error) => error } match
      case Some(error) => Left(error)
      case None        => fromBindings(addNonConflictingDefaults(decoded.collect { case Right(entry) => entry }.toMap))
  }
