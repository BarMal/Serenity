package com.serenity.config

import io.circe.syntax.given
import io.circe.{Decoder, Encoder}

import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

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
  case NewTab
  case CloseTab
  case FileSearch
  case NextTab
  case PreviousTab

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
      case NewTab                   => "new_tab"
      case CloseTab                 => "close_tab"
      case FileSearch               => "file_search"
      case NextTab                  => "next_tab"
      case PreviousTab              => "previous_tab"

case class HotkeyTrigger(
    keyType: InputKey,
    character: Option[Char],
    modifiers: Set[Modifier]
):
  def matches(info: KeyStrokeInfo): Boolean =
    info.keyType == keyType && info.character == character && info.modifiers == modifiers

  def render: String =
    val modifierParts =
      List(
        Option.when(modifiers.contains(Modifier.Ctrl))("ctrl"),
        Option.when(modifiers.contains(Modifier.Alt))("alt"),
        Option.when(modifiers.contains(Modifier.Shift))("shift")
      ).flatten
    val keyPart =
      keyType match
        case InputKey.Character => character.map(_.toString).getOrElse("")
        case InputKey.Tab       => "tab"
        case InputKey.ReverseTab => "reverse-tab"
        case InputKey.EOF       => "eof"
        case other              => other.toString.toLowerCase
    (modifierParts :+ keyPart).mkString("+")

object HotkeyTrigger:
  def parse(input: String): Option[HotkeyTrigger] =
    val parts = input.trim.toLowerCase.split("\\+").toList.map(_.trim).filter(_.nonEmpty)
    val (modifierParts, keyParts) = parts.partition {
      case "ctrl" | "alt" | "shift" => true
      case _                         => false
    }

    val modifiers = modifierParts.foldLeft(Set.empty[Modifier]) {
      case (acc, "ctrl")  => acc + Modifier.Ctrl
      case (acc, "alt")   => acc + Modifier.Alt
      case (acc, "shift") => acc + Modifier.Shift
      case (acc, _)       => acc
    }

    keyParts match
      case key :: Nil =>
        key match
          case "tab" =>
            Some(HotkeyTrigger(InputKey.Tab, None, modifiers))
          case "reverse-tab" | "reverse_tab" =>
            Some(HotkeyTrigger(InputKey.ReverseTab, None, modifiers))
          case "eof" =>
            Some(HotkeyTrigger(InputKey.EOF, None, modifiers))
          case single if single.length == 1 =>
            Some(HotkeyTrigger(InputKey.Character, Some(single.head), modifiers))
          case _ =>
            None
      case _ =>
        None

case class HotkeyConfig(
    bindings: Map[HotkeyAction, List[HotkeyTrigger]] = HotkeyConfig.defaultBindings
):
  def bindingsFor(action: HotkeyAction): List[HotkeyTrigger] =
    bindings.getOrElse(action, Nil)

  def withBinding(action: HotkeyAction, trigger: HotkeyTrigger): HotkeyConfig =
    copy(bindings = bindings + (action -> List(trigger)))

  def withBinding(action: HotkeyAction, binding: String): HotkeyConfig =
    HotkeyTrigger.parse(binding).map(trigger => withBinding(action, trigger)).getOrElse(this)

object HotkeyConfig:
  val defaultBindings: Map[HotkeyAction, List[HotkeyTrigger]] = Map(
    HotkeyAction.Save -> List(HotkeyTrigger(InputKey.Character, Some('s'), Set(Modifier.Ctrl))),
    HotkeyAction.Quit -> List(
      HotkeyTrigger(InputKey.Character, Some('q'), Set(Modifier.Ctrl)),
      HotkeyTrigger(InputKey.EOF, None, Set.empty)
    ),
    HotkeyAction.Undo -> List(HotkeyTrigger(InputKey.Character, Some('z'), Set(Modifier.Ctrl))),
    HotkeyAction.Redo -> List(HotkeyTrigger(InputKey.Character, Some('y'), Set(Modifier.Ctrl))),
    HotkeyAction.Copy -> List(HotkeyTrigger(InputKey.Character, Some('c'), Set(Modifier.Ctrl))),
    HotkeyAction.Paste -> List(HotkeyTrigger(InputKey.Character, Some('v'), Set(Modifier.Ctrl))),
    HotkeyAction.Cut -> List(HotkeyTrigger(InputKey.Character, Some('x'), Set(Modifier.Ctrl))),
    HotkeyAction.SelectAll -> List(HotkeyTrigger(InputKey.Character, Some('a'), Set(Modifier.Ctrl))),
    HotkeyAction.ToggleSyntaxHighlighting -> List(
      HotkeyTrigger(InputKey.Character, Some('h'), Set(Modifier.Ctrl))
    ),
    HotkeyAction.OpenFile -> List(HotkeyTrigger(InputKey.Character, Some('o'), Set(Modifier.Ctrl))),
    HotkeyAction.ToggleCommandRunner -> List(
      HotkeyTrigger(InputKey.Character, Some('p'), Set(Modifier.Ctrl))
    ),
    HotkeyAction.NewTab -> List(HotkeyTrigger(InputKey.Character, Some('t'), Set(Modifier.Ctrl))),
    HotkeyAction.CloseTab -> List(HotkeyTrigger(InputKey.Character, Some('w'), Set(Modifier.Ctrl))),
    HotkeyAction.FileSearch -> List(
      HotkeyTrigger(InputKey.Character, Some('f'), Set(Modifier.Ctrl, Modifier.Shift))
    ),
    HotkeyAction.NextTab -> List(HotkeyTrigger(InputKey.Tab, None, Set(Modifier.Ctrl))),
    HotkeyAction.PreviousTab -> List(
      HotkeyTrigger(InputKey.Tab, None, Set(Modifier.Ctrl, Modifier.Shift)),
      HotkeyTrigger(InputKey.ReverseTab, None, Set(Modifier.Ctrl))
    )
  )

  given Encoder[HotkeyAction] = Encoder.encodeString.contramap(_.configKey)
  given Decoder[HotkeyAction] = Decoder.decodeString.emap { key =>
    HotkeyAction.values.find(_.configKey == key).toRight(s"Unknown hotkey action: $key")
  }

  given Encoder[InputKey] = Encoder.encodeString.contramap(_.toString)
  given Decoder[InputKey] = Decoder.decodeString.emap { key =>
    InputKey.values.find(_.toString == key).toRight(s"Unknown input key: $key")
  }

  given Encoder[Modifier] = Encoder.encodeString.contramap(_.toString)
  given Decoder[Modifier] = Decoder.decodeString.emap { key =>
    Modifier.values.find(_.toString == key).toRight(s"Unknown modifier: $key")
  }

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
      case None        => Right(HotkeyConfig(defaultBindings ++ decoded.collect { case Right(entry) => entry }.toMap))
  }
