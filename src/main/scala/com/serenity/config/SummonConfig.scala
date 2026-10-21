package com.serenity.config

import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

/** A piece of window chrome that can be summoned by holding a key chord, whatever the config says about it. */
enum ChromeElement(val configKey: String, val defaultLetter: Char):
  case LineNumbers extends ChromeElement("line_numbers", 'l')
  case StatusBar   extends ChromeElement("status_bar", 's')
  case TabStrip    extends ChromeElement("tab_strip", 't')
  case Scrollbar   extends ChromeElement("scrollbar", 'b')
  case PaneHeaders extends ChromeElement("pane_headers", 'h')

object ChromeElement:

  def fromConfigKey(value: String): Option[ChromeElement] =
    ChromeElement.values.find(_.configKey == value.trim.toLowerCase(java.util.Locale.ROOT))

/** The keys that summon chrome: the base `modifiers` held with an element's letter shows it for as long as the chord is
  * held, and `lock` pressed during the hold keeps it shown. A letter of `None` leaves that element with no chord.
  */
final case class SummonConfig(
    modifiers: Set[Modifier] = SummonConfig.DefaultModifiers,
    lock: HotkeyTrigger = SummonConfig.DefaultLock,
    letters: Map[ChromeElement, Char] = SummonConfig.DefaultLetters
):

  def letterOf(element: ChromeElement): Option[Char] = letters.get(element)

  def withLetter(element: ChromeElement, letter: Option[Char]): SummonConfig =
    copy(letters = letter.fold(letters - element)(letters.updated(element, _)))

  def elementForLetter(letter: Char): Option[ChromeElement] =
    ChromeElement.values.find(letterOf(_).contains(letter.toLower))

  /** The element a terminal keystroke summons. A terminal that cannot report Shift with a letter sends a capital, which
    * stands for the Shift it dropped.
    */
  def elementFor(info: KeyStrokeInfo): Option[ChromeElement] =
    info.character.filter(_ => info.keyType == InputKey.Character).flatMap { character =>
      val shifted = if character.isUpper then info.modifiers + Modifier.Shift else info.modifiers
      Option.when(shifted == modifiers)(elementForLetter(character)).flatten
    }

  def chordOf(element: ChromeElement): Option[HotkeyTrigger] =
    letterOf(element).map(letter => HotkeyTrigger(InputKey.Character, Some(letter), modifiers))

  /** This config with every chord that cannot work taken out, and what was taken out and why.
    *
    * A letter two elements share goes to the first of them; a chord that a global hotkey, a command hotkey or a focused
    * keymap already holds goes to that binding, which the user set first; a lock key that is also a letter goes back to
    * the default, since a press of it could not tell lock from summon.
    */
  def settled(hotkeys: HotkeyConfig, keymaps: FocusedKeymapConfig): (SummonConfig, List[HotkeyConflict]) =
    val held = SummonConfig.hotkeyHolders(hotkeys, keymaps)
    val (kept, letterConflicts) =
      ChromeElement.values.toList.foldLeft((this, List.empty[HotkeyConflict])) {
        case ((config, conflicts), element) =>
          chordOf(element).fold((config, conflicts)) { chord =>
            val earlierElement = ChromeElement.values.takeWhile(_ != element).find(letterOf(_) == letterOf(element))
            val keeper         = earlierElement.map(other => s"ui.summon.${other.configKey}").orElse(held.get(chord))
            keeper.fold((config, conflicts)) { keptBy =>
              (
                config.withLetter(element, None),
                conflicts :+ HotkeyConflict(chord.render, keptBy, List(s"ui.summon.${element.configKey}"))
              )
            }
          }
      }
    if kept.lockIsALetter then
      (
        kept.copy(lock = SummonConfig.DefaultLock),
        letterConflicts :+ HotkeyConflict(kept.lock.render, "ui.summon.letters", List("ui.summon.lock"))
      )
    else (kept, letterConflicts)

  /** The change, unless it would leave a chord that cannot work, as when a letter is typed into the settings list. */
  def withLetterChecked(
    element: ChromeElement,
    letter: Option[Char],
    hotkeys: HotkeyConfig,
    keymaps: FocusedKeymapConfig
  ): Option[SummonConfig] =
    checked(withLetter(element, letter), hotkeys, keymaps)

  def withModifiersChecked(
    newModifiers: Set[Modifier],
    hotkeys: HotkeyConfig,
    keymaps: FocusedKeymapConfig
  ): Option[SummonConfig] =
    checked(copy(modifiers = newModifiers), hotkeys, keymaps)

  def withLockChecked(newLock: HotkeyTrigger): Option[SummonConfig] =
    Some(copy(lock = newLock)).filterNot(_.lockIsALetter)

  private def lockIsALetter: Boolean =
    letters.values.exists(letter => lock == HotkeyTrigger(InputKey.Character, Some(letter), Set.empty))

  private def checked(candidate: SummonConfig, hotkeys: HotkeyConfig, keymaps: FocusedKeymapConfig) =
    Some(candidate).filter(_.settled(hotkeys, keymaps)._2.isEmpty)

object SummonConfig:

  val DefaultModifiers: Set[Modifier] = Set(Modifier.Alt, Modifier.Shift)

  val DefaultLock: HotkeyTrigger = HotkeyTrigger(InputKey.Character, Some(' '), Set.empty)

  val DefaultLetters: Map[ChromeElement, Char] =
    ChromeElement.values.map(element => element -> element.defaultLetter).toMap

  private val lockKeys: Set[InputKey] = Set(InputKey.Character, InputKey.Enter, InputKey.Tab, InputKey.Backspace)

  /** Modifiers written the way a hotkey writes them: `alt+shift`. Shift alone is no chord, since it types capitals. */
  def parseModifiers(text: String): Option[Set[Modifier]] =
    val parts = text.trim.toLowerCase(java.util.Locale.ROOT).split("\\+").toList.map(_.trim)
    val parsed = parts.map {
      case "ctrl"                     => Some(Modifier.Ctrl)
      case "alt"                      => Some(Modifier.Alt)
      case "shift"                    => Some(Modifier.Shift)
      case "meta" | "cmd" | "command" => Some(Modifier.Meta)
      case _                          => None
    }
    Option
      .when(parsed.nonEmpty && parsed.forall(_.isDefined))(parsed.flatten.toSet)
      .filter(_ != Set(Modifier.Shift))

  def renderModifiers(modifiers: Set[Modifier]): String =
    List(Modifier.Ctrl -> "ctrl", Modifier.Alt -> "alt", Modifier.Meta -> "meta", Modifier.Shift -> "shift")
      .collect { case (modifier, name) if modifiers.contains(modifier) => name }
      .mkString("+")

  /** `Some(None)` is `off`: the element has no chord. */
  def parseLetter(text: String): Option[Option[Char]] =
    text.trim.toLowerCase(java.util.Locale.ROOT) match
      case "off" => Some(None)
      case single if single.length == 1 && single.head.isLetterOrDigit && single.head.toInt < 128 =>
        Some(Some(single.head))
      case _ => None

  def renderLetter(letter: Option[Char]): String = letter.fold("off")(_.toString)

  def parseLock(text: String): Option[HotkeyTrigger] =
    HotkeyTrigger.parse(text).filter(trigger => trigger.modifiers.isEmpty && lockKeys.contains(trigger.keyType))

  def renderLock(lock: HotkeyTrigger): String = lock.render

  /** Every chord a binding already holds, by the name of the binding that holds it. */
  private def hotkeyHolders(hotkeys: HotkeyConfig, keymaps: FocusedKeymapConfig): Map[HotkeyTrigger, String] =
    val global   = HotkeyConfig.actionTargets(hotkeys.bindings).sortBy(_._1)
    val commands = hotkeys.commandBindings.toList.sortBy(_._1).map((id, triggers) => s"command.$id" -> triggers)
    (global ++ commands ++ keymaps.allBindings)
      .flatMap((holder, triggers) => triggers.map(_ -> holder))
      .reverse
      .toMap
