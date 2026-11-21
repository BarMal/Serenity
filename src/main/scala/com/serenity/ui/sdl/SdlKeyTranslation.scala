package com.serenity.ui.sdl

import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

/** Turns SDL's key and text events into the strokes the Swing handler produces for the same typing.
  *
  * A printable key without Ctrl, Alt or Meta yields nothing here: SDL reports the character it types as a separate
  * `TextInput`, and translating both would type it twice. Chords follow the Swing handler's set (letters, digits and
  * the slash), so a hotkey bound for Swing matches under SDL. AltGr, dead keys and Option glyphs are not handled.
  */
object SdlKeyTranslation:

  def keystrokes(event: SdlEvent): List[KeyStrokeInfo] =
    event match
      case SdlEvent.TextInput(text) =>
        text.toList
          .filterNot(char => Character.isISOControl(char))
          .map(char => KeyStrokeInfo(InputKey.Character, Some(char), Set.empty))
      case SdlEvent.Key(true, _, _, key, flags) => pressed(key, modifierSet(flags))
      case _                                    => Nil

  private def code(name: String): Int = SdlLayouts.keycode(name)

  private val namedKeys: Map[Int, InputKey] =
    Map(
      code("SDLK_RETURN")    -> InputKey.Enter,
      code("SDLK_KP_ENTER")  -> InputKey.Enter,
      code("SDLK_BACKSPACE") -> InputKey.Backspace,
      code("SDLK_DELETE")    -> InputKey.Delete,
      code("SDLK_ESCAPE")    -> InputKey.Escape,
      code("SDLK_UP")        -> InputKey.ArrowUp,
      code("SDLK_DOWN")      -> InputKey.ArrowDown,
      code("SDLK_LEFT")      -> InputKey.ArrowLeft,
      code("SDLK_RIGHT")     -> InputKey.ArrowRight,
      code("SDLK_HOME")      -> InputKey.Home,
      code("SDLK_END")       -> InputKey.End,
      code("SDLK_PAGEUP")    -> InputKey.PageUp,
      code("SDLK_PAGEDOWN")  -> InputKey.PageDown,
      code("SDLK_F1")        -> InputKey.F1,
      code("SDLK_F2")        -> InputKey.F2,
      code("SDLK_F3")        -> InputKey.F3,
      code("SDLK_F4")        -> InputKey.F4,
      code("SDLK_F5")        -> InputKey.F5,
      code("SDLK_F6")        -> InputKey.F6,
      code("SDLK_F7")        -> InputKey.F7,
      code("SDLK_F8")        -> InputKey.F8,
      code("SDLK_F9")        -> InputKey.F9,
      code("SDLK_F10")       -> InputKey.F10,
      code("SDLK_F11")       -> InputKey.F11,
      code("SDLK_F12")       -> InputKey.F12
    )

  private val modifierKeys: Set[Int] =
    List("LCTRL", "RCTRL", "LSHIFT", "RSHIFT", "LALT", "RALT", "LGUI", "RGUI").map(name => code(s"SDLK_$name")).toSet

  private val tab: Int          = code("SDLK_TAB")
  private val extendedKeys: Int = code("SDLK_SCANCODE_MASK")

  private def pressed(key: Int, modifiers: Set[Modifier]): List[KeyStrokeInfo] =
    if key == tab then List(tabStroke(modifiers))
    else
      namedKeys.get(key) match
        case Some(inputKey)                     => List(KeyStrokeInfo(inputKey, None, modifiers))
        case None if modifierKeys.contains(key) => Nil
        case None if (key & extendedKeys) != 0  => List(KeyStrokeInfo(InputKey.Unknown, None, modifiers))
        case None                               => chord(key, modifiers).toList

  /** Shift is carried by the key type, not the modifiers, so a translator only sees the Ctrl and Alt it acts on. */
  private def tabStroke(modifiers: Set[Modifier]): KeyStrokeInfo =
    if modifiers.contains(Modifier.Shift) then KeyStrokeInfo(InputKey.ReverseTab, None, modifiers - Modifier.Shift)
    else KeyStrokeInfo(InputKey.Tab, None, modifiers)

  private def chord(key: Int, modifiers: Set[Modifier]): Option[KeyStrokeInfo] =
    val held = modifiers.exists(modifier => modifier != Modifier.Shift)
    Option
      .when(held)(chordCharacter(key))
      .flatten
      .map(char => KeyStrokeInfo(InputKey.Character, Some(char), modifiers))

  /** SDL names a letter key by its lower-case ASCII code whatever Shift does; the upper case is folded for safety. */
  private def chordCharacter(key: Int): Option[Char] =
    val char = key.toChar
    Option.when(key >= 0 && key <= Char.MaxValue && isChordCharacter(char))(char.toLower)

  private def isChordCharacter(char: Char): Boolean =
    (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || (char >= '0' && char <= '9') || char == '/'

  private val modifierBits: List[(Modifier, List[Int])] =
    List(
      Modifier.Ctrl  -> List("SDL_KMOD_LCTRL", "SDL_KMOD_RCTRL"),
      Modifier.Alt   -> List("SDL_KMOD_LALT", "SDL_KMOD_RALT"),
      Modifier.Shift -> List("SDL_KMOD_LSHIFT", "SDL_KMOD_RSHIFT"),
      Modifier.Meta  -> List("SDL_KMOD_LGUI", "SDL_KMOD_RGUI")
    ).map((modifier, names) => modifier -> names.map(SdlLayouts.keyModifier))

  /** Lock and AltGr flags are not modifiers a hotkey can name, so only the four keys' bits count. */
  private def modifierSet(flags: Int): Set[Modifier] =
    modifierBits.collect { case (modifier, bits) if bits.exists(bit => (flags & bit) != 0) => modifier }.toSet
