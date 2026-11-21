package com.serenity.ui.sdl

import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Every key and modifier number here is read from the SDL headers' output (`SdlGolden`), not from the binding under
  * test, so the translation is held against the pinned release.
  */
class SdlKeyTranslationSpec extends AnyFlatSpec with Matchers:

  private def code(name: String): Int = SdlGolden(name).toInt

  private def press(key: Int, modifiers: Int = 0, repeat: Boolean = false): SdlEvent =
    SdlEvent.Key(down = true, repeat = repeat, scancode = 0, key = key, modifiers = modifiers)

  private def release(key: Int): SdlEvent =
    SdlEvent.Key(down = false, repeat = false, scancode = 0, key = key, modifiers = 0)

  private def stroke(key: InputKey, modifiers: Modifier*): KeyStrokeInfo =
    KeyStrokeInfo(key, None, modifiers.toSet)

  private def character(char: Char, modifiers: Modifier*): KeyStrokeInfo =
    KeyStrokeInfo(InputKey.Character, Some(char), modifiers.toSet)

  private val leftCtrl  = code("SDL_KMOD_LCTRL")
  private val rightCtrl = code("SDL_KMOD_RCTRL")
  private val leftAlt   = code("SDL_KMOD_LALT")
  private val rightAlt  = code("SDL_KMOD_RALT")
  private val leftGui   = code("SDL_KMOD_LGUI")
  private val rightGui  = code("SDL_KMOD_RGUI")
  private val leftShift = code("SDL_KMOD_LSHIFT")
  private val shiftKeys = leftShift | code("SDL_KMOD_RSHIFT")

  private def letter(char: Char): Int = code("SDLK_A") + (char - 'a')

  "SdlKeyTranslation" should "type the text the input method committed, one stroke per character" in {
    SdlKeyTranslation.keystrokes(SdlEvent.TextInput("a")) shouldBe List(character('a'))
    SdlKeyTranslation.keystrokes(SdlEvent.TextInput("é")) shouldBe List(character('é'))
    SdlKeyTranslation.keystrokes(SdlEvent.TextInput("ab")) shouldBe List(character('a'), character('b'))
  }

  it should "pass a character outside the Basic Multilingual Plane as its two UTF-16 halves, in order" in {
    SdlKeyTranslation.keystrokes(SdlEvent.TextInput("😀")) shouldBe
      List(character('\uD83D'), character('\uDE00'))
  }

  it should "drop control characters from committed text" in {
    SdlKeyTranslation.keystrokes(SdlEvent.TextInput("a\nb\u0001")) shouldBe List(character('a'), character('b'))
    SdlKeyTranslation.keystrokes(SdlEvent.TextInput("")) shouldBe Nil
  }

  private val namedKeys: List[(String, InputKey)] =
    List(
      "SDLK_RETURN"    -> InputKey.Enter,
      "SDLK_KP_ENTER"  -> InputKey.Enter,
      "SDLK_BACKSPACE" -> InputKey.Backspace,
      "SDLK_DELETE"    -> InputKey.Delete,
      "SDLK_ESCAPE"    -> InputKey.Escape,
      "SDLK_TAB"       -> InputKey.Tab,
      "SDLK_UP"        -> InputKey.ArrowUp,
      "SDLK_DOWN"      -> InputKey.ArrowDown,
      "SDLK_LEFT"      -> InputKey.ArrowLeft,
      "SDLK_RIGHT"     -> InputKey.ArrowRight,
      "SDLK_HOME"      -> InputKey.Home,
      "SDLK_END"       -> InputKey.End,
      "SDLK_PAGEUP"    -> InputKey.PageUp,
      "SDLK_PAGEDOWN"  -> InputKey.PageDown
    )

  private val functionKeys: List[(String, InputKey)] =
    List(
      InputKey.F1,
      InputKey.F2,
      InputKey.F3,
      InputKey.F4,
      InputKey.F5,
      InputKey.F6,
      InputKey.F7,
      InputKey.F8,
      InputKey.F9,
      InputKey.F10,
      InputKey.F11,
      InputKey.F12
    ).map(key => s"SDLK_${key.toString}" -> key)

  (namedKeys ++ functionKeys).foreach { (name, expected) =>
    it should s"turn $name into $expected" in {
      SdlKeyTranslation.keystrokes(press(code(name))) shouldBe List(stroke(expected))
    }
  }

  it should "send Shift+Tab as ReverseTab without the Shift, as the Swing handler does" in {
    SdlKeyTranslation.keystrokes(press(code("SDLK_TAB"), leftShift)) shouldBe List(stroke(InputKey.ReverseTab))
    SdlKeyTranslation.keystrokes(press(code("SDLK_TAB"), leftShift | leftCtrl)) shouldBe
      List(stroke(InputKey.ReverseTab, Modifier.Ctrl))
  }

  it should "keep the modifiers held with a named key" in {
    SdlKeyTranslation.keystrokes(press(code("SDLK_LEFT"), rightCtrl | leftShift)) shouldBe
      List(stroke(InputKey.ArrowLeft, Modifier.Ctrl, Modifier.Shift))
    SdlKeyTranslation.keystrokes(press(code("SDLK_F5"), rightAlt)) shouldBe List(stroke(InputKey.F5, Modifier.Alt))
    SdlKeyTranslation.keystrokes(press(code("SDLK_RETURN"), leftGui)) shouldBe List(
      stroke(InputKey.Enter, Modifier.Meta)
    )
  }

  it should "send Ctrl+letter, Alt+letter and Meta+letter as the letter with the modifier" in {
    SdlKeyTranslation.keystrokes(press(letter('s'), leftCtrl)) shouldBe List(character('s', Modifier.Ctrl))
    SdlKeyTranslation.keystrokes(press(letter('x'), rightAlt)) shouldBe List(character('x', Modifier.Alt))
    SdlKeyTranslation.keystrokes(press(letter('c'), rightGui)) shouldBe List(character('c', Modifier.Meta))
  }

  it should "keep Shift in a chord and lower-case the letter" in {
    SdlKeyTranslation.keystrokes(press(letter('s'), leftCtrl | leftShift)) shouldBe
      List(character('s', Modifier.Ctrl, Modifier.Shift))
    SdlKeyTranslation.keystrokes(press('S'.toInt, leftCtrl)) shouldBe
      List(character('s', Modifier.Ctrl))
    SdlKeyTranslation.keystrokes(press(letter('x'), leftAlt | shiftKeys)) shouldBe
      List(character('x', Modifier.Alt, Modifier.Shift))
  }

  it should "send Ctrl+digit and Ctrl+slash as characters, as the Swing handler does" in {
    SdlKeyTranslation.keystrokes(press(code("SDLK_0"), leftCtrl)) shouldBe List(character('0', Modifier.Ctrl))
    SdlKeyTranslation.keystrokes(press(code("SDLK_9"), leftCtrl)) shouldBe List(character('9', Modifier.Ctrl))
    SdlKeyTranslation.keystrokes(press(code("SDLK_SLASH"), leftCtrl)) shouldBe List(character('/', Modifier.Ctrl))
  }

  it should "send nothing for a printable key pressed without Ctrl, Alt or Meta: its character arrives as text" in {
    SdlKeyTranslation.keystrokes(press(letter('a'))) shouldBe Nil
    SdlKeyTranslation.keystrokes(press(letter('a'), leftShift)) shouldBe Nil
    SdlKeyTranslation.keystrokes(press(code("SDLK_SPACE"))) shouldBe Nil
    SdlKeyTranslation.keystrokes(press(code("SDLK_0"))) shouldBe Nil
  }

  it should "send nothing for a chord that is not a letter, digit or slash" in {
    SdlKeyTranslation.keystrokes(press(code("SDLK_SPACE"), leftCtrl)) shouldBe Nil
    SdlKeyTranslation.keystrokes(press('['.toInt, leftCtrl)) shouldBe Nil
  }

  it should "ignore a key release" in {
    SdlKeyTranslation.keystrokes(release(code("SDLK_RETURN"))) shouldBe Nil
    SdlKeyTranslation.keystrokes(release(letter('s'))) shouldBe Nil
  }

  it should "let a repeating key through" in {
    SdlKeyTranslation.keystrokes(press(code("SDLK_BACKSPACE"), repeat = true)) shouldBe
      List(stroke(InputKey.Backspace))
    SdlKeyTranslation.keystrokes(press(letter('s'), leftCtrl, repeat = true)) shouldBe
      List(character('s', Modifier.Ctrl))
  }

  it should "send nothing for a modifier key pressed on its own" in
    List("SDLK_LCTRL", "SDLK_RCTRL", "SDLK_LSHIFT", "SDLK_RSHIFT", "SDLK_LALT", "SDLK_RALT", "SDLK_LGUI", "SDLK_RGUI")
      .foreach(name => SdlKeyTranslation.keystrokes(press(code(name))) shouldBe Nil)

  it should "report a key it has no meaning for as Unknown, keeping the modifiers" in {
    SdlKeyTranslation.keystrokes(press(code("SDLK_INSERT"))) shouldBe List(stroke(InputKey.Unknown))
    SdlKeyTranslation.keystrokes(press(code("SDLK_INSERT"), leftCtrl)) shouldBe
      List(stroke(InputKey.Unknown, Modifier.Ctrl))
  }

  it should "not treat Num Lock, Caps Lock or the AltGr flag as modifiers" in {
    val locks = 0x1000 | 0x2000 | 0x4000
    SdlKeyTranslation.keystrokes(press(code("SDLK_RETURN"), locks)) shouldBe List(stroke(InputKey.Enter))
  }

  it should "send nothing for events that are not keys or text" in {
    SdlKeyTranslation.keystrokes(SdlEvent.Quit) shouldBe Nil
    SdlKeyTranslation.keystrokes(SdlEvent.MouseMotion(1f, 2f, 0f, 0f)) shouldBe Nil
    SdlKeyTranslation.keystrokes(SdlEvent.TextEditing("ni", 0, 2)) shouldBe Nil
    SdlKeyTranslation.keystrokes(SdlEvent.ClipboardUpdate) shouldBe Nil
  }
