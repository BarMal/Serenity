package com.serenity.ui.tui

import com.serenity.keystroke.events.{MouseButton, MouseClick, MouseDrag, MouseMove, MousePress}
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import TerminalInputDecoder.DecodedToken

/** Covers #1108's pure byte-sequence decoder: a table of raw terminal input byte strings, each decoded to exactly the
  * [[KeyStrokeInfo]] / mouse-event values `SwingInputHandler` would have produced for the equivalent AWT event. Pure
  * and testable without a real terminal, per the issue's own acceptance criteria.
  */
class TerminalInputDecoderSpec extends AnyFlatSpec with Matchers:

  private val esc = 0x1b.toByte

  private def bytes(s: String): Array[Byte] = s.getBytes(java.nio.charset.StandardCharsets.UTF_8)
  private def csi(s: String): Array[Byte]   = esc +: bytes(s"[$s")

  private def decodeAll(input: Array[Byte]): List[DecodedToken] =
    val result = TerminalInputDecoder.decode(input)
    result.remainder shouldBe empty
    result.tokens

  private def tok(k: InputKey, ch: Option[Char] = None, mods: Set[Modifier] = Set.empty): DecodedToken.Key =
    DecodedToken.Key(KeyStrokeInfo(k, ch, mods))

  "the decoder" should "decode a plain printable character" in {
    decodeAll(bytes("a")) shouldBe List(tok(InputKey.Character, Some('a')))
  }

  it should "decode a multi-byte UTF-8 character as a single Character token" in {
    decodeAll(bytes("é")) shouldBe List(tok(InputKey.Character, Some('é')))
  }

  it should "decode the arrow keys" in {
    decodeAll(csi("A")) shouldBe List(tok(InputKey.ArrowUp))
    decodeAll(csi("B")) shouldBe List(tok(InputKey.ArrowDown))
    decodeAll(csi("C")) shouldBe List(tok(InputKey.ArrowRight))
    decodeAll(csi("D")) shouldBe List(tok(InputKey.ArrowLeft))
  }

  it should "decode Home and End in both the xterm and VT220 forms" in {
    decodeAll(csi("H")) shouldBe List(tok(InputKey.Home))
    decodeAll(csi("F")) shouldBe List(tok(InputKey.End))
    decodeAll(csi("1~")) shouldBe List(tok(InputKey.Home))
    decodeAll(csi("4~")) shouldBe List(tok(InputKey.End))
  }

  it should "decode PageUp and PageDown" in {
    decodeAll(csi("5~")) shouldBe List(tok(InputKey.PageUp))
    decodeAll(csi("6~")) shouldBe List(tok(InputKey.PageDown))
  }

  it should "decode F1 through F12" in {
    decodeAll(esc +: bytes("OP")) shouldBe List(tok(InputKey.F1))
    decodeAll(esc +: bytes("OQ")) shouldBe List(tok(InputKey.F2))
    decodeAll(esc +: bytes("OR")) shouldBe List(tok(InputKey.F3))
    decodeAll(esc +: bytes("OS")) shouldBe List(tok(InputKey.F4))
    decodeAll(csi("15~")) shouldBe List(tok(InputKey.F5))
    decodeAll(csi("17~")) shouldBe List(tok(InputKey.F6))
    decodeAll(csi("18~")) shouldBe List(tok(InputKey.F7))
    decodeAll(csi("19~")) shouldBe List(tok(InputKey.F8))
    decodeAll(csi("20~")) shouldBe List(tok(InputKey.F9))
    decodeAll(csi("21~")) shouldBe List(tok(InputKey.F10))
    decodeAll(csi("23~")) shouldBe List(tok(InputKey.F11))
    decodeAll(csi("24~")) shouldBe List(tok(InputKey.F12))
  }

  it should "decode Tab and Shift+Tab (ReverseTab)" in {
    decodeAll(Array(0x09.toByte)) shouldBe List(tok(InputKey.Tab))
    decodeAll(csi("Z")) shouldBe List(tok(InputKey.ReverseTab))
  }

  it should "decode Backspace and Delete" in {
    decodeAll(Array(0x7f.toByte)) shouldBe List(tok(InputKey.Backspace))
    decodeAll(Array(0x08.toByte)) shouldBe List(tok(InputKey.Backspace))
    decodeAll(csi("3~")) shouldBe List(tok(InputKey.Delete))
  }

  it should "decode Enter" in {
    decodeAll(Array(0x0d.toByte)) shouldBe List(tok(InputKey.Enter))
  }

  it should "decode Ctrl+letter from control bytes" in {
    decodeAll(Array(0x01.toByte)) shouldBe List(tok(InputKey.Character, Some('a'), Set(Modifier.Ctrl)))
    decodeAll(Array(0x1a.toByte)) shouldBe List(tok(InputKey.Character, Some('z'), Set(Modifier.Ctrl)))
  }

  it should "resolve the Ctrl+I / Tab and Ctrl+M / Enter legacy collisions to the named key" in {
    decodeAll(Array(0x09.toByte)) shouldBe List(tok(InputKey.Tab))
    decodeAll(Array(0x0d.toByte)) shouldBe List(tok(InputKey.Enter))
  }

  it should "decode Alt+letter from an ESC prefix" in {
    decodeAll(esc +: bytes("a")) shouldBe List(tok(InputKey.Character, Some('a'), Set(Modifier.Alt)))
  }

  it should "decode Alt+Ctrl+letter from an ESC-prefixed control byte" in {
    decodeAll(Array(esc, 0x01.toByte)) shouldBe List(
      tok(InputKey.Character, Some('a'), Set(Modifier.Alt, Modifier.Ctrl))
    )
  }

  it should "leave a lone ESC as an incomplete remainder awaiting disambiguation" in {
    val result = TerminalInputDecoder.decode(Array(esc))
    result.tokens shouldBe Nil
    result.remainder shouldBe Array(esc)
  }

  it should "resolve a lone ESC remainder to bare Escape via decodeFinal" in {
    TerminalInputDecoder.decodeFinal(Array(esc)) shouldBe List(tok(InputKey.Escape))
  }

  it should "leave a partial CSI sequence as a remainder rather than misdecoding it" in {
    val result = TerminalInputDecoder.decode(Array(esc, '['.toByte))
    result.tokens shouldBe Nil
    result.remainder shouldBe Array(esc, '['.toByte)
  }

  it should "decode an SGR mouse press" in {
    decodeAll(csi("<0;10;5M")) shouldBe List(
      DecodedToken.Mouse(MousePress(col = 9, row = 4, shiftDown = false, button = MouseButton.Primary))
    )
  }

  it should "decode an SGR mouse release as a click" in {
    decodeAll(csi("<0;10;5m")) shouldBe List(
      DecodedToken.Mouse(MouseClick(col = 9, row = 4, clickCount = 1, shiftDown = false, button = MouseButton.Primary))
    )
  }

  it should "decode an SGR mouse drag (button held, motion bit set)" in {
    decodeAll(csi("<32;10;5M")) shouldBe List(
      DecodedToken.Mouse(MouseDrag(col = 9, row = 4, shiftDown = false, button = MouseButton.Primary))
    )
  }

  it should "decode an SGR any-motion move (mode 1003, no button held) as MouseMove" in {
    decodeAll(csi("<35;10;5M")) shouldBe List(
      DecodedToken.Mouse(MouseMove(col = 9, row = 4, shiftDown = false))
    )
  }

  it should "decode the right and middle mouse buttons, and the Shift bit" in {
    decodeAll(csi("<2;1;1M")) shouldBe List(
      DecodedToken.Mouse(MousePress(col = 0, row = 0, shiftDown = false, button = MouseButton.Secondary))
    )
    decodeAll(csi("<1;1;1M")) shouldBe List(
      DecodedToken.Mouse(MousePress(col = 0, row = 0, shiftDown = false, button = MouseButton.Middle))
    )
    decodeAll(csi("<4;1;1M")) shouldBe List(
      DecodedToken.Mouse(MousePress(col = 0, row = 0, shiftDown = true, button = MouseButton.Primary))
    )
  }

  it should "decode a bracketed paste block as a single Pasted token, not individual keystrokes" in {
    val pasted = esc +: bytes("[200~line one\nline two~") // literal '~' mid-paste must not be mistaken for a terminator
    val input  = pasted ++ (esc +: bytes("[201~"))
    decodeAll(input) shouldBe List(DecodedToken.Pasted("line one\nline two~"))
  }

  it should "decode a bracketed paste containing characters that would otherwise be hotkeys" in {
    val text  = "ctrl+q is not a hotkey here"
    val input = (esc +: bytes("[200~")) ++ bytes(text) ++ (esc +: bytes("[201~"))
    decodeAll(input) shouldBe List(DecodedToken.Pasted(text))
  }

  it should "leave an unterminated bracketed paste as a remainder" in {
    val input  = (esc +: bytes("[200~")) ++ bytes("still typing")
    val result = TerminalInputDecoder.decode(input)
    result.tokens shouldBe Nil
    result.remainder shouldBe input
  }

  it should "resolve an unterminated bracketed paste at EOF via decodeFinal, best-effort" in {
    val input = (esc +: bytes("[200~")) ++ bytes("cut off")
    TerminalInputDecoder.decodeFinal(input) shouldBe List(DecodedToken.Pasted("cut off"))
  }

  it should "decode multiple tokens back to back from a single buffer" in {
    val input = bytes("ab") ++ csi("A") ++ Array(0x0d.toByte)
    decodeAll(input) shouldBe List(
      tok(InputKey.Character, Some('a')),
      tok(InputKey.Character, Some('b')),
      tok(InputKey.ArrowUp),
      tok(InputKey.Enter)
    )
  }

  // ===CSI-u (fixterms): kitty-protocol and xterm modifyOtherKeys/formatOtherKeys=1 share this exact wire shape --
  // `CSI keycode[:alt-codes] ; modifiers[:event][;text] u` -- so one decoding path serves both (#1109).===

  it should "decode Ctrl+A and Ctrl+Shift+A as distinct CSI-u sequences, unlike the legacy control-byte collapse" in {
    decodeAll(csi("97;5u")) shouldBe List(tok(InputKey.Character, Some('a'), Set(Modifier.Ctrl)))
    decodeAll(csi("97;6u")) shouldBe List(tok(InputKey.Character, Some('a'), Set(Modifier.Ctrl, Modifier.Shift)))
  }

  it should "decode Ctrl+Enter as distinct from bare Enter via CSI-u" in {
    decodeAll(csi("13u")) shouldBe List(tok(InputKey.Enter))
    decodeAll(csi("13;5u")) shouldBe List(tok(InputKey.Enter, mods = Set(Modifier.Ctrl)))
  }

  it should "decode CSI-u Tab and Shift+Tab, collapsing Shift into ReverseTab as the legacy path does" in {
    decodeAll(csi("9u")) shouldBe List(tok(InputKey.Tab))
    decodeAll(csi("9;2u")) shouldBe List(tok(InputKey.ReverseTab))
    decodeAll(csi("9;6u")) shouldBe List(tok(InputKey.ReverseTab, mods = Set(Modifier.Ctrl)))
  }

  it should "decode CSI-u Backspace and Escape with modifiers" in {
    decodeAll(csi("127;2u")) shouldBe List(tok(InputKey.Backspace, mods = Set(Modifier.Shift)))
    decodeAll(csi("27;3u")) shouldBe List(tok(InputKey.Escape, mods = Set(Modifier.Alt)))
  }

  it should "decode every CSI-u modifier bit (shift/alt/ctrl/meta)" in {
    decodeAll(csi("97;2u")) shouldBe List(tok(InputKey.Character, Some('a'), Set(Modifier.Shift)))
    decodeAll(csi("97;3u")) shouldBe List(tok(InputKey.Character, Some('a'), Set(Modifier.Alt)))
    decodeAll(csi("97;5u")) shouldBe List(tok(InputKey.Character, Some('a'), Set(Modifier.Ctrl)))
    decodeAll(csi("97;33u")) shouldBe List(tok(InputKey.Character, Some('a'), Set(Modifier.Meta)))
    decodeAll(csi("97;8u")) shouldBe List(
      tok(InputKey.Character, Some('a'), Set(Modifier.Shift, Modifier.Alt, Modifier.Ctrl))
    )
  }

  it should "decode CSI-u repeat events as an ordinary key stroke" in {
    decodeAll(csi("97;5:2u")) shouldBe List(tok(InputKey.Character, Some('a'), Set(Modifier.Ctrl)))
  }

  it should "drop CSI-u release events for ordinary keys, rather than emitting a second stroke" in {
    decodeAll(csi("97;5:3u")) shouldBe Nil
  }

  it should "decode bare kitty-protocol modifier press/release as ModifierEdge tokens, not Key tokens" in {
    decodeAll(csi("57442u")) shouldBe List(DecodedToken.ModifierEdge(Modifier.Ctrl, pressed = true))
    decodeAll(csi("57442;1:3u")) shouldBe List(DecodedToken.ModifierEdge(Modifier.Ctrl, pressed = false))
    decodeAll(csi("57448u")) shouldBe List(DecodedToken.ModifierEdge(Modifier.Ctrl, pressed = true)) // right Ctrl
    decodeAll(csi("57441u")) shouldBe List(DecodedToken.ModifierEdge(Modifier.Shift, pressed = true))
    decodeAll(csi("57443u")) shouldBe List(DecodedToken.ModifierEdge(Modifier.Alt, pressed = true))
    decodeAll(csi("57446u")) shouldBe List(DecodedToken.ModifierEdge(Modifier.Meta, pressed = true))
  }

  it should "treat a kitty-protocol modifier repeat event as another press edge" in {
    decodeAll(csi("57442;1:2u")) shouldBe List(DecodedToken.ModifierEdge(Modifier.Ctrl, pressed = true))
  }

  it should "decode a modifyOtherKeys/formatOtherKeys=1 combo identically to the equivalent basic kitty CSI-u sequence" in {
    // modifyOtherKeys mode 2 + formatOtherKeys=1 never sends the event-type or alternate-key subfields kitty can --
    // it is a strict subset of the same wire shape, so the same code path must decode it identically.
    decodeAll(csi("13;5u")) shouldBe decodeAll(csi("13;5:1u"))
    decodeAll(csi("97;6u")) shouldBe List(tok(InputKey.Character, Some('a'), Set(Modifier.Ctrl, Modifier.Shift)))
  }
