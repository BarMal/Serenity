package com.serenity.ui.tui

import java.nio.charset.StandardCharsets

import com.serenity.keystroke.events.MouseButton

/** The wire form of a keystroke: the bytes a real terminal sends for it, which is the only input a TUI session ever
  * actually receives. Specs are written against this rather than against `KeyStrokeInfo` or `Event` so that everything
  * from the byte decoder upwards stays inside the system under test.
  *
  * `name` exists purely so a failing script can say which key it was pressing.
  */
final case class TuiKey(name: String, bytes: Array[Byte]):
  def ++(next: TuiKey): TuiKey = TuiKey(s"$name $next.name", bytes ++ next.bytes)

/** Byte sequences for the keys, chords, mouse reports and paste framing a terminal emits.
  *
  * Modified keys are encoded the way a terminal that negotiated an extended keyboard protocol reports them: xterm's
  * `CSI 1;<mod><letter>` for cursor keys and the kitty/CSI-u `CSI <code>;<mod>u` form for modified characters, both of
  * which [[TerminalInputDecoder]] decodes. A terminal stuck on the legacy encoding cannot express most of these at all
  * -- that is epic #1103's documented degradation, not something the harness papers over.
  */
object TuiKeys:

  private val Esc: Byte = 0x1b.toByte

  private def bytesOf(text: String): Array[Byte] = text.getBytes(StandardCharsets.UTF_8)
  private def csi(body: String): Array[Byte]     = Esc +: bytesOf(s"[$body")

  /** CSI-u / `modifyCursorKeys` modifier encoding: a bitmask of shift(1), alt(2), ctrl(4), offset by one. */
  private def modifierParam(shift: Boolean, alt: Boolean, ctrl: Boolean): Int =
    1 + (if shift then 1 else 0) + (if alt then 2 else 0) + (if ctrl then 4 else 0)

  def text(value: String): TuiKey = TuiKey(s"'$value'", bytesOf(value))

  def char(value: Char): TuiKey = TuiKey(s"'$value'", bytesOf(value.toString))

  /** A control chord as the terminal encodes it: Ctrl+A is 0x01, Ctrl+Q is 0x11, and so on. */
  def ctrl(letter: Char): TuiKey =
    TuiKey(s"Ctrl+${letter.toUpper}", Array((letter.toUpper - 'A' + 1).toByte))

  /** Ctrl+Shift+letter, which has no legacy encoding at all -- a terminal only reports it in the CSI-u form. */
  def ctrlShift(letter: Char): TuiKey =
    TuiKey(
      s"Ctrl+Shift+${letter.toUpper}",
      csi(s"${letter.toLower.toInt};${modifierParam(shift = true, alt = false, ctrl = true)}u")
    )

  def alt(letter: Char): TuiKey =
    TuiKey(s"Alt+$letter", Esc +: bytesOf(letter.toString))

  val Enter: TuiKey      = TuiKey("Enter", Array(0x0d.toByte))
  val Tab: TuiKey        = TuiKey("Tab", Array(0x09.toByte))
  val ReverseTab: TuiKey = TuiKey("Shift+Tab", csi("Z"))
  val Backspace: TuiKey  = TuiKey("Backspace", Array(0x7f.toByte))
  val Escape: TuiKey     = TuiKey("Escape", Array(Esc))
  val Delete: TuiKey     = TuiKey("Delete", csi("3~"))

  val ArrowUp: TuiKey    = TuiKey("Up", csi("A"))
  val ArrowDown: TuiKey  = TuiKey("Down", csi("B"))
  val ArrowRight: TuiKey = TuiKey("Right", csi("C"))
  val ArrowLeft: TuiKey  = TuiKey("Left", csi("D"))
  val Home: TuiKey       = TuiKey("Home", csi("H"))
  val End: TuiKey        = TuiKey("End", csi("F"))
  val PageUp: TuiKey     = TuiKey("PageUp", csi("5~"))
  val PageDown: TuiKey   = TuiKey("PageDown", csi("6~"))

  val F1: TuiKey = TuiKey("F1", Esc +: bytesOf("OP"))

  /** A cursor key with modifiers held, in the `CSI 1;<mod><letter>` form xterm uses for them.
    *
    * Only valid for the keys a terminal already reports as a CSI sequence -- the arrows, Home and End. A key sent as a
    * bare control byte (Tab, Enter, Backspace, Escape) has no such form: a terminal reports those modified only in the
    * CSI-u encoding, which is what [[csiU]] and the named chords below are for.
    */
  def modified(key: TuiKey, shift: Boolean = false, alt: Boolean = false, ctrl: Boolean = false): TuiKey =
    require(
      key.bytes.length >= 2 && key.bytes(0) == Esc && key.bytes(1) == '['.toByte,
      s"${key.name} is not a CSI key, so it has no CSI-parameter form -- use csiU for its modified encoding"
    )
    val finalByte = key.bytes.lastOption.getOrElse('A'.toByte).toChar
    val label     = List(Option.when(ctrl)("Ctrl"), Option.when(alt)("Alt"), Option.when(shift)("Shift")).flatten
    TuiKey(
      (label :+ key.name).mkString("+"),
      csi(s"1;${modifierParam(shift, alt, ctrl)}$finalByte")
    )

  /** A key in the CSI-u form (`CSI <code>;<mod> u`), which is how a terminal speaking the kitty keyboard protocol
    * reports a modified key that the legacy encoding cannot express at all.
    */
  def csiU(name: String, code: Int, shift: Boolean = false, alt: Boolean = false, ctrl: Boolean = false): TuiKey =
    TuiKey(name, csi(s"$code;${modifierParam(shift, alt, ctrl)}u"))

  val CtrlTab: TuiKey      = csiU("Ctrl+Tab", 9, ctrl = true)
  val CtrlShiftTab: TuiKey = csiU("Ctrl+Shift+Tab", 9, shift = true, ctrl = true)

  /** Bracketed paste: the terminal frames pasted text so the application inserts it as one paste rather than replaying
    * it as individual keystrokes, which would fire hotkeys on any embedded control character.
    */
  def paste(text: String): TuiKey =
    TuiKey(s"paste(${text.length} chars)", csi("200~") ++ bytesOf(text) ++ csi("201~"))

  val FocusIn: TuiKey  = TuiKey("focus-in", csi("I"))
  val FocusOut: TuiKey = TuiKey("focus-out", csi("O"))

  private def buttonCode(button: MouseButton): Int = button match
    case MouseButton.Primary   => 0
    case MouseButton.Middle    => 1
    case MouseButton.Secondary => 2
    case MouseButton.Other     => 3

  /** SGR mouse reporting (`CSI < Cb ; Cx ; Cy M`/`m`, DEC modes 1002/1003/1006), whose coordinates are 1-based. */
  def mousePress(col: Int, row: Int, button: MouseButton = MouseButton.Primary, shift: Boolean = false): TuiKey =
    val code = buttonCode(button) + (if shift then 4 else 0)
    TuiKey(s"press($col,$row)", csi(s"<$code;${col + 1};${row + 1}M"))

  def mouseRelease(col: Int, row: Int, button: MouseButton = MouseButton.Primary, shift: Boolean = false): TuiKey =
    val code = buttonCode(button) + (if shift then 4 else 0)
    TuiKey(s"release($col,$row)", csi(s"<$code;${col + 1};${row + 1}m"))

  def mouseMove(col: Int, row: Int): TuiKey =
    TuiKey(s"move($col,$row)", csi(s"<35;${col + 1};${row + 1}M"))

  def mouseDrag(col: Int, row: Int, button: MouseButton = MouseButton.Primary): TuiKey =
    TuiKey(s"drag($col,$row)", csi(s"<${32 + buttonCode(button)};${col + 1};${row + 1}M"))
end TuiKeys
