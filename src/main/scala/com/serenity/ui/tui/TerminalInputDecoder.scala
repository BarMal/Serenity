package com.serenity.ui.tui

import java.nio.charset.StandardCharsets

import com.serenity.keystroke.events.{MouseButton, MouseClick, MouseDrag, MouseInputEvent, MouseMove, MousePress}
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}

/** Pure decoding of raw terminal input bytes into the same [[KeyStrokeInfo]] / mouse-event vocabulary
  * `SwingInputHandler` produces from AWT events (`SwingInputHandler.scala:229-317`), plus the one extra token kind only
  * a terminal produces: bracketed-paste text.
  *
  * Total and side-effect-free over a byte buffer: no I/O, no blocking, no notion of "wait a little longer for more
  * bytes." That split is what makes this testable with plain byte arrays (`TerminalInputDecoderSpec`) -- the
  * ESC-disambiguation deadline and the JLine read loop live in [[TerminalInputHandler]] instead.
  *
  * ===Legacy-protocol collapses (documented, not fixed here)===
  *
  * Raw terminal byte sequences are a strictly lossier vocabulary than AWT's `KeyEvent`, and some collisions are
  * unresolvable without a richer protocol (kitty's, tracked in a follow-up issue):
  *   - Ctrl+Shift+`letter` is indistinguishable from Ctrl+`letter` -- terminals send the same control byte either way,
  *     so `Set(Modifier.Shift)` is never present alongside `Modifier.Ctrl` here.
  *   - `0x09` is both the Tab key and Ctrl+I; this decoder always resolves it to [[InputKey.Tab]], matching
  *     `SwingInputHandler`'s own precedence (`VK_TAB` is matched before the generic Ctrl+letter branch).
  *   - `0x0D` is both the Enter key and Ctrl+M; this decoder always resolves it to [[InputKey.Enter]], for the same
  *     reason.
  */
object TerminalInputDecoder:

  sealed trait DecodedToken

  object DecodedToken:
    final case class Key(info: KeyStrokeInfo)      extends DecodedToken
    final case class Mouse(event: MouseInputEvent) extends DecodedToken
    final case class Pasted(text: String)          extends DecodedToken

  /** @param tokens
    *   tokens fully decoded from the front of `bytes`, in order.
    * @param remainder
    *   trailing bytes that form an incomplete sequence -- a partial UTF-8 character, a partial escape sequence, an
    *   unterminated bracketed paste, or a lone `ESC` awaiting disambiguation. The caller feeds more bytes in (appended
    *   to this) and decodes again; on end-of-input or a disambiguation-deadline expiry it instead calls [[decodeFinal]]
    *   on this remainder.
    */
  final case class DecodeResult(tokens: List[DecodedToken], remainder: Array[Byte])

  private val Esc: Byte   = 0x1b
  private val Csi: Byte   = '['.toByte
  private val Ss3: Byte   = 'O'.toByte
  private val Tilde: Byte = '~'.toByte

  private val PasteStartParams              = "200"
  private val PasteStartMarker: Array[Byte] = Array(Esc, Csi, '2'.toByte, '0'.toByte, '0'.toByte, Tilde)
  private val PasteEndMarker: Array[Byte]   = Array(Esc, Csi, '2'.toByte, '0'.toByte, '1'.toByte, Tilde)

  def decode(bytes: Array[Byte]): DecodeResult =
    @annotation.tailrec
    def loop(i: Int, acc: List[DecodedToken]): DecodeResult =
      if i >= bytes.length then DecodeResult(acc.reverse, Array.emptyByteArray)
      else
        step(bytes, i) match
          case Step.Complete(emitted, next) => loop(next, emitted.reverse ::: acc)
          case Step.Incomplete              => DecodeResult(acc.reverse, bytes.slice(i, bytes.length))
    loop(0, Nil)

  /** Force-resolve whatever [[decode]] left as an incomplete `remainder`, for the two situations where no more bytes
    * are coming: the ESC-disambiguation deadline expired (remainder is a lone `ESC`, resolved to [[InputKey.Escape]]),
    * or the stream hit EOF with a genuinely truncated sequence (best-effort: an unterminated bracketed paste yields its
    * text so far, anything else is dropped rather than guessed at).
    */
  def decodeFinal(remainder: Array[Byte]): List[DecodedToken] =
    if remainder.isEmpty then Nil
    else if remainder.sameElements(Array(Esc)) then
      List(DecodedToken.Key(KeyStrokeInfo(InputKey.Escape, None, Set.empty)))
    else if remainder.length >= PasteStartMarker.length && remainder
          .take(PasteStartMarker.length)
          .sameElements(PasteStartMarker)
    then List(DecodedToken.Pasted(new String(remainder.drop(PasteStartMarker.length), StandardCharsets.UTF_8)))
    else Nil

  private enum Step:
    case Complete(tokens: List[DecodedToken], nextIndex: Int)
    case Incomplete

  private def step(bytes: Array[Byte], i: Int): Step =
    if bytes(i) == Esc then decodeEscape(bytes, i)
    else decodePlain(bytes, i)

  private def decodeEscape(bytes: Array[Byte], i: Int): Step =
    if i + 1 >= bytes.length then Step.Incomplete
    else
      bytes(i + 1) match
        case Csi => decodeCsi(bytes, i)
        case Ss3 =>
          if i + 2 >= bytes.length then Step.Incomplete
          else
            val key = bytes(i + 2) match
              case 'P' => InputKey.F1
              case 'Q' => InputKey.F2
              case 'R' => InputKey.F3
              case 'S' => InputKey.F4
              case _   => InputKey.Unknown
            Step.Complete(List(DecodedToken.Key(KeyStrokeInfo(key, None, Set.empty))), i + 3)
        case _ =>
          // ESC immediately followed by a non-CSI, non-SS3 byte: Alt held down for whatever that byte decodes to.
          decodePlain(bytes, i + 1) match
            case Step.Incomplete => Step.Incomplete
            case Step.Complete(emitted, nextIndex) =>
              Step.Complete(emitted.map(withAlt), nextIndex)

  private def withAlt(token: DecodedToken): DecodedToken = token match
    case DecodedToken.Key(info) => DecodedToken.Key(info.copy(modifiers = info.modifiers + Modifier.Alt))
    case other                  => other

  private def decodeCsi(bytes: Array[Byte], i: Int): Step =
    // Params: 0x30-0x3F (digits, ';', '<', '?', ...); intermediates: 0x20-0x2F; final: 0x40-0x7E.
    scanCsiEnd(bytes, i + 2) match
      case None => Step.Incomplete
      case Some(j) =>
        val finalByte = bytes(j)
        val params    = new String(bytes, i + 2, j - (i + 2), StandardCharsets.US_ASCII)
        if params == PasteStartParams && finalByte == Tilde then decodePaste(bytes, j + 1)
        else if params.headOption.contains('<') && (finalByte == 'M'.toByte || finalByte == 'm'.toByte) then
          decodeSgrMouse(params.tail, finalByte == 'm'.toByte, j + 1)
        else Step.Complete(List(decodeCsiKey(params, finalByte)), j + 1)

  @annotation.tailrec
  private def scanCsiEnd(bytes: Array[Byte], j: Int): Option[Int] =
    if j >= bytes.length then None
    else if isCsiParamOrIntermediate(bytes(j)) then scanCsiEnd(bytes, j + 1)
    else Some(j)

  private def isCsiParamOrIntermediate(b: Byte): Boolean =
    (b >= 0x20 && b <= 0x2f) || (b >= 0x30 && b <= 0x3f)

  private def decodePaste(bytes: Array[Byte], contentStart: Int): Step =
    val terminatorIndex = indexOfSlice(bytes, PasteEndMarker, contentStart)
    if terminatorIndex < 0 then Step.Incomplete
    else
      val text = new String(bytes, contentStart, terminatorIndex - contentStart, StandardCharsets.UTF_8)
      Step.Complete(List(DecodedToken.Pasted(text)), terminatorIndex + PasteEndMarker.length)

  @annotation.tailrec
  private def indexOfSlice(haystack: Array[Byte], needle: Array[Byte], from: Int): Int =
    if from + needle.length > haystack.length then -1
    else if haystack.slice(from, from + needle.length).sameElements(needle) then from
    else indexOfSlice(haystack, needle, from + 1)

  private def decodeSgrMouse(params: String, isRelease: Boolean, nextIndex: Int): Step =
    params.split(';').toList match
      case cbStr :: cxStr :: cyStr :: Nil =>
        (cbStr.toIntOption, cxStr.toIntOption, cyStr.toIntOption) match
          case (Some(cb), Some(cx), Some(cy)) =>
            Step.Complete(sgrMouseEvent(cb, cx, cy, isRelease).map(DecodedToken.Mouse.apply).toList, nextIndex)
          case _ => Step.Complete(Nil, nextIndex)
      case _ => Step.Complete(Nil, nextIndex)

  /** SGR mouse encoding (`ESC[<Cb;Cx;Cy M`/`m`, modes 1002/1003/1006): `Cb` bits 0-1 are the button (3 = "no button",
    * only seen during 1003 any-motion reporting), bit 2 is Shift, bit 5 is the motion flag (drag/move rather than
    * press/release), bit 6 marks a wheel event. `Cx`/`Cy` are already 1-based cell coordinates -- no pixel-to-cell
    * conversion needed, unlike the Swing path. Only `shiftDown` survives onto the resulting [[MouseInputEvent]]:
    * `SwingInputHandler` itself only ever reads `e.isShiftDown` for mouse events, never Ctrl/Alt/Meta, so there is
    * nowhere to put Cb's Ctrl/Meta bits even if we decoded them.
    */
  private def sgrMouseEvent(cb: Int, cx: Int, cy: Int, isRelease: Boolean): Option[MouseInputEvent] =
    val isWheel = (cb & 0x40) != 0
    if isWheel then None // No MouseWheel event exists in the Event model; out of scope for this issue.
    else
      val col      = cx - 1
      val row      = cy - 1
      val isMotion = (cb & 0x20) != 0
      val shift    = (cb & 0x04) != 0
      val button = (cb & 0x03) match
        case 0 => MouseButton.Primary
        case 1 => MouseButton.Middle
        case 2 => MouseButton.Secondary
        case _ => MouseButton.Other
      if isMotion then
        if (cb & 0x03) == 3 then Some(MouseMove(col, row, shiftDown = shift))
        else Some(MouseDrag(col, row, shiftDown = shift, button = button))
      else if isRelease then Some(MouseClick(col, row, clickCount = 1, shiftDown = shift, button = button))
      else Some(MousePress(col, row, shiftDown = shift, button = button))

  private def decodeCsiKey(params: String, finalByte: Byte): DecodedToken =
    val key = finalByte.toChar match
      case 'A' => InputKey.ArrowUp
      case 'B' => InputKey.ArrowDown
      case 'C' => InputKey.ArrowRight
      case 'D' => InputKey.ArrowLeft
      case 'H' => InputKey.Home
      case 'F' => InputKey.End
      case 'Z' => InputKey.ReverseTab
      case '~' =>
        params match
          case "1" | "7" => InputKey.Home
          case "4" | "8" => InputKey.End
          case "5"       => InputKey.PageUp
          case "6"       => InputKey.PageDown
          case "3"       => InputKey.Delete
          case "11"      => InputKey.F1
          case "12"      => InputKey.F2
          case "13"      => InputKey.F3
          case "14"      => InputKey.F4
          case "15"      => InputKey.F5
          case "17"      => InputKey.F6
          case "18"      => InputKey.F7
          case "19"      => InputKey.F8
          case "20"      => InputKey.F9
          case "21"      => InputKey.F10
          case "23"      => InputKey.F11
          case "24"      => InputKey.F12
          case _         => InputKey.Unknown
      case _ => InputKey.Unknown
    DecodedToken.Key(KeyStrokeInfo(key, None, Set.empty))

  private def decodePlain(bytes: Array[Byte], i: Int): Step =
    val b        = bytes(i)
    val unsigned = b & 0xff
    if unsigned == 0x09 then complete1(InputKey.Tab, i)
    else if unsigned == 0x0d || unsigned == 0x0a then complete1(InputKey.Enter, i)
    else if unsigned == 0x7f || unsigned == 0x08 then complete1(InputKey.Backspace, i)
    else if unsigned >= 0x01 && unsigned <= 0x1a then
      Step.Complete(
        List(
          DecodedToken.Key(KeyStrokeInfo(InputKey.Character, Some(('a' + (unsigned - 1)).toChar), Set(Modifier.Ctrl)))
        ),
        i + 1
      )
    else if unsigned < 0x20 then
      Step.Complete(Nil, i + 1) // Unrepresentable control byte (Ctrl+@, Ctrl+\, ...): dropped.
    else if unsigned < 0x80 then
      Step.Complete(List(DecodedToken.Key(KeyStrokeInfo(InputKey.Character, Some(unsigned.toChar), Set.empty))), i + 1)
    else decodeUtf8Char(bytes, i)

  private def complete1(key: InputKey, i: Int): Step =
    Step.Complete(List(DecodedToken.Key(KeyStrokeInfo(key, None, Set.empty))), i + 1)

  private def decodeUtf8Char(bytes: Array[Byte], i: Int): Step =
    val lead = bytes(i) & 0xff
    val length =
      if (lead & 0xe0) == 0xc0 then 2
      else if (lead & 0xf0) == 0xe0 then 3
      else if (lead & 0xf8) == 0xf0 then 4
      else 1 // Invalid lead byte: consume it alone rather than stalling the decoder forever.
    if i + length > bytes.length then Step.Incomplete
    else
      val decoded = new String(bytes, i, length, StandardCharsets.UTF_8)
      // A Char is UTF-16 code-unit-wide, same as AWT's KeyEvent#getKeyChar; codepoints outside the BMP (4-byte
      // UTF-8 sequences) can't be represented as one and are dropped rather than emitting a bogus surrogate half.
      if decoded.length == 1 then
        Step.Complete(
          List(DecodedToken.Key(KeyStrokeInfo(InputKey.Character, Some(decoded.charAt(0)), Set.empty))),
          i + length
        )
      else Step.Complete(Nil, i + length)
