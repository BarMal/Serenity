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

    /** A bare modifier key's own press or release, decoded from a kitty-protocol CSI-u sequence reporting one of its
      * private-use-area modifier codepoints (`57441`-`57452`; see [[BareModifierCodepoints]]). xterm's
      * `modifyOtherKeys`/`formatOtherKeys=1` has no equivalent -- it never emits these codepoints -- so this token only
      * ever arises on a terminal that answered the kitty `CSI ? u` capability query. Consumed by
      * [[TerminalInputHandler]] to drive [[com.serenity.input.ModifierTapDetector]], the same double-tap state machine
      * `SwingInputHandler` runs over AWT modifier press/release events.
      */
    final case class ModifierEdge(modifier: Modifier, pressed: Boolean) extends DecodedToken

    /** Terminal focus reporting (`CSI ?1004h`, #1171): `CSI I` on focus-in, `CSI O` on focus-out. `TerminalShell`
      * enables the mode on acquire and disables it on release; `TerminalInputHandler` routes this token to the
      * runtime's focus callback rather than the ordinary key/event queue, mirroring how the Swing window's
      * focus-lost/focus-gained listeners feed [[com.serenity.app.AppRuntime.onWindowFocusChanged]].
      */
    final case class FocusChanged(focused: Boolean) extends DecodedToken

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
  private val CsiU: Byte  = 'u'.toByte

  /** Terminal focus reporting (`CSI ?1004h`): the terminal sends a bare `CSI I` on focus-in and `CSI O` on focus-out --
    * no params, so gated on `params.isEmpty` in [[decodeCsi]] to avoid colliding with a param-carrying sequence that
    * happened to share a final byte (none do today, but the guard costs nothing and documents the intent).
    */
  private val FocusIn: Byte  = 'I'.toByte
  private val FocusOut: Byte = 'O'.toByte

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
    * are coming: the ESC-disambiguation deadline expired (remainder is a lone `ESC`, resolved to [[InputKey.Escape]];
    * or `ESC ESC`, Alt+Escape, resolved to [[InputKey.Escape]] with [[Modifier.Alt]]), or the stream hit EOF with a
    * genuinely truncated sequence (best-effort: an unterminated bracketed paste yields its text so far, anything else
    * is dropped rather than guessed at).
    */
  def decodeFinal(remainder: Array[Byte]): List[DecodedToken] =
    if remainder.isEmpty then Nil
    else if remainder.sameElements(Array(Esc)) then
      List(DecodedToken.Key(KeyStrokeInfo(InputKey.Escape, None, Set.empty)))
    else if remainder.sameElements(Array(Esc, Esc)) then
      List(DecodedToken.Key(KeyStrokeInfo(InputKey.Escape, None, Set(Modifier.Alt))))
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
        else if finalByte == CsiU then Step.Complete(decodeCsiU(params), j + 1)
        else if params.isEmpty && finalByte == FocusIn then Step.Complete(List(DecodedToken.FocusChanged(true)), j + 1)
        else if params.isEmpty && finalByte == FocusOut then
          Step.Complete(List(DecodedToken.FocusChanged(false)), j + 1)
        else Step.Complete(decodeCsiKey(params, finalByte), j + 1)

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

  /** Decodes the legacy CSI-letter (arrow keys, Home/End, Shift+Tab) and CSI-tilde (Home/End/PageUp/PageDown/Delete/
    * F-keys) key forms. Under kitty's "disambiguate escape codes" enhancement these keep their classic final byte but
    * gain the same `keycode;modifiers[:event]` shape `decodeCsiU` already handles for the CSI-u form -- e.g. `CSI
    * 1;1:3A` for an Up-arrow release, `CSI 5;1:3~` for a PageUp release (kitty keyboard-protocol spec,
    * `docs/keyboard-protocol.rst`: the "disambiguate escape codes" section gives `CSI 1;modifier [~ABCDEFHPQS]` as the
    * legacy-letter shape once that flag is on, and the "report event types" section documents the `:event` subfield as
    * appended to "the modifiers field" generally, with worked examples only for the `CSI u` form -- sw.kovidgoyal.net
    * is blocked by this environment's egress proxy, so this was confirmed against github.com/kovidgoyal/kitty's raw doc
    * source rather than the rendered page; applying the same modifier-field shape to the letter/tilde forms here is
    * this decoder's own inference from that shared "modifiers field" wording, not a verbatim-quoted example).
    *
    * A release (`eventType == 3`) is dropped entirely, mirroring `decodeCsiU`'s `case None if eventType == 3 => Nil` --
    * without this, a terminal that confirmed the kitty protocol reports a release for every plain arrow-key press too,
    * and each one decoded to a second, spurious navigation stroke identical to the press.
    *
    * The CSI-tilde branch matches only the leading `;`-delimited field against the key-number table, rather than
    * `params` whole: a modified tilde-form key (`CSI 5;2~` for Shift+PageUp, standard xterm behavior independent of
    * kitty) or a kitty-annotated one (`CSI 5;1:3~`) both carry a second field the exact-match against `params` used to
    * miss entirely, silently falling through to [[InputKey.Unknown]] -- a distinct, pre-existing gap from the
    * double-fire bug, fixed here since it is the same root cause (this decoder ignoring what follows the key number).
    *
    * The modifier value in that same field (`CSI 1;5C` for Ctrl+Right, `CSI 1;6C` for Ctrl+Shift+Right, standard xterm
    * `modifyCursorKeys`/`modifyOtherKeys` behavior, not kitty-specific) is decoded via [[modifiersOf]] and carried onto
    * the resulting [[KeyStrokeInfo]] -- previously dropped here entirely (`Set.empty` regardless of `params`), which
    * silently broke every Ctrl/Shift/Alt-modified arrow, Home/End and PageUp/PageDown stroke in the TUI (#1245).
    */
  private def decodeCsiKey(params: String, finalByte: Byte): List[DecodedToken] =
    if eventTypeOf(params) == 3 then Nil // Release of a plain key: not a keystroke of its own.
    else
      val modifiers = modifiersOf(params)
      val key = finalByte.toChar match
        case 'A' => InputKey.ArrowUp
        case 'B' => InputKey.ArrowDown
        case 'C' => InputKey.ArrowRight
        case 'D' => InputKey.ArrowLeft
        case 'H' => InputKey.Home
        case 'F' => InputKey.End
        case 'Z' => InputKey.ReverseTab
        case 'P' => InputKey.F1
        case 'Q' => InputKey.F2
        case 'R' => InputKey.F3
        case 'S' => InputKey.F4
        case '~' =>
          params.split(";", -1).headOption.getOrElse(params) match
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
      List(DecodedToken.Key(KeyStrokeInfo(key, None, modifiers)))

  /** Shared `keycode;modifiers[:eventType]` event-type extraction, used by both [[decodeCsiU]] and [[decodeCsiKey]] --
    * same wire position (the `;`-delimited field after the leading keycode/dummy-`1`, `:`-delimited sub-field after the
    * modifier value), same meaning, same default (`1`, press) when the sub-field or the whole field is absent -- e.g. a
    * bare legacy `CSI A` (`params` empty) or an un-annotated `CSI 1;5A` (Ctrl+Up, no kitty event-type reporting) both
    * correctly default to a press.
    */
  private def eventTypeOf(params: String): Int =
    val fields    = params.split(";", -1)
    val modFields = fields.drop(1).headOption.getOrElse("").split(":", -1)
    modFields.drop(1).headOption.flatMap(_.toIntOption).getOrElse(1)

  /** Shared modifier-value extraction from the same `...;modifiers[:event]` field [[eventTypeOf]] reads the event type
    * out of, decoded via [[csiUModifiers]] -- used by both [[decodeCsiU]] (whose keycode field precedes it) and
    * [[decodeCsiKey]] (whose legacy letter/tilde form carries the identical field once a terminal reports modified
    * cursor/tilde keys, kitty-annotated or plain xterm `modifyCursorKeys` alike).
    */
  private def modifiersOf(params: String): Set[Modifier] =
    val fields    = params.split(";", -1)
    val modFields = fields.drop(1).headOption.getOrElse("").split(":", -1)
    val modsValue = modFields.headOption.flatMap(_.toIntOption).filter(_ > 0).getOrElse(1)
    csiUModifiers(modsValue)

  /** kitty-protocol private-use-area codepoints for a bare modifier key's own press/release, keyed by the [[Modifier]]
    * it double-taps as. `Super`/`Hyper`/`CapsLock`/`NumLock` have codepoints too (per the kitty spec) but no
    * corresponding [[Modifier]] case in this codebase, so they are deliberately left unmapped and fall through to
    * [[decodeCsiU]]'s ordinary-key path, where their non-printable codepoint drops them.
    */
  private val BareModifierCodepoints: Map[Int, Modifier] = Map(
    57441 -> Modifier.Shift,
    57447 -> Modifier.Shift,
    57442 -> Modifier.Ctrl,
    57448 -> Modifier.Ctrl,
    57443 -> Modifier.Alt,
    57449 -> Modifier.Alt,
    57446 -> Modifier.Meta,
    57452 -> Modifier.Meta
  )

  /** Decodes a CSI-u (fixterms) sequence: `keycode[:shifted[:base]] ; modifiers[:event][;text] u`. Shared wire shape
    * for both the kitty keyboard protocol (full form, including bare-modifier and release events) and xterm's
    * `modifyOtherKeys` mode 2 with `formatOtherKeys=1` (always a bare `keycode ; modifiers u`, event type always
    * implicitly "press") -- one decoding path serves both tiers, since a modifyOtherKeys-shaped sequence is simply a
    * kitty-shaped one with the optional subfields omitted.
    */
  private def decodeCsiU(params: String): List[DecodedToken] =
    val fields    = params.split(";", -1)
    val keycode   = fields.headOption.flatMap(_.split(":", -1).headOption).flatMap(_.toIntOption)
    val eventType = eventTypeOf(params)
    val modifiers = modifiersOf(params)

    keycode match
      case None => Nil
      case Some(code) =>
        BareModifierCodepoints.get(code) match
          case Some(modifier)         => List(DecodedToken.ModifierEdge(modifier, pressed = eventType != 3))
          case None if eventType == 3 => Nil // Release of an ordinary key: not a keystroke of its own.
          case None =>
            namedCsiUKey(code, modifiers)
              .orElse(csiUChar(code).map(ch => (InputKey.Character, Some(ch), modifiers)))
              .map { case (key, ch, mods) => DecodedToken.Key(KeyStrokeInfo(key, ch, mods)) }
              .toList

  /** modifier value is `1 + bitmask`; `super`(0b1000)/`hyper`(0b10000)/`caps_lock`(0b1000000)/`num_lock`(0b10000000)
    * have no [[Modifier]] case in this codebase and are dropped, same as [[BareModifierCodepoints]].
    */
  private def csiUModifiers(modsValue: Int): Set[Modifier] =
    val bits = modsValue - 1
    Set(
      Option.when((bits & 0x01) != 0)(Modifier.Shift),
      Option.when((bits & 0x02) != 0)(Modifier.Alt),
      Option.when((bits & 0x04) != 0)(Modifier.Ctrl),
      Option.when((bits & 0x20) != 0)(Modifier.Meta)
    ).flatten

  /** The four keys CSI-u's "disambiguate escape codes" flag exists to promote out of ambiguous C0 control bytes --
    * matching the same collisions [[decodePlain]] documents as unresolvable in the legacy path (Ctrl+I/Tab,
    * Ctrl+M/Enter), only now with a modifier that survives. Shift+Tab collapses into [[InputKey.ReverseTab]] with Shift
    * stripped from its modifiers, mirroring `SwingInputHandler`'s `tabMods` handling of `VK_TAB`.
    */
  private def namedCsiUKey(code: Int, modifiers: Set[Modifier]): Option[(InputKey, Option[Char], Set[Modifier])] =
    code match
      case 13                                      => Some((InputKey.Enter, None, modifiers))
      case 9 if modifiers.contains(Modifier.Shift) => Some((InputKey.ReverseTab, None, modifiers - Modifier.Shift))
      case 9                                       => Some((InputKey.Tab, None, modifiers))
      case 127                                     => Some((InputKey.Backspace, None, modifiers))
      case 27                                      => Some((InputKey.Escape, None, modifiers))
      case _                                       => None

  /** A codepoint outside the BMP can't be represented as one UTF-16 `Char` (same constraint [[decodeUtf8Char]]
    * documents for the legacy path); other non-printable codepoints (C0/C1 controls not covered by [[namedCsiUKey]])
    * have no keystroke to report either.
    */
  private def csiUChar(code: Int): Option[Char] =
    Option.when(code >= 0x20 && code < 0x110000 && Character.charCount(code) == 1)(code.toChar)

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
    else if unsigned >= 0x1c && unsigned <= 0x1f then
      // FS/GS/RS/US: Ctrl+\, Ctrl+], Ctrl+^, Ctrl+_ -- the ASCII control-code convention of control byte + 0x40.
      Step.Complete(
        List(DecodedToken.Key(KeyStrokeInfo(InputKey.Character, Some((unsigned + 0x40).toChar), Set(Modifier.Ctrl)))),
        i + 1
      )
    else if unsigned == 0x1b then
      // Only reachable via decodeEscape's Alt-prefix branch recursing here on a second ESC (Alt+Escape); a bare ESC
      // never reaches decodePlain at the top level, since step() routes it to decodeEscape first.
      if i + 1 >= bytes.length then Step.Incomplete // lone ESC awaiting disambiguation, same as the top-level case
      else complete1(InputKey.Escape, i)
    else if unsigned < 0x20 then Step.Complete(Nil, i + 1) // Unrepresentable control byte (Ctrl+@): dropped.
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
