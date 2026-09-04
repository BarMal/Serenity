package com.serenity.ui.tui

import java.awt.Color
import java.nio.charset.StandardCharsets
import java.util.Base64

import scala.annotation.tailrec

import com.serenity.ui.layout.CharWidth
import com.serenity.ui.theme.TextStyle

/** A mock terminal, test-only: it interprets the escape sequences Serenity's TUI actually writes -- cursor positioning,
  * erase, 24-bit SGR runs, DECTCEM/DECSCUSR, DEC private modes, OSC 52 clipboard writes -- and holds the result as a
  * cell grid plus the terminal-side state a real terminal would hold. This is what makes a TUI behaviour assertion a
  * statement about the screen ("row 4 reads `  1 hello`, the caret sits on cell (9, 4)") rather than a substring search
  * over raw bytes, which passes just as happily when content lands in the wrong place.
  *
  * Immutable: [[consume]] returns a new emulator, so a spec can hold a frame from before an interaction and compare it
  * against the frame after. Anything it does not recognise (the keyboard-protocol negotiation sequences, an unknown
  * private mode) is absorbed rather than printed, exactly as a terminal ignoring an unsupported control would.
  *
  * @param pending
  *   an escape sequence split across two [[consume]] calls, held until the rest of it arrives -- so consuming a byte
  *   stream in arbitrary chunks gives the same result as consuming it whole.
  */
final case class TerminalEmulator(
    frame: TerminalFrame,
    cursor: TerminalEmulator.Cursor,
    pen: TerminalEmulator.Pen,
    privateModes: Set[Int],
    osc52Payloads: Vector[String],
    pending: String
):
  import TerminalEmulator.*

  def width: Int  = frame.width
  def height: Int = frame.height

  def consume(ansi: String): TerminalEmulator = interpret(this, pending + ansi)

  def cellAt(col: Int, row: Int): TerminalCell = frame(col, row)

  /** The row's printable text. A wide glyph contributes its own character and nothing for the continuation cell it
    * reserves, so the string reads the way the row looks rather than padding to the column count.
    */
  def rowText(row: Int): String = (0 until width).map(col => frame(col, row).text).mkString

  def rows: Vector[String] = (0 until height).map(rowText).toVector

  /** The first `(column, row)` at which `text` appears, in text offsets within the row -- which is the cell column too
    * for any row without a wide glyph to its left.
    */
  def find(text: String): Option[(Int, Int)] =
    rows.zipWithIndex.collectFirst { case (line, row) if line.contains(text) => (line.indexOf(text), row) }

  def rowsContaining(text: String): Vector[Int] =
    rows.zipWithIndex.collect { case (line, row) if line.contains(text) => row }

  def inAlternateScreen: Boolean     = privateModes.contains(AlternateScreenMode)
  def mouseTrackingEnabled: Boolean  = MouseModes.exists(privateModes.contains)
  def bracketedPasteEnabled: Boolean = privateModes.contains(BracketedPasteMode)
  def focusReportingEnabled: Boolean = privateModes.contains(FocusReportingMode)
  def synchronizedUpdate: Boolean    = privateModes.contains(SynchronizedUpdateMode)

  /** The whole grid as text, framed and row-numbered, with the caret's state spelled out underneath. Attached to every
    * failing screen assertion so a red test shows the terminal rather than a string mismatch.
    */
  def render: String =
    val border = "     +" + "-" * width + "+"
    val body   = rows.zipWithIndex.map((line, row) => f"$row%4d |$line%s".padTo(6 + width, ' ') + "|")
    val caretLine =
      s"caret row=${cursor.row} col=${cursor.col} visible=${cursor.visible} " +
        s"shape=${cursor.shape.map(_.toString).getOrElse("none")}"
    (border +: body :+ border :+ caretLine).mkString("\n")

object TerminalEmulator:

  /** The terminal's own caret: where it sits, whether DECTCEM has it shown, and the DECSCUSR shape parameter last
    * requested (`None` until the surface asks for one).
    */
  final case class Cursor(col: Int, row: Int, visible: Boolean, shape: Option[Int])

  /** The colours and style subsequent glyphs are drawn with -- the running SGR state. */
  final case class Pen(fg: Color, bg: Color, style: TextStyle)

  /** SGR 49 resets to the terminal's own background; `TerminalAnsiDiff` emits it for an alpha-0 background, so it reads
    * back as that same sentinel and an emit/replay round trip reproduces a transparent cell.
    */
  val TransparentBackground: Color = new Color(0, 0, 0, 0)

  private val AlternateScreenMode    = 1049
  private val CursorVisibilityMode   = 25
  private val BracketedPasteMode     = 2004
  private val FocusReportingMode     = 1004
  private val SynchronizedUpdateMode = 2026
  private val MouseModes             = Set(1000, 1002, 1003, 1006)

  private val Esc = 0x1b.toChar
  private val Bel = 0x07.toChar

  def blank(width: Int, height: Int, fg: Color = Color.WHITE, bg: Color = Color.BLACK): TerminalEmulator =
    fromFrame(TerminalFrame.blank(width, height, fg, bg), fg, bg)

  def fromFrame(frame: TerminalFrame, fg: Color = Color.WHITE, bg: Color = Color.BLACK): TerminalEmulator =
    TerminalEmulator(
      frame = frame,
      cursor = Cursor(col = 0, row = 0, visible = true, shape = None),
      pen = Pen(fg, bg, TextStyle.normal),
      privateModes = Set.empty,
      osc52Payloads = Vector.empty,
      pending = ""
    )

  /** One control sequence, split into the fields ECMA-48 defines: an optional private-parameter prefix (`?`, `>`, `<`,
    * `=`), the parameter and intermediate byte runs, and the final byte that selects the function.
    */
  final private case class Csi(
      prefix: Option[Char],
      params: String,
      intermediates: String,
      finalByte: Char,
      next: Int
  )

  final private case class Interp(cursor: Cursor, pen: Pen, modes: Set[Int], payloads: Vector[String])

  private def interpret(start: TerminalEmulator, ansi: String): TerminalEmulator =
    val width  = start.frame.width
    val height = start.frame.height
    val grid   = Array.tabulate(height, width)((row, col) => start.frame(col, row))

    def writeCell(col: Int, row: Int, cell: TerminalCell): Unit =
      if col >= 0 && col < width && row >= 0 && row < height then grid(row)(col) = cell

    def clearRange(fromIndex: Int, untilIndex: Int, pen: Pen): Unit =
      (math.max(0, fromIndex) until math.min(width * height, untilIndex))
        .foreach(index => writeCell(index % width, index / width, TerminalCell.blank(pen.fg, pen.bg)))

    def printCodePoint(state: Interp, codePoint: Int): Interp =
      val glyphWidth = CharWidth.of(codePoint)
      val start =
        if state.cursor.col + glyphWidth > width then state.cursor.copy(col = 0, row = state.cursor.row + 1)
        else state.cursor
      val row  = math.min(start.row, height - 1)
      val span = if glyphWidth == 2 then CellSpan.Wide else CellSpan.Narrow
      writeCell(start.col, row, TerminalCell(codePoint, state.pen.fg, state.pen.bg, state.pen.style, span))
      if glyphWidth == 2 then
        writeCell(start.col + 1, row, TerminalCell.continuation(state.pen.fg, state.pen.bg, state.pen.style))
      state.copy(cursor = start.copy(col = start.col + glyphWidth, row = row))

    def applyErase(state: Interp, csi: Csi): Interp =
      val cursorIndex = state.cursor.row * width + math.min(state.cursor.col, width - 1)
      val rowStart    = state.cursor.row * width
      val mode        = firstParam(csi.params, default = 0)
      csi.finalByte match
        case 'J' =>
          mode match
            case 0 => clearRange(cursorIndex, width * height, state.pen)
            case 1 => clearRange(0, cursorIndex + 1, state.pen)
            case _ => clearRange(0, width * height, state.pen)
        case _ =>
          mode match
            case 0 => clearRange(cursorIndex, rowStart + width, state.pen)
            case 1 => clearRange(rowStart, cursorIndex + 1, state.pen)
            case _ => clearRange(rowStart, rowStart + width, state.pen)
      state

    def applyCsi(state: Interp, csi: Csi): Interp =
      (csi.prefix, csi.intermediates, csi.finalByte) match
        case (None, "", 'H') | (None, "", 'f') =>
          state.copy(cursor = cupTarget(state.cursor, csi.params, width, height))
        case (None, "", 'J') | (None, "", 'K') => applyErase(state, csi)
        case (None, "", 'm')                   => state.copy(pen = applySgr(state.pen, csi.params))
        case (None, " ", 'q')                  => state.copy(cursor = state.cursor.copy(shape = csi.params.toIntOption))
        case (Some('?'), "", 'h')              => applyPrivateModes(state, csi.params, set = true)
        case (Some('?'), "", 'l')              => applyPrivateModes(state, csi.params, set = false)
        case _                                 => state

    @tailrec def loop(index: Int, state: Interp): (Int, Interp) =
      if index >= ansi.length then (index, state)
      else
        ansi(index) match
          case Esc if index + 1 < ansi.length && ansi(index + 1) == '[' =>
            scanCsi(ansi, index + 2) match
              case Some(csi) => loop(csi.next, applyCsi(state, csi))
              case None      => (index, state)
          case Esc if index + 1 < ansi.length && ansi(index + 1) == ']' =>
            scanOsc(ansi, index + 2) match
              case Some((body, next)) => loop(next, state.copy(payloads = state.payloads ++ osc52Payload(body)))
              case None               => (index, state)
          case Esc if index + 1 < ansi.length => loop(index + 2, state)
          case Esc                            => (index, state)
          case '\r'                           => loop(index + 1, state.copy(cursor = state.cursor.copy(col = 0)))
          case '\n' =>
            loop(index + 1, state.copy(cursor = state.cursor.copy(row = math.min(state.cursor.row + 1, height - 1))))
          case '\b' =>
            loop(index + 1, state.copy(cursor = state.cursor.copy(col = math.max(0, state.cursor.col - 1))))
          case '\t' =>
            val stop = math.min(width - 1, (state.cursor.col / 8 + 1) * 8)
            loop(index + 1, state.copy(cursor = state.cursor.copy(col = stop)))
          case char if char.isControl => loop(index + 1, state)
          case _ =>
            val codePoint = ansi.codePointAt(index)
            loop(index + Character.charCount(codePoint), printCodePoint(state, codePoint))

    val (consumedTo, finalState) =
      loop(0, Interp(start.cursor, start.pen, start.privateModes, start.osc52Payloads))

    TerminalEmulator(
      frame = TerminalFrame(width, height, grid.map(_.toVector).toVector),
      cursor = finalState.cursor,
      pen = finalState.pen,
      privateModes = finalState.modes,
      osc52Payloads = finalState.payloads,
      pending = ansi.substring(consumedTo)
    )

  private def cupTarget(cursor: Cursor, params: String, width: Int, height: Int): Cursor =
    val fields = params.split(";", -1).toVector
    val row    = fields.headOption.flatMap(_.toIntOption).getOrElse(1) - 1
    val col    = fields.drop(1).headOption.flatMap(_.toIntOption).getOrElse(1) - 1
    cursor.copy(col = math.max(0, math.min(col, width - 1)), row = math.max(0, math.min(row, height - 1)))

  private def firstParam(params: String, default: Int): Int =
    params.split(";", -1).headOption.flatMap(_.toIntOption).getOrElse(default)

  private def applyPrivateModes(state: Interp, params: String, set: Boolean): Interp =
    params.split(";", -1).flatMap(_.toIntOption).foldLeft(state) { (current, mode) =>
      if mode == CursorVisibilityMode then current.copy(cursor = current.cursor.copy(visible = set))
      else current.copy(modes = if set then current.modes + mode else current.modes - mode)
    }

  private def applySgr(pen: Pen, params: String): Pen =
    val fields = if params.isEmpty then Array("0") else params.split(";", -1)

    @tailrec def loop(index: Int, current: Pen): Pen =
      if index >= fields.length then current
      else
        fields(index) match
          case "0" | "" => loop(index + 1, current.copy(style = TextStyle.normal))
          case "1"      => loop(index + 1, current.copy(style = current.style.copy(isBold = true)))
          case "3"      => loop(index + 1, current.copy(style = current.style.copy(isItalic = true)))
          case "4"      => loop(index + 1, current.copy(style = current.style.copy(isUnderlined = true)))
          case "38"     => loop(index + 5, current.copy(fg = truecolor(fields, index)))
          case "48"     => loop(index + 5, current.copy(bg = truecolor(fields, index)))
          case "49"     => loop(index + 1, current.copy(bg = TransparentBackground))
          case _        => loop(index + 1, current)

    loop(0, pen)

  private def truecolor(fields: Array[String], index: Int): Color =
    val channels = (2 to 4).map(offset => fields.lift(index + offset).flatMap(_.toIntOption).getOrElse(0))
    new Color(channels(0), channels(1), channels(2))

  private def isParamByte(char: Char): Boolean        = char >= 0x30 && char <= 0x3f
  private def isIntermediateByte(char: Char): Boolean = char >= 0x20 && char <= 0x2f
  private def isFinalByte(char: Char): Boolean        = char >= 0x40 && char <= 0x7e

  private def scanCsi(ansi: String, from: Int): Option[Csi] =
    val prefix     = Option.when(from < ansi.length && "?<>=".contains(ansi(from)))(ansi(from))
    val paramStart = from + prefix.size
    val paramEnd   = scanWhile(ansi, paramStart, isParamByte)
    val interEnd   = scanWhile(ansi, paramEnd, isIntermediateByte)
    Option.when(interEnd < ansi.length && isFinalByte(ansi(interEnd)))(
      Csi(
        prefix = prefix,
        params = ansi.substring(paramStart, paramEnd),
        intermediates = ansi.substring(paramEnd, interEnd),
        finalByte = ansi(interEnd),
        next = interEnd + 1
      )
    )

  @tailrec private def scanWhile(ansi: String, from: Int, predicate: Char => Boolean): Int =
    if from < ansi.length && predicate(ansi(from)) then scanWhile(ansi, from + 1, predicate) else from

  /** An OSC body and the index past its terminator -- BEL or ST (`ESC \`), both of which real terminals accept. */
  private def scanOsc(ansi: String, from: Int): Option[(String, Int)] =
    val bel = ansi.indexOf(Bel.toInt, from)
    val st  = ansi.indexOf(s"$Esc\\", from)
    (bel, st) match
      case (-1, -1)        => None
      case (b, -1)         => Some((ansi.substring(from, b), b + 1))
      case (-1, s)         => Some((ansi.substring(from, s), s + 2))
      case (b, s) if b < s => Some((ansi.substring(from, b), b + 1))
      case (_, s)          => Some((ansi.substring(from, s), s + 2))

  /** The decoded text of an `OSC 52 ; <selection> ; <base64>` clipboard write, or nothing for any other OSC. */
  private def osc52Payload(body: String): Option[String] =
    body.split(";", 3).toVector match
      case Vector("52", _, encoded) =>
        scala.util.Try(new String(Base64.getDecoder.decode(encoded), StandardCharsets.UTF_8)).toOption
      case _ => None

end TerminalEmulator
