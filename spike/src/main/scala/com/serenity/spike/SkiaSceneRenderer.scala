package com.serenity.spike

import org.jetbrains.skia.{Canvas, Font, FontEdging, FontHinting, FontMgr, FontStyle, Paint, Rect, TextLine}
import org.jetbrains.skia.shaper.Shaper

/** How the renderer shapes a row it has not seen: see `ShapingBench` for what each costs. */
enum Shaping:
  /** HarfBuzz via `Shaper.shape` with caller-supplied runs, kept as a `TextBlob` + caret array ([[RunShaped]]). */
  case Explicit

  /** One reused `Shaper.shapeLine` per row. */
  case Reused

  /** `TextLine.make` per row. */
  case TextLineMake

/** Paints an [[EditorState]] with Skia: gutter with line numbers, current-line band, a selection, wrapped prose and a
  * caret on a vertically centred row. Shaped rows are cached by text, the way a retained renderer would keep glyph runs,
  * so a frame reshapes only rows whose text changed. Owned by one thread at a time: the cache is not synchronised.
  */
final class SkiaSceneRenderer(val scale: Float, shaping: Shaping = Shaping.Explicit, subpixelText: Boolean = true):
  import Geometry.*

  private val typeface   = FontMgr.Companion.getDefault().matchFamilyStyle("serif", FontStyle.Companion.getNORMAL())
  private val font       = textFont(FontSize)
  private val numberFont = textFont(11f)
  private val shaper     = Shaper.Companion.make()
  private val explicit   = Map(font -> ExplicitRunShaper(font), numberFont -> ExplicitRunShaper(numberFont))

  private def textFont(size: Float): Font =
    val created = Font(typeface, size)
    created.setSubpixel(subpixelText)
    created.setEdging(FontEdging.ANTI_ALIAS)
    created.setHinting(FontHinting.SLIGHT)
    created

  def typefaceName: String = Option(typeface).map(_.getFamilyName).getOrElse("<none>")

  private def paint(argb: Int): Paint =
    val created = Paint()
    created.setColor(argb)
    created.setAntiAlias(true)
    created

  private val background  = paint(0xff1e1f24)
  private val gutter      = paint(0xff17181c)
  private val currentLine = paint(0xff26282f)
  private val selection   = paint(0xff2f4a6e)
  private val text        = paint(0xffd8d6cf)
  private val number      = paint(0xff6b6f7a)
  private val caret       = paint(0xffe8b04a)

  final private class Shaped(val line: ShapedText, var lastFrame: Long)
  private val shaped = java.util.HashMap[String, Shaped]()
  private var frame  = 0L
  private var shapedThisFrame = 0

  /** Rows shaped (cache misses) during the last `draw`. */
  def lastFrameShapedRows: Int = shapedThisFrame

  private def shape(key: String, value: String, withFont: Font): ShapedText =
    val hit = shaped.get(key)
    if hit != null then
      hit.lastFrame = frame
      hit.line
    else
      shapedThisFrame += 1
      val made = shaping match
        case Shaping.Explicit     => explicit(withFont).shape(value)
        case Shaping.Reused       => LineShaped(shaper.shapeLine(value, withFont))
        case Shaping.TextLineMake => LineShaped(TextLine.Companion.make(value, withFont))
      shaped.put(key, Shaped(made, frame))
      made

  def clearShapingCache(): Unit =
    shaped.values.forEach(_.line.close())
    shaped.clear()

  private def sweep(): Unit =
    if shaped.size > 4000 then
      val _ = shaped.entrySet.removeIf(entry =>
        val old = entry.getValue.lastFrame < frame - 2
        if old then entry.getValue.line.close()
        old
      )

  private val caretRowTop: Float = LogicalHeight / 2f - LineHeight / 2f

  private def rowTop(state: EditorState, globalRow: Int): Float =
    caretRowTop + (globalRow - state.caretGlobalRow) * LineHeight

  private def visibleRows(state: EditorState, clip: Rect): Range =
    val caretRow = state.caretGlobalRow
    val first    = caretRow + math.floor((clip.getTop - caretRowTop) / LineHeight).toInt
    val last     = caretRow + math.ceil((clip.getBottom - caretRowTop) / LineHeight).toInt
    math.max(0, first) until math.min(state.totalRows, last + 1)

  val fullViewport: Rect = Rect.Companion.makeWH(LogicalWidth.toFloat, LogicalHeight.toFloat)

  /** The logical-pixel region an edit on the caret line changed, or the whole viewport when the view scrolled. */
  def damage(before: EditorState, after: EditorState): Rect =
    if before.caretGlobalRow != after.caretGlobalRow then fullViewport
    else
      val line   = after.caretLine
      val top    = rowTop(after, after.rowStarts(line))
      val bottom =
        if before.rows(line).length != after.rows(line).length then LogicalHeight.toFloat
        else rowTop(after, after.rowStarts(line + 1))
      Rect.Companion.makeLTRB(0f, math.max(0f, top), LogicalWidth.toFloat, math.min(LogicalHeight.toFloat, bottom))

  /** Draws `state` clipped to `clip` (logical pixels) onto a device-pixel canvas. */
  def draw(canvas: Canvas, state: EditorState, clip: Rect = fullViewport): Unit =
    frame += 1
    shapedThisFrame = 0
    canvas.save()
    canvas.scale(scale, scale)
    canvas.clipRect(clip)
    canvas.drawRect(clip, background)
    canvas.drawRect(0f, 0f, GutterWidth, LogicalHeight.toFloat, gutter)
    val rows = visibleRows(state, clip)
    drawCurrentLine(canvas, state)
    drawSelection(canvas, state, rows)
    rows.foreach(globalRow => drawRow(canvas, state, globalRow))
    drawCaret(canvas, state)
    canvas.restore()
    sweep()

  /** The frame's rectangles only, no text: separates fill cost from glyph cost. */
  def drawFillsOnly(canvas: Canvas, state: EditorState): Unit =
    canvas.save()
    canvas.scale(scale, scale)
    canvas.drawRect(fullViewport, background)
    canvas.drawRect(0f, 0f, GutterWidth, LogicalHeight.toFloat, gutter)
    drawCurrentLine(canvas, state)
    canvas.restore()

  private def drawCurrentLine(canvas: Canvas, state: EditorState): Unit =
    val top    = rowTop(state, state.rowStarts(state.caretLine))
    val bottom = rowTop(state, state.rowStarts(state.caretLine + 1))
    canvas.drawRect(GutterWidth, top, LogicalWidth.toFloat, bottom, currentLine)

  /** A fixed selection two lines above the caret, painted per row from Skia's own caret coordinates. */
  private def drawSelection(canvas: Canvas, state: EditorState, rows: Range): Unit =
    val line = state.caretLine - 2
    if line >= 0 && state.lines(line).nonEmpty then
      val from  = math.min(10, state.lines(line).length)
      val until = math.min(260, state.lines(line).length)
      state.rows(line).zipWithIndex.foreach { (row, index) =>
        val globalRow = state.rowStarts(line) + index
        val start     = math.max(from, row.start)
        val end       = math.min(until, row.end)
        if rows.contains(globalRow) && start < end then
          val shapedRow = shape(row.text, row.text, font)
          val top       = rowTop(state, globalRow)
          canvas.drawRect(
            TextLeft + shapedRow.coord(start - row.start),
            top,
            TextLeft + shapedRow.coord(end - row.start),
            top + LineHeight,
            selection
          )
      }

  private def drawRow(canvas: Canvas, state: EditorState, globalRow: Int): Unit =
    val (line, index) = state.locate(globalRow)
    val row           = state.rows(line)(index)
    val top           = rowTop(state, globalRow)
    if row.text.nonEmpty then shape(row.text, row.text, font).draw(canvas, TextLeft, top + Baseline, text)
    if index == 0 then
      val label   = (line + 1).toString
      val numeral = shape("#" + label, label, numberFont)
      numeral.draw(canvas, GutterWidth - 10f - numeral.width, top + Baseline, number)

  private def drawCaret(canvas: Canvas, state: EditorState): Unit =
    val row  = state.rows(state.caretLine)(state.caretRowInLine)
    val x    = TextLeft + shape(row.text, row.text, font).coord(state.caretColumn - row.start)
    val top  = caretRowTop
    canvas.drawRect(x, top + 1f, x + 2f, top + LineHeight - 1f, caret)
