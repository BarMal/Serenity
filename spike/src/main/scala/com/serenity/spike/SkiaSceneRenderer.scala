package com.serenity.spike

import org.jetbrains.skia.{
  Canvas,
  Font,
  FontEdging,
  FontHinting,
  FontMgr,
  FontStyle,
  Image,
  Paint,
  Picture,
  PictureRecorder,
  Point,
  Rect,
  SamplingMode,
  Surface,
  TextBlob,
  TextLine
}
import org.jetbrains.skia.shaper.Shaper

/** How the renderer shapes a row it has not seen: see `ShapingBench` for what each costs. */
enum Shaping:
  /** HarfBuzz via `Shaper.shape` with caller-supplied runs, kept as a `TextBlob` + caret array ([[RunShaped]]). */
  case Explicit

  /** One reused `Shaper.shapeLine` per row. */
  case Reused

  /** `TextLine.make` per row. */
  case TextLineMake

/** What a frame reuses for a paragraph's text beyond the shaped rows (which every variant caches by row text). */
enum TextCache:
  /** One `drawTextBlob` per visible row, from the shaped-row cache. The spike's original behaviour. */
  case Rows

  /** One `TextBlob` per paragraph holding every wrapped row's glyphs at their row offsets: one draw per paragraph. */
  case Blob

  /** One recorded `Picture` per paragraph (its rows' blobs), replayed at the paragraph's offset. */
  case Picture

  /** One raster `Image` per paragraph at device scale (the text pixels on transparent), blitted at the offset. */
  case Image

/** A paragraph's identity for the paragraph caches: its text and everything its wrapped glyphs depend on. The font is
  * fixed per renderer, so only its size is in the key.
  */
final case class ParagraphKey(text: String, wrapWidthPx: Int, fontSize: Float, scale: Float)

/** Paints an [[EditorState]] with Skia: gutter with line numbers, current-line band, a selection, wrapped prose and a
  * caret. The anchor row (the caret's row unless scrolled) sits on the viewport's vertical centre band. Shaped rows are
  * cached by text, the way a retained renderer would keep glyph runs, so a frame reshapes only rows whose text
  * changed; `textCache` adds a per-paragraph cache on top. Owned by one thread at a time: nothing here is synchronised.
  */
final class SkiaSceneRenderer(
    val scale: Float,
    shaping: Shaping = Shaping.Explicit,
    subpixelText: Boolean = true,
    val textCache: TextCache = TextCache.Rows
):
  import Geometry.*

  private val typeface   = FontMgr.Companion.getDefault().matchFamilyStyle("serif", FontStyle.Companion.getNORMAL())
  private val font       = textFont(FontSize)
  private val numberFont = textFont(11f)
  private val shaper     = Shaper.Companion.make()
  private val explicit   = Map(font -> ExplicitRunShaper(font), numberFont -> ExplicitRunShaper(numberFont))

  if textCache == TextCache.Blob && shaping != Shaping.Explicit then
    sys.error("--cache=blob needs --shaper=explicit: only explicit shaping keeps the glyph arrays a paragraph blob is built from")

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

  private var viewportWidth: Float  = LogicalWidth.toFloat
  private var viewportHeight: Float = LogicalHeight.toFloat
  private var anchorTop: Float      = centredAnchorTop

  /** The anchor row's top, snapped to a device pixel so cached paragraph images land pixel-for-pixel. */
  private def centredAnchorTop: Float =
    math.floor((viewportHeight / 2f - LineHeight / 2f) * scale).toFloat / scale

  /** The logical viewport this renderer fills: the layer's real size in a window, which a tiling compositor may make
    * far smaller than the 1500x1000 the spike asks for.
    */
  def resize(width: Float, height: Float): Unit =
    if width != viewportWidth || height != viewportHeight then
      viewportWidth = width
      viewportHeight = height
      anchorTop = centredAnchorTop

  def viewport: Rect = Rect.Companion.makeWH(viewportWidth, viewportHeight)

  /** How many whole rows the viewport shows. */
  def visibleRowCount: Int = (viewportHeight / LineHeight).toInt

  final private class Shaped(val line: ShapedText, var lastFrame: Long)
  private val shaped          = java.util.HashMap[String, Shaped]()
  private var frame           = 0L
  private var shapedThisFrame = 0
  private var builtThisFrame  = 0

  /** Rows shaped (cache misses) during the last `draw`. */
  def lastFrameShapedRows: Int = shapedThisFrame

  /** Paragraph cache entries built (cache misses) during the last `draw`. */
  def lastFrameBuiltParagraphs: Int = builtThisFrame

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

  /** A paragraph's cached text, drawn with its first row's top at `top` (logical px). */
  sealed private trait ParagraphText:
    def draw(canvas: Canvas, top: Float): Unit
    def close(): Unit

  private object NoText extends ParagraphText:
    def draw(canvas: Canvas, top: Float): Unit = ()
    def close(): Unit                          = ()

  final private class BlobParagraph(blob: TextBlob) extends ParagraphText:
    def draw(canvas: Canvas, top: Float): Unit =
      val _ = canvas.drawTextBlob(blob, TextLeft, top, text)
    def close(): Unit = blob.close()

  final private class PictureParagraph(picture: Picture) extends ParagraphText:
    def draw(canvas: Canvas, top: Float): Unit =
      val _ = SkiaSceneRenderer.drawPicture.invoke(canvas, picture, Array(1f, 0f, TextLeft, 0f, 1f, top, 0f, 0f, 1f), null)
    def close(): Unit = picture.close()

  final private class ImageParagraph(image: Image, logicalWidth: Float, logicalHeight: Float) extends ParagraphText:
    private val source = Rect.Companion.makeWH(image.getWidth.toFloat, image.getHeight.toFloat)
    def draw(canvas: Canvas, top: Float): Unit =
      val _ = canvas.drawImageRect(
        image,
        source,
        Rect.Companion.makeXYWH(TextLeft, top, logicalWidth, logicalHeight),
        SamplingMode.Companion.getDEFAULT(),
        null,
        true
      )
    def close(): Unit = image.close()

  final private class CachedParagraph(val text: ParagraphText, var lastFrame: Long)
  private val paragraphs = java.util.HashMap[ParagraphKey, CachedParagraph]()

  private def paragraph(lineText: String, rows: Vector[Row]): ParagraphText =
    val key = ParagraphKey(lineText, WrapWidthPx, FontSize, scale)
    val hit = paragraphs.get(key)
    if hit != null then
      hit.lastFrame = frame
      hit.text
    else
      builtThisFrame += 1
      val made = buildParagraph(rows)
      paragraphs.put(key, CachedParagraph(made, frame))
      made

  private def buildParagraph(rows: Vector[Row]): ParagraphText =
    val height = rows.length * LineHeight
    textCache match
      case TextCache.Rows => sys.error("rows are not cached per paragraph")
      case TextCache.Blob =>
        val glyphs    = scala.collection.mutable.ArrayBuilder.ofShort()
        val positions = scala.collection.mutable.ArrayBuilder.ofRef[Point]()
        rows.zipWithIndex.foreach { (row, index) =>
          shape(row.text, row.text, font) match
            case run: RunShaped =>
              glyphs.addAll(run.glyphs)
              run.xs.foreach(x => positions.addOne(Point(x, index * LineHeight + Baseline)))
            case _ => sys.error("blob paragraphs need explicit shaping")
        }
        Option(TextBlob.Companion.makeFromPos(glyphs.result(), positions.result(), font)).fold(NoText)(BlobParagraph(_))
      case TextCache.Picture =>
        val recorder = PictureRecorder()
        val canvas   = recorder.beginRecording(Rect.Companion.makeWH(TextAreaWidth, height), null)
        rows.zipWithIndex.foreach { (row, index) =>
          if row.text.nonEmpty then shape(row.text, row.text, font).draw(canvas, 0f, index * LineHeight + Baseline, text)
        }
        val picture = recorder.finishRecordingAsPicture()
        recorder.close()
        PictureParagraph(picture)
      case TextCache.Image =>
        val surface = Surface.Companion.makeRasterN32Premul(
          math.ceil(TextAreaWidth * scale).toInt,
          math.max(1, math.ceil(height * scale).toInt)
        )
        val canvas = surface.getCanvas
        canvas.clear(0x00000000)
        canvas.scale(scale, scale)
        rows.zipWithIndex.foreach { (row, index) =>
          if row.text.nonEmpty then shape(row.text, row.text, font).draw(canvas, 0f, index * LineHeight + Baseline, text)
        }
        val image = surface.makeImageSnapshot()
        surface.close()
        ImageParagraph(image, TextAreaWidth, height)

  def clearShapingCache(): Unit =
    shaped.values.forEach(_.line.close())
    shaped.clear()
    clearParagraphCache()

  def clearParagraphCache(): Unit =
    paragraphs.values.forEach(_.text.close())
    paragraphs.clear()

  private def sweep(): Unit =
    if shaped.size > 4000 then
      val _ = shaped.entrySet.removeIf(entry =>
        val old = entry.getValue.lastFrame < frame - 2
        if old then entry.getValue.line.close()
        old
      )
    if paragraphs.size > 400 then
      val _ = paragraphs.entrySet.removeIf(entry =>
        val old = entry.getValue.lastFrame < frame - 2
        if old then entry.getValue.text.close()
        old
      )

  private def rowTop(state: EditorState, globalRow: Int): Float =
    anchorTop + (globalRow - state.anchorRow) * LineHeight

  private def visibleRows(state: EditorState, clip: Rect): Range =
    val anchor = state.anchorRow
    val first  = anchor + math.floor((clip.getTop - anchorTop) / LineHeight).toInt
    val last   = anchor + math.ceil((clip.getBottom - anchorTop) / LineHeight).toInt
    math.max(0, first) until math.min(state.totalRows, last + 1)

  /** The logical-pixel region an edit on the caret line changed, or the whole viewport when the view moved. */
  def damage(before: EditorState, after: EditorState): Rect =
    if before.anchorRow != after.anchorRow then viewport
    else
      val line   = after.caretLine
      val top    = rowTop(after, after.rowStarts(line))
      val bottom =
        if before.rows(line).length != after.rows(line).length then viewportHeight
        else rowTop(after, after.rowStarts(line + 1))
      Rect.Companion.makeLTRB(0f, math.max(0f, top), viewportWidth, math.min(viewportHeight, bottom))

  /** The band a scroll from `before` to `after` exposes, or `None` when the whole viewport is new (or nothing moved). */
  def exposedByScroll(before: EditorState, after: EditorState): Option[Rect] =
    val shift = (after.anchorRow - before.anchorRow) * LineHeight
    if shift == 0f || math.abs(shift) >= viewportHeight then None
    else if shift > 0f then Some(Rect.Companion.makeLTRB(0f, viewportHeight - shift, viewportWidth, viewportHeight))
    else Some(Rect.Companion.makeLTRB(0f, 0f, viewportWidth, -shift))

  /** Draws `state` clipped to `clip` (logical pixels) onto a device-pixel canvas. */
  def draw(canvas: Canvas, state: EditorState, clip: Rect = viewport): Unit =
    frame += 1
    shapedThisFrame = 0
    builtThisFrame = 0
    canvas.save()
    canvas.scale(scale, scale)
    canvas.clipRect(clip)
    canvas.drawRect(clip, background)
    canvas.drawRect(0f, clip.getTop, GutterWidth, clip.getBottom, gutter)
    val rows = visibleRows(state, clip)
    drawCurrentLine(canvas, state)
    drawSelection(canvas, state, rows)
    textCache match
      case TextCache.Rows => rows.foreach(globalRow => drawRow(canvas, state, globalRow))
      case _              => drawParagraphs(canvas, state, rows)
    drawCaret(canvas, state)
    canvas.restore()
    sweep()

  /** The frame's rectangles only, no text: separates fill cost from glyph cost. */
  def drawFillsOnly(canvas: Canvas, state: EditorState): Unit =
    canvas.save()
    canvas.scale(scale, scale)
    canvas.drawRect(viewport, background)
    canvas.drawRect(0f, 0f, GutterWidth, viewportHeight, gutter)
    drawCurrentLine(canvas, state)
    canvas.restore()

  private def drawCurrentLine(canvas: Canvas, state: EditorState): Unit =
    val top    = rowTop(state, state.rowStarts(state.caretLine))
    val bottom = rowTop(state, state.rowStarts(state.caretLine + 1))
    canvas.drawRect(GutterWidth, top, viewportWidth, bottom, currentLine)

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
    if index == 0 then drawLineNumber(canvas, line, top)

  /** Every paragraph with a row in `rows`, each drawn whole (the clip trims rows outside the viewport). */
  private def drawParagraphs(canvas: Canvas, state: EditorState, rows: Range): Unit =
    if rows.nonEmpty then
      val (firstLine, _) = state.locate(rows.head)
      val (lastLine, _)  = state.locate(rows.last)
      (firstLine to lastLine).foreach { line =>
        val top = rowTop(state, state.rowStarts(line))
        if state.lines(line).nonEmpty then paragraph(state.lines(line), state.rows(line)).draw(canvas, top)
        if rows.contains(state.rowStarts(line)) then drawLineNumber(canvas, line, top)
      }

  private def drawLineNumber(canvas: Canvas, line: Int, top: Float): Unit =
    val label   = (line + 1).toString
    val numeral = shape("#" + label, label, numberFont)
    numeral.draw(canvas, GutterWidth - 10f - numeral.width, top + Baseline, number)

  private def drawCaret(canvas: Canvas, state: EditorState): Unit =
    val row = state.rows(state.caretLine)(state.caretRowInLine)
    val x   = TextLeft + shape(row.text, row.text, font).coord(state.caretColumn - row.start)
    val top = rowTop(state, state.caretGlobalRow)
    canvas.drawRect(x, top + 1f, x + 2f, top + LineHeight - 1f, caret)

object SkiaSceneRenderer:
  /** `Canvas.drawPicture(picture, matrix, paint)`, which keeps the picture as one nested op (a recording canvas
    * references it rather than copying its ops, unlike `Picture.playback`). `Matrix33` is a Kotlin value class, so
    * the JVM name is mangled to a name Scala cannot call directly.
    */
  val drawPicture: java.lang.reflect.Method =
    classOf[Canvas].getMethod("drawPicture-gqNt1-A", classOf[Picture], classOf[Array[Float]], classOf[Paint])
