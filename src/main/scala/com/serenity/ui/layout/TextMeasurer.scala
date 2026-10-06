package com.serenity.ui.layout

import com.serenity.ui.renderer.FontSpec

/** The text measurement wrapping, caret placement and viewport centring take from a rendering backend. A backend must
  * lay text out with the measurer it paints with, or carets and wrap points drift from the drawn glyphs.
  *
  * Columns are absolute buffer columns: `startColumn` is the column of `text(0)`, and [[TextMeasurer.LineFonts]] and
  * [[TextMeasurer.FontRun]] are indexed the same way.
  */
trait TextMeasurer:

  /** Equal for two measurers only when they measure every input identically, so a cache keyed on it never serves one
    * measurer's results to another.
    */
  def metricsKey: TextMeasurer.MetricsKey

  /** Whether text in `font` is laid out from measured glyphs rather than on its uniform cell grid. */
  def measuresProportionally(font: FontSpec): Boolean

  def cellMetrics(font: FontSpec): CellMetrics

  /** `text[from, until)` cut into maximal runs of the one font each column is measured and painted in. */
  def itemise(
    text: String,
    from: Int,
    until: Int,
    startColumn: Int,
    fonts: TextMeasurer.LineFonts
  ): Vector[TextMeasurer.FontRun]

  /** Height and ascent of a visual line covering `[startColumn, endColumn)`, from the tallest font over it. */
  def lineMetrics(fonts: TextMeasurer.LineFonts, startColumn: Int, endColumn: Int): TextMeasurer.LineMetrics

  /** Each character's leading caret plus the trailing edge, before collapsed carets are spread apart. */
  def rawCaretXs(text: String, startColumn: Int, fonts: TextMeasurer.LineFonts): IArray[Float]

  def glyphWidthPx(font: FontSpec, text: String): Float

  /** The glyphs, positions and cluster map of `run`'s characters shaped alone at bidi `level` (odd is right to left).
    * `run` is in the columns of `text`, whose first character is at `startColumn`; each [[TextMeasurer.FontRun]] from
    * [[itemise]] is shaped by one call.
    */
  def shape(text: String, startColumn: Int, run: TextMeasurer.FontRun, level: Byte): TextMeasurer.ShapedRun

  /** Carets read from a full layout of `text`, however context-free it is: exact, but a caret query per character. */
  def exactCaretXs(text: String, startColumn: Int, fonts: TextMeasurer.LineFonts): IArray[Float]

  /** Each character's leading caret plus the trailing edge as wrapping places it: the fixed cell grid when
    * `measuredLayout` is false, otherwise [[rawCaretXs]] with collapsed carets spread apart.
    */
  def caretXs(
    text: String,
    startColumn: Int,
    fonts: TextMeasurer.LineFonts,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics
  ): IArray[Float] =
    if text.isEmpty then IArray(0.0f)
    else if !measuredLayout then
      val charWidth = cellMetrics.charWidth.toFloat
      if cellMetrics.displayWidthAware then TextCaretMeasurement.displayWidthCaretXs(text, charWidth)
      else IArray.tabulate(text.length + 1)(index => index * charWidth)
    else TextCaretMeasurement.normalizeCollapsedCarets(rawCaretXs(text, startColumn, fonts))

  /** How many characters of `line` from `from` fit `panelWidthPx` when measured by full layout: candidates of growing
    * length are laid out until one overflows. Always at least one character.
    */
  def exactFit(
    line: String,
    from: Int,
    panelWidthPx: Int,
    startColumn: Int,
    fonts: TextMeasurer.LineFonts,
    cellMetrics: CellMetrics
  ): Int =
    val remainingLength = line.length - from
    val width           = panelWidthPx.toFloat

    @annotation.tailrec
    def loop(limit: Int): Int =
      val carets = TextCaretMeasurement.normalizeCollapsedCarets(
        exactCaretXs(line.substring(from, from + limit), startColumn, fonts)
      )
      val firstOver  = carets.indexWhere(_ > width)
      val maxFitting = if firstOver < 0 then limit else math.max(0, firstOver - 1)
      if limit >= remainingLength || carets(limit) > width || maxFitting < limit then math.max(1, maxFitting)
      else loop(math.min(remainingLength, math.max(limit + 1, limit * 2)))

    loop(math.min(remainingLength, ParagraphMeasurement.initialCandidateLength(panelWidthPx, cellMetrics)))

  /** The smallest cut at or after `from` that shaping in `font` does not read across, or `text.length`. */
  def shapingCutAtOrAfter(text: String, from: Int, font: FontSpec): Int

  /** The largest cut at or before `limit` that shaping in `font` does not read across, or 0. */
  def shapingCutAtOrBefore(text: String, limit: Int, font: FontSpec): Int

  /** False when some character of `text` is shaped from its neighbours whatever the font, so [[advances]] cannot stand
    * in for laying the text out.
    */
  private[layout] def mayMeasureByAdvances(text: String): Boolean

  /** Advances of `text[from, until)`, indexed from `from`. */
  private[layout] def advances(
    text: String,
    from: Int,
    until: Int,
    startColumn: Int,
    fonts: TextMeasurer.LineFonts
  ): GlyphAdvances

object TextMeasurer:

  /** `settings` is everything besides fonts, fallback chain and paragraph direction the backend's measurements depend
    * on, compared by equality.
    */
  final case class MetricsKey(
      backend: String,
      settings: Vector[Matchable],
      fallbackChain: Vector[String] = Vector.empty,
      direction: ParagraphDirection = ParagraphDirection.LeftToRight
  )

  enum ParagraphDirection:
    case LeftToRight, RightToLeft

  /** One font run shaped: glyph `i` is drawn at (`xs(i)`, `ys(i)`) and stands for the characters from `clusters(i)`;
    * `xs` and `ys` also hold the pen position after the last glyph. Positions are relative to the run's start and in
    * visual order, so at an odd `level` they are not in character order.
    */
  final case class ShapedRun(
      font: FontSpec,
      level: Byte,
      glyphs: IArray[Int],
      xs: IArray[Float],
      ys: IArray[Float],
      clusters: IArray[Int]
  ):
    def advancePx: Float = xs(glyphs.length)

  final case class LineMetrics(heightPx: Int, ascentPx: Int)

  /** `font` for the columns `[startColumn, endColumn)`. */
  final case class FontRun(startColumn: Int, endColumn: Int, font: FontSpec)

  /** The fonts one logical line is styled in: `base` wherever no run covers a column. */
  final case class LineFonts(base: FontSpec, runs: Vector[FontRun])
