package com.serenity.ui.layout

import java.awt.font.{FontRenderContext, TextAttribute, TextHitInfo, TextLayout}
import java.text.{AttributedString, StringCharacterIterator}
import java.util.Locale

import com.ibm.icu.text.BreakIterator
import com.serenity.richtext.ParagraphRole
import com.serenity.state.models.{TextCaretStop, TextVisualLine}
import com.serenity.text.TextEditing
import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver
import com.serenity.ui.renderer.{FontFirstUse, FontProbes}

/** The wrap-and-measure algorithm as it stood before per-paragraph measurement: every row fitted by laying out a
  * candidate prefix with `TextLayout`, and every row's carets read back from a `TextLayout` of that row alone. Kept as
  * the reference the production path is checked against.
  */
object LegacyLineMeasurement:

  private val lineExtents = FontLineExtents()

  def wrap(
    text: String,
    bufferLine: Int,
    panelWidthPx: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics,
    baseColumn: Int = 0,
    maxVisualLines: Int = Int.MaxValue,
    paragraphRole: ParagraphRole = ParagraphRole.Body,
    dropCapIndentPx: Float = 0.0f
  ): Vector[TextVisualLine] =
    def shape(segment: String, start: Int, end: Int) =
      shapeSegment(segment, bufferLine, start, end, resolver, frc, measuredLayout, cellMetrics)
    if maxVisualLines <= 0 then Vector.empty
    else if text.isEmpty then Vector(inset(shape("", baseColumn, baseColumn), paragraphRole, 0, dropCapIndentPx))
    else
      /** The home row's wrap gives back the width of the character the glyph is drawn in place of, as its placement
        * does.
        */
      def replacedCharacterWidthPx(startColumn: Int): Float =
        val first = text.substring(startColumn, startColumn + Character.charCount(text.codePointAt(startColumn)))
        shape(first, baseColumn + startColumn, baseColumn + startColumn + first.length)
          .xForColumn(baseColumn + startColumn + first.length)
          .getOrElse(0.0f)

      @annotation.tailrec
      def loop(startColumn: Int, acc: Vector[TextVisualLine]): Vector[TextVisualLine] =
        if startColumn >= text.length || acc.length >= maxVisualLines then acc
        else
          val spanInsetPx = DropCapLayout.leftInsetPx(paragraphRole, acc.length, dropCapIndentPx)
          val givenBackPx =
            if DropCapLayout.isHomeRow(paragraphRole, acc.length, baseColumn + startColumn) then
              replacedCharacterWidthPx(startColumn).min(spanInsetPx)
            else 0.0f
          val wrapWidthPx  = math.max(1, panelWidthPx - math.round(spanInsetPx))
          val segmentStart = baseColumn + startColumn
          val fitted =
            fittingLength(
              text,
              startColumn,
              wrapWidthPx,
              givenBackPx,
              segmentStart,
              resolver,
              frc,
              measuredLayout,
              cellMetrics
            )
          val segmentLength = wordBoundarySegmentLength(text, startColumn, fitted)
          val end           = startColumn + segmentLength
          val row           = shape(text.substring(startColumn, end), segmentStart, baseColumn + end)
          loop(end, acc :+ inset(row, paragraphRole, acc.length, dropCapIndentPx))
      loop(0, Vector.empty)

  /** The indent every spanned row starts at. The home row's first character is drawn as the glyph, so the row gives the
    * width of that character back from its offset, and its own text then starts at the indent as well.
    */
  private def inset(row: TextVisualLine, role: ParagraphRole, lineWithinParagraph: Int, indentPx: Float) =
    val indent = DropCapLayout.leftInsetPx(role, lineWithinParagraph, indentPx)
    val dropped =
      if DropCapLayout.isHomeRow(role, lineWithinParagraph, row.startColumn) && row.text.nonEmpty then
        row.xForColumn(row.startColumn + Character.charCount(row.text.codePointAt(0))).getOrElse(0.0f)
      else 0.0f
    row.copy(xOffsetPx = row.xOffsetPx + (indent - dropped).max(0.0f))

  def shapeSegment(
    text: String,
    bufferLine: Int,
    startColumn: Int,
    endColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics
  ): TextVisualLine =
    val xs = caretXs(text, startColumn, resolver, frc, measuredLayout, cellMetrics)
    val caretStops = graphemeBoundaryOffsets(text).map { offset =>
      TextCaretStop(startColumn + offset, xs.lift(offset).getOrElse(xs.lastOption.getOrElse(0.0f)))
    }
    val sorted =
      if caretStops.sliding(2).forall {
            case Vector(first, second) => first.xPx <= second.xPx
            case _                     => true
          }
      then caretStops
      else caretStops.sortBy(_.xPx)
    val (heightPx, ascentPx) =
      if measuredLayout then resolver.lineMetrics(frc, lineExtents, startColumn, endColumn) else (0, 0)
    TextVisualLine(
      bufferLine = bufferLine,
      startColumn = startColumn,
      endColumn = endColumn,
      text = text,
      widthPx = xs.lastOption.getOrElse(0.0f),
      caretStops = caretStops,
      xSortedCaretStops = sorted,
      heightPx = heightPx,
      ascentPx = ascentPx
    )

  def caretXs(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics
  ): Vector[Float] =
    if text.isEmpty then Vector(0.0f)
    else if !measuredLayout then
      val charWidth = cellMetrics.charWidth.toFloat
      if cellMetrics.displayWidthAware then displayWidthCaretXs(text, charWidth, resolver.tabSize)
      else uniformCaretXs(text, charWidth, resolver.tabSize)
    else normalizeCollapsedCarets(layoutCaretXs(text, absoluteStartColumn, resolver, frc))

  /** The stops of a row and, for a row read left to right, the right edge of the characters before each of them shaped
    * on their own.
    */
  final case class RowEdges(carets: Vector[Float], trailing: Option[Vector[Float]])

  /** Each character's caret read per row, the way the draw path places glyphs: a left-to-right character at the x of
    * its glyph, a right-to-left one at the leading (right) edge of its glyph, a ligature's characters sharing its span
    * evenly. A row in several fonts that also has right-to-left text, and a slanted face, are read from `TextLayout` as
    * before.
    */
  def layoutCaretXs(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext
  ): Vector[Float] =
    layoutEdges(text, absoluteStartColumn, resolver, frc).carets

  /** [[unsnappedEdges]] with each wide character of a code line set to two cells of the primary font and each one-cell
    * fallback character to one, the edges after it moved to match: the rule the production path adds, applied here row
    * by row.
    */
  def layoutEdges(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext
  ): RowEdges =
    val edges = unsnappedEdges(text, absoluteStartColumn, resolver, frc)
    if resolver.wideCellPx <= 0.0f then edges
    else
      val carets =
        WideCells.snapped(
          IArray.from(edges.carets),
          text,
          resolver.wideCellPx,
          index => resolver.isFallbackAt(absoluteStartColumn + index)
        )
      RowEdges(
        carets.toVector,
        edges.trailing.map(trailing =>
          trailing.zipWithIndex.map((edge, index) => edge + carets(index) - edges.carets(index))
        )
      )

  private def unsnappedEdges(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext
  ): RowEdges =
    if text.isEmpty then RowEdges(Vector(0.0f), Some(Vector(0.0f)))
    else
      val fonts = (0 until text.length).map(index => resolver.fontAt(absoluteStartColumn + index)).distinct
      val bidi  = java.text.Bidi.requiresBidi(text.toCharArray, 0, text.length)
      if !fonts.forall(GlyphAdvances.layoutAdvanceMatchesSum(_, frc, FontProbes())) || (bidi && fonts.length > 1) then
        RowEdges(textLayoutCaretXs(text, absoluteStartColumn, resolver, frc), None)
      else glyphEdges(text, absoluteStartColumn, resolver, frc, bidi)

  final private case class PlacedGlyph(char: Int, x: Float, advance: Float, rightToLeft: Boolean)

  private def glyphEdges(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    bidi: Boolean
  ): RowEdges =
    @annotation.tailrec
    def runsFrom(index: Int, acc: Vector[(Int, Int, java.awt.Font)]): Vector[(Int, Int, java.awt.Font)] =
      if index >= text.length then acc
      else
        val runFont = resolver.fontAt(absoluteStartColumn + index)
        @annotation.tailrec
        def runEnd(cursor: Int): Int =
          if cursor < text.length && resolver.fontAt(absoluteStartColumn + cursor) == runFont then runEnd(cursor + 1)
          else cursor
        val end = runEnd(index + 1)
        runsFrom(end, acc :+ ((index, end, runFont)))
    def shapeRun(start: Int, end: Int, runFont: java.awt.Font, rightToLeft: Boolean): (Vector[PlacedGlyph], Float) =
      FontFirstUse.settle(runFont, frc)
      val chars  = text.substring(start, end).toCharArray
      val flag   = if rightToLeft then java.awt.Font.LAYOUT_RIGHT_TO_LEFT else java.awt.Font.LAYOUT_LEFT_TO_RIGHT
      val glyphs = runFont.layoutGlyphVector(frc, chars, 0, chars.length, flag)
      val count  = glyphs.getNumGlyphs
      val starts = glyphs.getGlyphCharIndices(0, count, new Array[Int](count))
      val xs     = glyphs.getGlyphPositions(0, count + 1, new Array[Float](2 * (count + 1)))
      val placed = Vector.tabulate(count) { glyph =>
        PlacedGlyph(start + starts(glyph), xs(2 * glyph), glyphs.getGlyphMetrics(glyph).getAdvance, rightToLeft)
      }
      (placed, xs(2 * count))
    val (placed, advance) =
      if bidi then
        val paragraph = java.text.Bidi(text, java.text.Bidi.DIRECTION_DEFAULT_LEFT_TO_RIGHT)
        val count     = paragraph.getRunCount
        val levels    = Array.tabulate(count)(run => paragraph.getRunLevel(run).toByte)
        val order     = Array.tabulate[AnyRef](count)(run => Integer.valueOf(run))
        java.text.Bidi.reorderVisually(levels, 0, order, 0, count)
        val runFont = resolver.fontAt(absoluteStartColumn)
        order.toVector.foldLeft((Vector.empty[PlacedGlyph], 0.0f)) {
          case ((acc, offset), boxed) =>
            val run = boxed.asInstanceOf[Integer].intValue
            val (glyphs, runAdvance) =
              shapeRun(
                paragraph.getRunStart(run),
                paragraph.getRunLimit(run),
                runFont,
                paragraph.getRunLevel(run) % 2 == 1
              )
            (acc ++ glyphs.map(glyph => glyph.copy(x = glyph.x + offset)), offset + runAdvance)
        }
      else
        runsFrom(0, Vector.empty).foldLeft((Vector.empty[PlacedGlyph], 0.0f)) {
          case ((acc, offset), (start, end, runFont)) =>
            val (glyphs, runAdvance) = shapeRun(start, end, runFont, rightToLeft = false)
            (acc ++ glyphs.map(glyph => glyph.copy(x = glyph.x + offset)), offset + runAdvance)
        }
    val own = text.indices.map { char =>
      val at = placed.filter(_.char == char)
      if at.isEmpty then None
      else if at.head.rightToLeft then Some(at.map(glyph => glyph.x + glyph.advance).max)
      else Some(at.map(_.x).min)
    }
    val stops = own.indices.map(char => own(char).orElse(own.drop(char + 1).flatten.headOption).getOrElse(advance))
    val trailing = Option.unless(bidi) {
      val pulls = text.indices.map { char =>
        val glyph     = placed.indexWhere(_.char == char)
        val spanStart = runsFrom(0, Vector.empty).exists(_._1 == char)
        if glyph <= 0 || spanStart || placed(glyph).advance == 0.0f then 0.0f
        else placed(glyph).x - placed.take(glyph).map(before => before.x + before.advance).max
      }
      (stops.indices.map(char => stops(char) - pulls(char)) :+ advance).toVector
    }
    RowEdges((stops :+ advance).toVector, trailing)

  private def textLayoutCaretXs(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext
  ): Vector[Float] =
    val attributed = AttributedString(text)
    @annotation.tailrec
    def applyRuns(index: Int): Unit =
      if index < text.length then
        val runFont = resolver.fontAt(absoluteStartColumn + index)
        @annotation.tailrec
        def runEnd(cursor: Int): Int =
          if cursor < text.length && resolver.fontAt(absoluteStartColumn + cursor) == runFont then runEnd(cursor + 1)
          else cursor
        val end = runEnd(index + 1)
        attributed.addAttribute(TextAttribute.FONT, runFont, index, end)
        applyRuns(end)
    applyRuns(0)
    val layout = TextLayout(attributed.getIterator, frc)
    (0 until text.length).toVector.map(index => layout.getCaretInfo(TextHitInfo.leading(index))(0)) :+
      layout.getAdvance

  private def fittingLength(
    line: String,
    from: Int,
    panelWidthPx: Int,
    givenBackPx: Float,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics
  ): Int =
    val widthPx = panelWidthPx.toFloat + givenBackPx
    if !measuredLayout then
      val charWidth = math.max(1, cellMetrics.charWidth)
      if cellMetrics.displayWidthAware then
        fittingDisplayWidthSegmentLength(
          line,
          from,
          panelWidthPx + math.round(givenBackPx),
          charWidth,
          resolver.tabSize
        )
      else fittingUniformSegmentLength(line, from, panelWidthPx + math.round(givenBackPx), charWidth, resolver.tabSize)
    else
      val remaining = line.length - from
      @annotation.tailrec
      def loop(limit: Int): Int =
        val edges      = layoutEdges(line.substring(from, from + limit), absoluteStartColumn, resolver, frc)
        val carets     = edges.trailing.getOrElse(normalizeCollapsedCarets(edges.carets))
        val maxFitting = carets.zipWithIndex.takeWhile(_._1 <= widthPx).map(_._2).lastOption.getOrElse(0)
        if limit >= remaining || carets.lastOption.exists(_ > widthPx) || maxFitting < limit then
          math.max(1, maxFitting)
        else loop(math.min(remaining, math.max(limit + 1, limit * 2)))
      loop(math.min(remaining, math.max(16, panelWidthPx / math.max(1, cellMetrics.charWidth) + 32)))

  private def wordBoundarySegmentLength(line: String, from: Int, fittingLength: Int): Int =
    if fittingLength >= line.length - from then line.length - from
    else
      val boundary = BreakIterator.getLineInstance(Locale.ROOT)
      boundary.setText(StringCharacterIterator(line, from, line.length, from))
      val candidate = boundary.preceding(from + fittingLength + 1) - from
      if candidate > 0 then candidate else fittingLength

  private def cellsOf(codePoint: Int, usedCells: Int, tabSize: Int): Int =
    if codePoint == '\t' then tabSize - usedCells % tabSize else CharWidth.of(codePoint)

  private def uniformCaretXs(text: String, charWidth: Float, tabSize: Int): Vector[Float] =
    text
      .foldLeft((Vector(0.0f), 0)) {
        case ((acc, cells), char) =>
          val next = cells + (if char == '\t' then tabSize - cells % tabSize else 1)
          (acc :+ next * charWidth, next)
      }
      ._1

  private def fittingUniformSegmentLength(
    line: String,
    from: Int,
    panelWidthPx: Int,
    charWidth: Int,
    tabSize: Int
  ): Int =
    val cellsAvailable = panelWidthPx / charWidth
    val fitted = line
      .drop(from)
      .foldLeft((0, 0, true)) {
        case ((count, cells, open), char) =>
          val next = cells + (if char == '\t' then tabSize - cells % tabSize else 1)
          if open && next <= cellsAvailable then (count + 1, next, true) else (count, cells, false)
      }
      ._1
    math.max(1, fitted)

  private def displayWidthCaretXs(text: String, charWidth: Float, tabSize: Int): Vector[Float] =
    @annotation.tailrec
    def loop(index: Int, cells: Int, acc: Vector[Float]): Vector[Float] =
      if index >= text.length then acc :+ cells * charWidth
      else
        val codePoint = text.codePointAt(index)
        val advanced  = cells + cellsOf(codePoint, cells, tabSize)
        val charCount = Character.charCount(codePoint)
        loop(
          index + charCount,
          advanced,
          if charCount == 2 then acc :+ cells * charWidth :+ advanced * charWidth else acc :+ cells * charWidth
        )
    loop(0, 0, Vector.empty)

  private def fittingDisplayWidthSegmentLength(
    text: String,
    from: Int,
    panelWidthPx: Int,
    charWidth: Int,
    tabSize: Int
  ): Int =
    @annotation.tailrec
    def loop(index: Int, usedCells: Int): Int =
      if index >= text.length then index
      else
        val codePoint = text.codePointAt(index)
        val advance   = cellsOf(codePoint, usedCells, tabSize)
        if (usedCells + advance) * charWidth > panelWidthPx then index
        else loop(index + Character.charCount(codePoint), usedCells + advance)
    val fitted = loop(from, 0) - from
    if fitted > 0 then fitted else math.min(text.length - from, Character.charCount(text.codePointAt(from)))

  def graphemeBoundaryOffsets(text: String): Vector[Int] =
    @annotation.tailrec
    def loop(offset: Int, acc: Vector[Int]): Vector[Int] =
      if offset >= text.length then if acc.lastOption.contains(text.length) then acc else acc :+ text.length
      else
        val next = TextEditing.nextGraphemeBoundary(text, offset)
        loop(next, acc :+ next)
    loop(0, Vector(0))

  def normalizeCollapsedCarets(rawXs: Vector[Float]): Vector[Float] =
    if rawXs.length < 3 then rawXs
    else
      val normalized = rawXs.toArray
      val epsilon    = 0.01f
      @annotation.tailrec
      def plateauEndFrom(index: Int, plateauValue: Float): Int =
        if index + 1 < normalized.length && math.abs(normalized(index + 1) - plateauValue) <= epsilon then
          plateauEndFrom(index + 1, plateauValue)
        else index
      @annotation.tailrec
      def normalizeFrom(index: Int): Unit =
        if index < normalized.length - 1 then
          val plateauValue = normalized(index)
          if math.abs(normalized(index + 1) - plateauValue) <= epsilon then
            val plateauEnd   = plateauEndFrom(index + 1, plateauValue)
            val plateauStart = index - 1
            val startX       = normalized(plateauStart)
            val endX         = normalized(plateauEnd)
            val segmentCount = plateauEnd - plateauStart
            if endX > startX && segmentCount > 0 then
              val step = (endX - startX) / segmentCount.toFloat
              (plateauStart + 1 to plateauEnd).foreach { pointIndex =>
                normalized(pointIndex) = startX + step * (pointIndex - plateauStart)
              }
            normalizeFrom(plateauEnd + 1)
          else normalizeFrom(index + 1)
      normalizeFrom(1)
      normalized.toVector
