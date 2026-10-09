package com.serenity.ui.layout

import java.awt.font.{FontRenderContext, TextAttribute, TextHitInfo, TextLayout}
import java.text.{AttributedString, StringCharacterIterator}
import java.util.Locale

import com.ibm.icu.text.BreakIterator
import com.serenity.richtext.ParagraphRole
import com.serenity.state.models.{TextCaretStop, TextVisualLine}
import com.serenity.text.TextEditing
import com.serenity.ui.layout.TextCaretMeasurement.LineFontResolver

/** The wrap-and-measure algorithm as it stood before per-paragraph measurement: every row fitted by laying out a
  * candidate prefix with `TextLayout`, and every row's carets read back from a `TextLayout` of that row alone. Kept as
  * the reference the production path is checked against.
  */
object LegacyLineMeasurement:

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
    dropCapGlyphWidthPx: Float = 0.0f
  ): Vector[TextVisualLine] =
    def shape(segment: String, start: Int, end: Int) =
      shapeSegment(segment, bufferLine, start, end, resolver, frc, measuredLayout, cellMetrics)
    if maxVisualLines <= 0 then Vector.empty
    else if text.isEmpty then
      Vector(DropCapLayout.applyInset(shape("", baseColumn, baseColumn), paragraphRole, 0, dropCapGlyphWidthPx))
    else
      @annotation.tailrec
      def loop(startColumn: Int, acc: Vector[TextVisualLine]): Vector[TextVisualLine] =
        if startColumn >= text.length || acc.length >= maxVisualLines then acc
        else
          val insetPx      = DropCapLayout.leftInsetPx(paragraphRole, acc.length, dropCapGlyphWidthPx)
          val wrapWidthPx  = math.max(1, panelWidthPx - math.round(insetPx))
          val segmentStart = baseColumn + startColumn
          val fitted =
            fittingLength(text, startColumn, wrapWidthPx, segmentStart, resolver, frc, measuredLayout, cellMetrics)
          val segmentLength = wordBoundarySegmentLength(text, startColumn, fitted)
          val end           = startColumn + segmentLength
          val row           = shape(text.substring(startColumn, end), segmentStart, baseColumn + end)
          loop(end, acc :+ DropCapLayout.applyInset(row, paragraphRole, acc.length, dropCapGlyphWidthPx))
      loop(0, Vector.empty)

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
    val (heightPx, ascentPx) = if measuredLayout then resolver.lineMetrics(frc, startColumn, endColumn) else (0, 0)
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
      if cellMetrics.displayWidthAware then displayWidthCaretXs(text, charWidth)
      else Vector.tabulate(text.length + 1)(index => index * charWidth)
    else normalizeCollapsedCarets(layoutCaretXs(text, absoluteStartColumn, resolver, frc))

  def layoutCaretXs(
    text: String,
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext
  ): Vector[Float] =
    if text.isEmpty then Vector(0.0f)
    else
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
    absoluteStartColumn: Int,
    resolver: LineFontResolver,
    frc: FontRenderContext,
    measuredLayout: Boolean,
    cellMetrics: CellMetrics
  ): Int =
    if !measuredLayout then
      val charWidth = math.max(1, cellMetrics.charWidth)
      if cellMetrics.displayWidthAware then fittingDisplayWidthSegmentLength(line, from, panelWidthPx, charWidth)
      else math.max(1, math.min(line.length - from, panelWidthPx / charWidth))
    else
      val remaining = line.length - from
      @annotation.tailrec
      def loop(limit: Int): Int =
        val carets = normalizeCollapsedCarets(
          layoutCaretXs(line.substring(from, from + limit), absoluteStartColumn, resolver, frc)
        )
        val maxFitting = carets.zipWithIndex.takeWhile(_._1 <= panelWidthPx.toFloat).map(_._2).lastOption.getOrElse(0)
        if limit >= remaining || carets.lastOption.exists(_ > panelWidthPx.toFloat) || maxFitting < limit then
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

  private def displayWidthCaretXs(text: String, charWidth: Float): Vector[Float] =
    @annotation.tailrec
    def loop(index: Int, xPx: Float, acc: Vector[Float]): Vector[Float] =
      if index >= text.length then acc :+ xPx
      else
        val codePoint = text.codePointAt(index)
        val advanced  = xPx + CharWidth.of(codePoint) * charWidth
        val charCount = Character.charCount(codePoint)
        loop(index + charCount, advanced, if charCount == 2 then acc :+ xPx :+ advanced else acc :+ xPx)
    loop(0, 0.0f, Vector.empty)

  private def fittingDisplayWidthSegmentLength(text: String, from: Int, panelWidthPx: Int, charWidth: Int): Int =
    @annotation.tailrec
    def loop(index: Int, usedPx: Int): Int =
      if index >= text.length then index
      else
        val codePoint = text.codePointAt(index)
        val advance   = CharWidth.of(codePoint) * charWidth
        if usedPx + advance > panelWidthPx then index
        else loop(index + Character.charCount(codePoint), usedPx + advance)
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
