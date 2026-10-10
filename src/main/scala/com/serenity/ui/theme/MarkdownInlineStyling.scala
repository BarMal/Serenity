package com.serenity.ui.theme

import java.awt.Font

import scala.annotation.tailrec

import com.serenity.markdown.MarkdownInlineSpans.{Run, Style}

/** How the stretches [[com.serenity.markdown.MarkdownInlineSpans]] finds are measured and painted. Layout and the draw
  * path both go through [[textStyle]], so the glyphs painted are the ones whose advances were measured.
  */
object MarkdownInlineStyling:

  /** Code is set in a monospaced face unless the buffer's font already is one; text with a scale above 1 is set that
    * much larger than the buffer's font, which is `baseFontSize` points.
    */
  def textStyle(style: Style, baseIsMonospaced: Boolean, baseFontSize: Float): TextStyle =
    TextStyle(
      isBold = style.bold,
      isItalic = style.italic,
      isStrikethrough = style.strike,
      fontFamily = Option.when(style.code && !baseIsMonospaced)(Font.MONOSPACED),
      fontSize = Option.when(style.scale > 1.0f)(baseFontSize * style.scale)
    )

  /** `segments`, the highlighted pieces of a visual line whose first character is at buffer column `startColumn`, split
    * wherever `runs` change and restyled: content takes its emphasis, code, size or muted style, and a marker is drawn
    * muted, or in the background colour when `hideMarkers` -- it then has no width, so nothing of it shows. Pieces
    * outside every run are untouched.
    */
  def restyle(
    segments: List[StyledText],
    runs: Vector[Run],
    startColumn: Int,
    hideMarkers: Boolean,
    theme: Theme,
    baseIsMonospaced: Boolean,
    baseFontSize: Float
  ): List[StyledText] =
    val (_, result) = segments.foldLeft((startColumn, List.empty[StyledText])) {
      case ((column, acc), segment) =>
        val pieces = split(segment, column, runs).map {
          case (piece, run) =>
            run.fold(piece)(restyledPiece(piece, _, hideMarkers, theme, baseIsMonospaced, baseFontSize))
        }
        (column + segment.content.length, acc ++ pieces)
    }
    result

  private def restyledPiece(
    piece: StyledText,
    run: Run,
    hideMarkers: Boolean,
    theme: Theme,
    baseIsMonospaced: Boolean,
    baseFontSize: Float
  ): StyledText =
    if run.isMarker then
      if hideMarkers then
        piece.copy(style = TextStyle.normal, foregroundColor = theme.background, backgroundColor = theme.background)
      else piece.copy(style = TextStyle.normal, foregroundColor = theme.muted)
    else
      piece.copy(
        style = piece.style.combine(textStyle(run.style, baseIsMonospaced, baseFontSize)),
        foregroundColor = if run.style.muted then theme.muted else piece.foregroundColor,
        backgroundColor = if run.style.code then theme.panel.background else piece.backgroundColor
      )

  /** `segment` cut at the edges of the runs it overlaps, each piece with the run covering it, if any. */
  private def split(segment: StyledText, firstColumn: Int, runs: Vector[Run]): List[(StyledText, Option[Run])] =
    val lastColumn = firstColumn + segment.content.length

    @tailrec
    def from(column: Int, acc: List[(StyledText, Option[Run])]): List[(StyledText, Option[Run])] =
      if column >= lastColumn then acc.reverse
      else
        val covering = runs.find(run => run.startColumn <= column && column < run.endColumn)
        val end = covering match
          case Some(run) => math.min(run.endColumn, lastColumn)
          case None =>
            math.min(runs.find(_.startColumn > column).map(_.startColumn).getOrElse(lastColumn), lastColumn)
        val piece = segment.copy(content = segment.content.substring(column - firstColumn, end - firstColumn))
        from(end, (piece, covering) :: acc)

    from(firstColumn, Nil)
