package com.serenity.ui.theme

import java.awt.Font

import scala.annotation.tailrec

import com.serenity.markdown.MarkdownInlineSpans.{Run, Style}
import com.serenity.ui.color.RenderColor

/** How the stretches [[com.serenity.markdown.MarkdownInlineSpans]] finds are measured and painted. Layout and the draw
  * path both go through [[textStyle]], so the glyphs painted are the ones whose advances were measured.
  */
object MarkdownInlineStyling:

  /** Code is set in a monospaced face unless the buffer's font already is one. A heading's text takes its weight,
    * slant, family and size from `headings`, the size being its multiple of the buffer's font, `baseFontSize` points.
    */
  def textStyle(style: Style, baseIsMonospaced: Boolean, baseFontSize: Float, headings: HeadingSettings): TextStyle =
    val heading = Option.when(style.headingLevel > 0)(headings.textStyle(style.headingLevel))
    TextStyle(
      isBold = style.bold || heading.exists(_.isBold),
      isItalic = style.italic || heading.exists(_.isItalic),
      isStrikethrough = style.strike,
      fontFamily = Option.when(style.code && !baseIsMonospaced)(Font.MONOSPACED).orElse(heading.flatMap(_.fontFamily)),
      fontSize = Option.when(style.headingLevel > 0)(baseFontSize * headings.sizeMultiple(style.headingLevel))
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
    baseFontSize: Float,
    headings: HeadingSettings
  ): List[StyledText] =
    val (_, result) = segments.foldLeft((startColumn, List.empty[StyledText])) {
      case ((column, acc), segment) =>
        val pieces = split(segment, column, runs).map {
          case (piece, run) =>
            run.fold(piece)(restyledPiece(piece, _, hideMarkers, theme, baseIsMonospaced, baseFontSize, headings))
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
    baseFontSize: Float,
    headings: HeadingSettings
  ): StyledText =
    if run.isMarker then
      if hideMarkers then
        piece.copy(style = TextStyle.normal, foregroundColor = theme.background, backgroundColor = theme.background)
      else piece.copy(style = TextStyle.normal, foregroundColor = theme.muted)
    else
      // The highlighter sets a heading's text bold; whether it is bold is the heading settings' to say, as measured.
      val highlighted = if run.style.headingLevel > 0 then piece.style.copy(isBold = false) else piece.style
      piece.copy(
        style = highlighted.combine(textStyle(run.style, baseIsMonospaced, baseFontSize, headings)),
        foregroundColor = foreground(piece, run, theme, headings),
        backgroundColor = if run.style.code then theme.panel.background else piece.backgroundColor
      )

  private def foreground(piece: StyledText, run: Run, theme: Theme, headings: HeadingSettings): RenderColor =
    if run.style.muted then theme.muted
    else if run.style.headingLevel > 0 then
      headingForeground(piece.foregroundColor, run.style.headingLevel, theme, headings)
    else piece.foregroundColor

  /** The colour a level-`level` Markdown heading's text is painted in, given the colour `painted` the highlighter gave
    * it. Only the theme's heading colour is replaced, not a link's or a code span's inside the heading.
    */
  def headingForeground(painted: RenderColor, level: Int, theme: Theme, headings: HeadingSettings): RenderColor =
    if painted == theme.colorFor(SyntaxElement.Heading).foreground then
      headings.colourFor(level, markdown = true).resolve(theme)
    else painted

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
