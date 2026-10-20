package com.serenity.ui.renderer

import com.serenity.state.models.TextVisualLine
import com.serenity.state.reducers.CompositionPreview.Span
import com.serenity.ui.theme.{StyledText, Theme}

/** The pre-edit's look while an input method composes (#2008): underlined, with the clause being converted on the
  * selection background. What the text already looked like (syntax colours, weight) is kept under it.
  */
private[renderer] object CompositionStyling:

  def overlay(segments: List[StyledText], row: TextVisualLine, span: Span, theme: Theme): List[StyledText] =
    val from  = (span.startColumn - row.startColumn).max(0)
    val until = (span.endColumn - row.startColumn).min(row.text.length)
    if row.bufferLine != span.line || from >= until then segments
    else
      val clauseEdges = span.clauses.flatMap(clause =>
        List(span.startColumn + clause.start - row.startColumn, span.startColumn + clause.end - row.startColumn)
      )
      val cuts = (from :: until :: clauseEdges).filter(index => index >= from && index <= until).toSet
      val pieces = segments
        .foldLeft((0, List.empty[(StyledText, Int)])) {
          case ((offset, acc), segment) =>
            (offset + segment.content.length, acc ++ piecesOf(segment, offset, cuts))
        }
        ._2
      pieces.map { (piece, start) =>
        if start >= from && start < until then styled(piece, row.startColumn + start, span, theme) else piece
      }

  private def piecesOf(segment: StyledText, offset: Int, cuts: Set[Int]): List[(StyledText, Int)] =
    val inside = cuts.filter(cut => cut > offset && cut < offset + segment.content.length).toList.sorted
    val points = offset :: inside ::: List(offset + segment.content.length)
    points.zip(points.drop(1)).collect {
      case (start, end) if end > start =>
        segment.copy(content = segment.content.substring(start - offset, end - offset)) -> start
    }

  private def styled(piece: StyledText, column: Int, span: Span, theme: Theme): StyledText =
    val selected = span.clauses.exists { clause =>
      clause.selected && column >= span.startColumn + clause.start && column < span.startColumn + clause.end
    }
    val underlined = piece.style.copy(isUnderlined = true)
    if selected then
      piece.copy(
        style = underlined,
        foregroundColor = theme.selection.foreground,
        backgroundColor = theme.selection.background
      )
    else piece.copy(style = underlined)
