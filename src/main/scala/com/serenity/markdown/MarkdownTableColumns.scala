package com.serenity.markdown

/** How the cells of a pipe table are padded so its pipes line up. A cell's width is the distance from the pipe before
  * it to the pipe after it; every row pads each cell up to the widest of its column, by giving one column an extra
  * advance (see the layout's `LineFontResolver.extraAdvances`). Measuring is the layout's job; this is the geometry.
  */
object MarkdownTableColumns:

  enum Alignment:
    case Left, Right, Center

  /** The widest cell of each column, in pixels, measured with the markers hidden, and the column alignments. */
  final case class Columns(widthsPx: Vector[Float], alignments: Vector[Alignment])

  /** The text of a row between two pipes, `[start, end)`; `end` is the column of the closing pipe, or the line's length
    * for a last cell that has none.
    */
  final case class Cell(start: Int, end: Int):
    def hasClosingPipe(line: String): Boolean = end < line.length

  private val PaddingThresholdPx = 0.01f

  def cells(line: String): Vector[Cell] =
    val pipes   = MarkdownBlockSpans.pipeColumns(line)
    val leading = pipes.headOption.exists(first => line.take(first).forall(_.isWhitespace))
    val starts  = if leading then pipes.map(_ + 1) else 0 +: pipes.map(_ + 1)
    val ends    = pipes.drop(if leading then 1 else 0) :+ line.length
    val all     = starts.zip(ends).map { case (start, end) => Cell(start, end) }
    all.lastOption.filter(last => pipes.nonEmpty && line.substring(last.start, last.end).forall(_.isWhitespace)) match
      case Some(_) => all.dropRight(1)
      case None    => all

  def alignments(delimiterRow: String): Vector[Alignment] =
    cells(delimiterRow).map { cell =>
      val text = delimiterRow.substring(cell.start, cell.end).trim
      if text.startsWith(":") && text.endsWith(":") && text.length > 1 then Alignment.Center
      else if text.endsWith(":") then Alignment.Right
      else Alignment.Left
    }

  /** The extra advance, by column of `line`, that pads each pipe-terminated cell from its measured `cellWidthsPx` up to
    * its column's width. The padding sits where the alignment puts it: after the text, before it, or split. It never
    * lands on a hidden column, which has no advance to widen.
    */
  def extraAdvances(
    line: String,
    cellWidthsPx: Vector[Float],
    columns: Columns,
    isHidden: Int => Boolean
  ): Map[Int, Float] =
    cells(line).zipWithIndex.foldLeft(Map.empty[Int, Float]) {
      case (found, (cell, index)) =>
        val padding = columns.widthsPx.lift(index).getOrElse(0.0f) - cellWidthsPx.lift(index).getOrElse(0.0f)
        if !cell.hasClosingPipe(line) || padding < PaddingThresholdPx then found
        else
          val lastVisible = (cell.end - 1 to cell.start by -1).find(!isHidden(_))
          val openingPipe = Option.when(cell.start > 0)(cell.start - 1)
          val targets: List[(Int, Float)] = columns.alignments.lift(index).getOrElse(Alignment.Left) match
            case Alignment.Left  => lastVisible.orElse(openingPipe).map((_, padding)).toList
            case Alignment.Right => openingPipe.orElse(lastVisible).map((_, padding)).toList
            case Alignment.Center =>
              (openingPipe, lastVisible) match
                case (Some(opening), Some(last)) => List((opening, padding / 2), (last, padding - padding / 2))
                case (opening, last)             => opening.orElse(last).map((_, padding)).toList
          targets.foldLeft(found) {
            case (acc, (column, share)) => acc.updated(column, acc.getOrElse(column, 0.0f) + share)
          }
    }
