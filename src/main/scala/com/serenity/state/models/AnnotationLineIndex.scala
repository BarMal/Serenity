package com.serenity.state.models

import com.serenity.lsp.model.Diagnostic

/** Scene-owned interval node for bounded comment overlap queries. */
final private case class CommentIntervalNode(
    comment: DocumentComment,
    maxEnd: Int,
    left: Option[CommentIntervalNode],
    right: Option[CommentIntervalNode]
)

/** Scene-owned annotation lookup keyed by buffer line. */
final case class AnnotationLineIndex(
    comments: Vector[DocumentComment],
    diagnosticsByLine: Map[Int, List[Diagnostic]]
):

  private lazy val commentTree: Option[CommentIntervalNode] =
    def build(sorted: Vector[DocumentComment]): Option[CommentIntervalNode] =
      if sorted.isEmpty then None
      else
        val middle  = sorted.length / 2
        val comment = sorted(middle)
        val left    = build(sorted.take(middle))
        val right   = build(sorted.drop(middle + 1))
        // comment.end.line is always present, so folding from it as the seed always yields the true max.
        val maxEnd = (left.toList.map(_.maxEnd) ::: right.toList.map(_.maxEnd)).foldLeft(comment.end.line)(_ max _)
        Some(CommentIntervalNode(comment, maxEnd, left, right))
    build(comments.sortBy(_.start.line))

  def commentsByLine(visibleLines: Set[Int]): Map[Int, List[DocumentComment]] =
    if visibleLines.isEmpty then Map.empty
    else
      // visibleLines is non-empty here (guarded by the isEmpty check above).
      val start = visibleLines.foldLeft(Int.MaxValue)(_ min _)
      val end   = visibleLines.foldLeft(Int.MinValue)(_ max _)
      def overlapping(node: Option[CommentIntervalNode]): List[DocumentComment] =
        node match
          case None => Nil
          case Some(current) =>
            val fromLeft = if current.left.exists(_.maxEnd >= start) then overlapping(current.left) else Nil
            val here =
              if current.comment.start.line <= end && current.comment.end.line >= start then List(current.comment)
              else Nil
            val fromRight = if current.comment.start.line <= end then overlapping(current.right) else Nil
            fromLeft ::: here ::: fromRight
      overlapping(commentTree).foldLeft(Map.empty[Int, List[DocumentComment]]) { (byLine, comment) =>
        (comment.start.line.max(start) to comment.end.line.min(end)).iterator
          .filter(visibleLines.contains)
          .foldLeft(byLine)((updated, line) => updated.updated(line, comment :: updated.getOrElse(line, Nil)))
      }
