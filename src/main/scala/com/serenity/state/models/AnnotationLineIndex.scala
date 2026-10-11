package com.serenity.state.models

import scala.annotation.tailrec

import com.serenity.lsp.model.Diagnostic

/** Scene-owned interval node for bounded comment overlap queries. */
final private case class CommentIntervalNode(
    comment: DocumentComment,
    maxEnd: Int,
    left: Option[CommentIntervalNode],
    right: Option[CommentIntervalNode]
)

/** The comments of an index as an interval tree, built when first asked for and shared by every index that carries the
  * same comments.
  */
final private[models] class CommentTree(comments: Vector[DocumentComment]):

  lazy val root: Option[CommentIntervalNode] =
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

private[models] object CommentTree:
  def apply(comments: Vector[DocumentComment]): CommentTree = new CommentTree(comments)

/** Scene-owned annotation lookup keyed by buffer line. */
final case class AnnotationLineIndex(
    comments: Vector[DocumentComment],
    diagnosticsByLine: Map[Int, List[Diagnostic]],
    diagnostics: List[Diagnostic] = Nil
)(private[models] val tree: CommentTree):

  /** This index for `nextComments` and `nextDiagnostics`. An edit moves only the diagnostics near it, so the lines of
    * the ones that changed are regrouped and every other line is kept, instead of grouping the whole list again.
    */
  def carriedTo(
    nextComments: List[DocumentComment],
    nextDiagnostics: List[Diagnostic],
    move: Option[DiagnosticsMove]
  ): AnnotationLineIndex =
    AnnotationLineIndex.changedLines(diagnostics, nextDiagnostics, move) match
      case Some(lines) =>
        val regrouped         = AnnotationLineIndex.diagnosticsBySpannedLine(nextDiagnostics.filter(touches(_, lines)))
        val nextCommentVector = nextComments.toVector
        val sameComments      = nextCommentVector == comments
        AnnotationLineIndex(
          if sameComments then comments else nextCommentVector,
          (diagnosticsByLine -- lines) ++ regrouped.filter((line, _) => lines.contains(line)),
          nextDiagnostics
        )(if sameComments then tree else CommentTree(nextCommentVector))
      case None => AnnotationLineIndex.from(nextComments, nextDiagnostics)

  private def touches(diagnostic: Diagnostic, lines: Set[Int]): Boolean =
    val first = diagnostic.range.start.line
    val last  = AnnotationLineIndex.lastSpannedLine(diagnostic)
    lines.exists(line => line >= first && line <= last)

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
      overlapping(tree.root).foldLeft(Map.empty[Int, List[DocumentComment]]) { (byLine, comment) =>
        (comment.start.line.max(start) to comment.end.line.min(end)).iterator
          .filter(visibleLines.contains)
          .foldLeft(byLine)((updated, line) => updated.updated(line, comment :: updated.getOrElse(line, Nil)))
      }

object AnnotationLineIndex:

  def from(comments: List[DocumentComment], diagnostics: List[Diagnostic]): AnnotationLineIndex =
    val commentVector = comments.toVector
    AnnotationLineIndex(commentVector, diagnosticsBySpannedLine(diagnostics), diagnostics)(CommentTree(commentVector))

  /** The lines of the diagnostics that differ between two lists of the same length, compared position by position, as
    * an edit leaves them: `None` when the lists cannot be compared that way. Lists sharing a tail are not walked past
    * it.
    */
  def changedLines(
    before: List[Diagnostic],
    after: List[Diagnostic],
    move: Option[DiagnosticsMove] = None
  ): Option[Set[Int]] =
    move.filter(known => (known.before eq before) && (known.after eq after)).map(_.lines).orElse(walked(before, after))

  private def walked(before: List[Diagnostic], after: List[Diagnostic]): Option[Set[Int]] =
    @tailrec
    def walk(was: List[Diagnostic], now: List[Diagnostic], changed: Set[Int]): Option[Set[Int]] =
      if was eq now then Some(changed)
      else
        (was, now) match
          case (earlier :: earlierRest, later :: laterRest) =>
            val next =
              if (earlier eq later) || earlier == later then changed
              else changed ++ spannedLines(earlier) ++ spannedLines(later)
            walk(earlierRest, laterRest, next)
          case _ => None
    walk(before, after, Set.empty)

  /** Each diagnostic under every line its range covers. A range that ends at the very start of a line does not cover
    * that line, as in the LSP where an end position is exclusive.
    */
  def diagnosticsBySpannedLine(diagnostics: List[Diagnostic]): Map[Int, List[Diagnostic]] =
    diagnostics
      .flatMap(diagnostic => spannedLines(diagnostic).map(_ -> diagnostic))
      .groupMap(_._1)(_._2)

  private[models] def spannedLines(diagnostic: Diagnostic): Range =
    diagnostic.range.start.line to lastSpannedLine(diagnostic)

  private[models] def lastSpannedLine(diagnostic: Diagnostic): Int =
    val range = diagnostic.range
    val last =
      if range.end.character == 0 && range.end.line > range.start.line then range.end.line - 1 else range.end.line
    last.max(range.start.line)
