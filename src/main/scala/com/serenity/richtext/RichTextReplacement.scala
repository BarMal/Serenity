package com.serenity.richtext

import scala.annotation.tailrec

import cats.syntax.all.*

/** Carries a rich document onto replacement text that is not an edit of it (a formatter's output): the lines are
  * aligned, a line left as it was keeps its paragraph exactly, and a changed line is edited inside its paragraph so
  * formatting outside the changed span survives. A block whose line was replaced is gone, which the fidelity report of
  * the document's package then names as removed.
  */
object RichTextReplacement:

  private enum Segment:
    case Same(oldIndex: Int)
    case Changed(oldIndexes: Range, newIndexes: Range)

  // The alignment table is lines x lines, so a very large rewrite pairs lines by position instead.
  private val AlignmentCellLimit = 1_000_000

  private val atomCharacters = Set(
    InlineAtom.SoftBreakCharacter,
    InlineAtom.OpaqueCharacter,
    InlineAtom.BlockCharacter
  )

  /** `document` with its paragraphs mapped onto `replacement`, or `None` when that cannot be done faithfully: the
    * replacement puts an atom character where the document has no atom, and an atom cannot be made from a character.
    */
  def carried(document: RichTextDocument, replacement: String): Option[RichTextDocument] =
    val paragraphs = document.paragraphs.toVector
    val oldLines   = paragraphs.map(_.plainText)
    val newLines   = replacement.split("\n", -1).toVector
    val rewritten = segments(oldLines, newLines).foldLeft(Option(List.empty[RichTextParagraph])) { (done, segment) =>
      done.flatMap { soFar =>
        segment match
          case Segment.Same(index) => Some(paragraphs(index) :: soFar)
          case Segment.Changed(olds, news) =>
            rewrittenLines(olds.map(paragraphs).toList, news.map(newLines).toList).map(_.reverse ::: soFar)
      }
    }
    rewritten
      .map(done => RichTextDocument(done.reverse).withSource(document.source).normalized)
      .filter(_.plainText == replacement)

  private def segments(oldLines: Vector[String], newLines: Vector[String]): List[Segment] =
    val prefix = oldLines.zip(newLines).takeWhile(_ == _).size
    val suffix = oldLines.reverseIterator
      .zip(newLines.reverseIterator)
      .take(oldLines.size.min(newLines.size) - prefix)
      .takeWhile(_ == _)
      .size
    val oldMiddle = oldLines.slice(prefix, oldLines.size - suffix)
    val newMiddle = newLines.slice(prefix, newLines.size - suffix)
    val middle =
      if oldMiddle.isEmpty && newMiddle.isEmpty then Nil
      else if oldMiddle.size.toLong * newMiddle.size > AlignmentCellLimit then
        List(Segment.Changed(prefix until oldLines.size - suffix, prefix until newLines.size - suffix))
      else aligned(oldMiddle, newMiddle, prefix)
    (0 until prefix).map(Segment.Same(_)).toList ::: middle ::: (oldLines.size - suffix until oldLines.size)
      .map(Segment.Same(_))
      .toList

  /** The segments between and around the longest common subsequence of lines; `offset` is where the middle starts. */
  private def aligned(oldLines: Vector[String], newLines: Vector[String], offset: Int): List[Segment] =
    val common = commonLines(oldLines, newLines)
    val (_, _, found) = (common :+ (oldLines.size -> newLines.size)).foldLeft((0, 0, List.empty[Segment])) {
      case ((oldAt, newAt, soFar), (oldMatch, newMatch)) =>
        val gap =
          Option.when(oldMatch > oldAt || newMatch > newAt)(
            Segment.Changed((oldAt + offset) until (oldMatch + offset), (newAt + offset) until (newMatch + offset))
          )
        val same = Option.when(oldMatch < oldLines.size)(Segment.Same(oldMatch + offset))
        (oldMatch + 1, newMatch + 1, same.toList ::: gap.toList ::: soFar)
    }
    found.reverse

  /** Index pairs of a longest common subsequence of the two line lists, in order. */
  private def commonLines(oldLines: Vector[String], newLines: Vector[String]): List[(Int, Int)] =
    val bottom = Vector.fill(newLines.size + 1)(0)
    val rows = oldLines.indices
      .foldRight(List(bottom)) { (i, below) =>
        val row = newLines.indices.foldRight(List(0)) { (j, right) =>
          val length =
            if oldLines(i) == newLines(j) then below.head(j + 1) + 1
            else below.head(j).max(right.head)
          length :: right
        }
        row.toVector :: below
      }
      .toVector

    @tailrec def walk(i: Int, j: Int, found: List[(Int, Int)]): List[(Int, Int)] =
      if i >= oldLines.size || j >= newLines.size then found.reverse
      else if oldLines(i) == newLines(j) then walk(i + 1, j + 1, (i -> j) :: found)
      else if rows(i + 1)(j) >= rows(i)(j + 1) then walk(i + 1, j, found)
      else walk(i, j + 1, found)

    walk(0, 0, Nil)

  /** The paragraphs for `newLines` where `olds` stood: lines pair up by position, a paired paragraph is edited in
    * place, and a block, which takes no edit, is replaced by a plain paragraph.
    */
  private def rewrittenLines(olds: List[RichTextParagraph], newLines: List[String]): Option[List[RichTextParagraph]] =
    newLines.zipWithIndex
      .traverse { (text, index) =>
        olds.lift(index) match
          case Some(old) if !old.isOpaqueBlock => edited(old, text)
          case _ => Option.unless(text.exists(atomCharacters))(RichTextParagraph.plain(text))
      }

  private def edited(paragraph: RichTextParagraph, text: String): Option[RichTextParagraph] =
    val before = paragraph.plainText
    val prefix = before.zip(text).takeWhile(_ == _).size
    val suffix = before.reverseIterator
      .zip(text.reverseIterator)
      .take(before.length.min(text.length) - prefix)
      .takeWhile(_ == _)
      .size
    val inserted = text.substring(prefix, text.length - suffix)
    Option.unless(inserted.exists(atomCharacters))(
      paragraph.replaceRange(prefix, before.length - suffix, inserted).normalized
    )
