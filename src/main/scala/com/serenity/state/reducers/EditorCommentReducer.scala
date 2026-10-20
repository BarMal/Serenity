package com.serenity.state.reducers

import scala.annotation.tailrec

import com.serenity.document.{BlockCommentDelimiters, CommentSyntax}
import com.serenity.keystroke.events.{TextEntryEvent, ToggleBlockComment, ToggleLineComment}
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEditSupport.*
import com.serenity.state.undo.EditGrouping

/** Comments and uncomments with the tokens of the buffer's language ([[CommentSyntax]]). Both toggles are computed as
  * one batch of non-overlapping edits, so they are a single undo step and the selection can be carried across them.
  */
private[reducers] object EditorCommentReducer:

  import EditorCursorMovement.activeSelections
  import EditorCursorSupport.CursorEventContext

  def reduce(event: TextEntryEvent, ctx: CursorEventContext): ReducerResult =
    import ctx.{buffer, currentState, paneId}
    val syntax = buffer.document.language.map(CommentSyntax.of)
    val toggle = event match
      case ToggleLineComment  => syntax.flatMap(lineToggle(buffer, _))
      case ToggleBlockComment => syntax.flatMap(_.block).map(blockToggle(buffer, _))
      case _                  => None
    toggle.filter(_.edits.nonEmpty) match
      case None => ReducerResult.noEffects(currentState)
      case Some(Toggle(edits, gravity)) =>
        val (updated, applied) = applyToggle(buffer, edits, gravity)
        ReducerResult(
          Focused.replaceBuffer(currentState, updated),
          undoBoundaryEffects(buffer.id, paneId, buffer, applied, EditGrouping.Standalone)
        )

  /** Where a selection's ends go when text is inserted exactly at them. A bare caret always moves past the insertion,
    * so it stays on the text it was on.
    */
  final private case class Gravity(selectionStartMoves: Boolean, selectionEndMoves: Boolean)

  final private case class Toggle(edits: List[MultiCursorEdit], gravity: Gravity)

  /** A line comment keeps the selection over the lines it covered; a block comment leaves the commented text selected,
    * inside its delimiters.
    */
  private val coverLines   = Gravity(selectionStartMoves = false, selectionEndMoves = true)
  private val insideBlocks = Gravity(selectionStartMoves = true, selectionEndMoves = false)

  /** A line comment marker: the language's line token, or -- where it has none, as in Markdown and CSS -- its block
    * delimiters around each line.
    */
  final private case class LineMarker(open: String, close: Option[String]):

    def isCommented(line: String): Boolean =
      val text = line.strip
      text.startsWith(open) && close.forall(end => text.length >= open.length + end.length && text.endsWith(end))

  private object LineMarker:

    def of(syntax: CommentSyntax): Option[LineMarker] =
      syntax.line
        .map(LineMarker(_, None))
        .orElse(syntax.block.map(delimiters => LineMarker(delimiters.open, Some(delimiters.close))))

  final private case class SourceLine(index: Int, text: String):
    def isBlank: Boolean          = text.isBlank
    def indentLength: Int         = text.length - text.stripLeading.length
    def contentEnd: Int           = text.stripTrailing.length
    def indent: String            = text.take(indentLength)
    def start(content: Rope): Int = content.lineColumnToOffset(index, 0)

  private def lineToggle(buffer: Buffer, syntax: CommentSyntax): Option[Toggle] =
    LineMarker.of(syntax).map { marker =>
      val content = buffer.document.content
      val lines   = targetLines(buffer).flatMap(index => content.getLine(index).map(SourceLine(index, _)))
      val written = lines.filterNot(_.isBlank)
      val edits =
        if written.nonEmpty && written.forall(line => marker.isCommented(line.text)) then
          written.flatMap(uncommentLine(content, marker, _))
        else commentLines(content, marker, if written.isEmpty then lines else written)
      Toggle(numbered(edits), coverLines)
    }

  /** The lines a toggle covers: those with a caret, and those of a selection -- except the last when the selection ends
    * at its very start, since none of its text is selected.
    */
  private def targetLines(buffer: Buffer): List[Int] =
    val selected = activeSelections(buffer).flatMap { selection =>
      val endsAtLineStart = selection.end.column == 0 && selection.end.line > selection.start.line
      selection.start.line to (if endsAtLineStart then selection.end.line - 1 else selection.end.line)
    }
    val carets = buffer.cursorList.toList.filter(_.selection.isEmpty).map(_.position.line)
    (selected ++ carets).distinct.sorted

  private def commentLines(content: Rope, marker: LineMarker, lines: List[SourceLine]): List[MultiCursorEdit] =
    val column = commonIndent(lines.map(_.indent)).length
    lines.flatMap { line =>
      val at = line.start(content) + column
      if line.isBlank then List(insertion(at, marker.open + marker.close.fold(" ")(end => s" $end")))
      else
        insertion(at, marker.open + " ") ::
          marker.close.map(end => insertion(line.start(content) + line.contentEnd, s" $end")).toList
    }

  private def uncommentLine(content: Rope, marker: LineMarker, line: SourceLine): List[MultiCursorEdit] =
    val commented = line.text.substring(line.indentLength, line.contentEnd)
    val base      = line.start(content) + line.indentLength
    unwrapRanges(commented, marker.open, marker.close.getOrElse("")).map {
      case (from, until) => removal(base + from, base + until)
    }

  private def commonIndent(indents: List[String]): String =
    indents.reduceOption((first, second) => first.zip(second).takeWhile(_ == _).map(_._1).mkString).getOrElse("")

  private def blockToggle(buffer: Buffer, delimiters: BlockCommentDelimiters): Toggle =
    val content = buffer.document.content
    val edits = commentableRanges(buffer).flatMap { case (start, end) => toggleBlock(content, delimiters, start, end) }
    Toggle(numbered(edits), insideBlocks)

  /** What each caret or selection comments out: the selected text, or for a caret the text of its line without the
    * indentation and trailing blanks. Ranges that overlap are toggled once.
    */
  private def commentableRanges(buffer: Buffer): List[(Int, Int)] =
    val content                               = buffer.document.content
    def offset(position: CursorPosition): Int = content.lineColumnToOffset(position.line, position.column)
    val ranges = buffer.cursorList.toList
      .flatMap { cursor =>
        cursor.selection match
          case Some(selection) => List((offset(selection.start), offset(selection.end)))
          case None =>
            content.getLine(cursor.position.line).filterNot(_.isBlank).toList.map { text =>
              val line = SourceLine(cursor.position.line, text)
              (line.start(content) + line.indentLength, line.start(content) + line.contentEnd)
            }
      }
      .distinct
      .sorted
    ranges
      .foldLeft(List.empty[(Int, Int)]) {
        case (kept, range) => if kept.headOption.exists(_._2 > range._1) then kept else range :: kept
      }
      .reverse

  private def toggleBlock(
    content: Rope,
    delimiters: BlockCommentDelimiters,
    start: Int,
    end: Int
  ): List[MultiCursorEdit] =
    val BlockCommentDelimiters(open, close) = delimiters
    val text                                = content.sliceString(start, end)
    val leading                             = text.length - text.stripLeading.length
    val trimmed                             = text.strip
    val wrapped = trimmed.length >= open.length + close.length && trimmed.startsWith(open) && trimmed.endsWith(close)
    if wrapped then
      unwrapRanges(trimmed, open, close).map {
        case (from, until) => removal(start + leading + from, start + leading + until)
      }
    else
      val before = content.sliceString((start - open.length - 1).max(0), start)
      val after  = content.sliceString(end, (end + close.length + 1).min(content.weight))
      val openLength =
        if before.endsWith(s"$open ") then open.length + 1 else if before.endsWith(open) then open.length else 0
      val closeLength =
        if after.startsWith(s" $close") then close.length + 1 else if after.startsWith(close) then close.length else 0
      if openLength > 0 && closeLength > 0 then
        List(removal(start - openLength, start), removal(end, end + closeLength))
      else if text.isBlank then Nil
      else List(insertion(start, s"$open "), insertion(end, s" $close"))

  /** The ranges of `commented`, which begins with `open` and ends with `close`, that hold the delimiters and the one
    * space padding each from the text between them.
    */
  private def unwrapRanges(commented: String, open: String, close: String): List[(Int, Int)] =
    val openEnd    = open.length + (if commented.startsWith(" ", open.length) then 1 else 0)
    val closeStart = commented.length - close.length
    val padded     = closeStart - 1 >= openEnd && commented.charAt(closeStart - 1) == ' '
    List((0, openEnd), (if padded then closeStart - 1 else closeStart, commented.length)).filter {
      case (from, until) => from != until
    }

  private def insertion(at: Int, text: String): MultiCursorEdit = MultiCursorEdit(0, at, at, text)

  private def removal(from: Int, until: Int): MultiCursorEdit = MultiCursorEdit(0, from, until, "")

  private def numbered(edits: List[MultiCursorEdit]): List[MultiCursorEdit] =
    edits.sortBy(edit => (edit.start, edit.end)).zipWithIndex.map {
      case (edit, index) => edit.copy(ownerIndex = index)
    }

  private def applyToggle(
    buffer: Buffer,
    edits: List[MultiCursorEdit],
    gravity: Gravity
  ): (Buffer, List[MultiCursorEdit]) =
    val folded = foldEditsTracked(buffer, edits.sortBy(edit => (-edit.start, -edit.end))) { (content, edit) =>
      insertOrUnchanged(deleteOrUnchanged(content, edit.start, edit.end), edit.start, edit.insertedText)
    }
    val applied = folded.appliedAmong(edits)
    val before  = buffer.document.content
    val cursors = buffer.editing.cursors.toList
      .map(cursor => carried(cursor, before, folded.content, applied, gravity))
      .distinct
    val updated = buffer
      .withEditedDocument(folded.content, folded.richText, Some(folded.change(before.weight)))
      .copy(
        editing = EditingState.fromCursors(cursors),
        annotations = adjustAnnotations(buffer.annotations, before, folded.content, applied)
      )
    (updated.clampedToContent, applied)

  private def carried(
    cursor: Cursor,
    before: Rope,
    after: Rope,
    edits: List[MultiCursorEdit],
    gravity: Gravity
  ): Cursor =
    def mapped(position: CursorPosition, moves: Boolean): CursorPosition =
      after.offsetToCursorPosition(remapped(before.lineColumnToOffset(position.line, position.column), edits, moves))
    cursor.selection match
      case None => Cursor(mapped(cursor.position, moves = true))
      case Some(selection) =>
        val anchorIsStart = selection.anchor == selection.start
        Cursor(
          mapped(selection.focus, if anchorIsStart then gravity.selectionEndMoves else gravity.selectionStartMoves),
          Some(
            mapped(selection.anchor, if anchorIsStart then gravity.selectionStartMoves else gravity.selectionEndMoves)
          )
        )

  /** `offset` after `edits` (ascending, never overlapping): text removed around it pulls it back, text inserted before
    * it pushes it on, and text inserted exactly at it does so only when `insertionAtOffsetMoves`.
    */
  private def remapped(offset: Int, edits: List[MultiCursorEdit], insertionAtOffsetMoves: Boolean): Int =
    @tailrec
    def loop(pending: List[MultiCursorEdit], shift: Int): Int =
      pending match
        case Nil => offset + shift
        case edit :: rest =>
          val delta = edit.insertedText.length - (edit.end - edit.start)
          if edit.start == edit.end then
            if offset > edit.start || (offset == edit.start && insertionAtOffsetMoves) then loop(rest, shift + delta)
            else offset + shift
          else if offset >= edit.end then loop(rest, shift + delta)
          else if offset > edit.start then edit.start + shift
          else offset + shift
    loop(edits, 0)

end EditorCommentReducer
