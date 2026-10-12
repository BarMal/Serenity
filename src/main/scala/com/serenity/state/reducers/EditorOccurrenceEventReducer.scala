package com.serenity.state.reducers

import scala.annotation.tailrec

import cats.data.NonEmptyList
import com.serenity.keystroke.events.{SelectAllOccurrences, SelectNextOccurrence, SkipOccurrence, TextEntryEvent}
import com.serenity.rope.Rope
import com.serenity.state.models.*

/** The commands that grow a selection into several: select the next occurrence of the selected text (or of the word
  * under a bare caret), skip it, or select every one (#1994, #1951). Each new match becomes the first cursor, so the
  * caret the viewport follows is the newest one.
  *
  * A selection that began as a bare caret matches whole words only; one the user made matches wherever the text occurs.
  * That choice is kept in [[EditingState.occurrenceSearch]] across presses, and any other change of cursors drops it.
  * Matching is case-sensitive, so renaming `cat` leaves `Cat` alone.
  */
private[reducers] object EditorOccurrenceEventReducer:
  import EditorCursorSupport.CursorEventContext

  /** What VS Code calls `multiCursorLimit`: past this many cursors every edit path gets slow enough to read as hung. */
  val CursorLimit: Int = 10000

  def reduce(event: TextEntryEvent, ctx: CursorEventContext): ReducerResult =
    val updated = event match
      case SelectNextOccurrence => selectNext(ctx.buffer)
      case SkipOccurrence       => skip(ctx.buffer)
      case SelectAllOccurrences => selectAll(ctx.buffer)
      case _                    => None
    ReducerResult.noEffects(updated.fold(ctx.currentState)(Focused.replaceBuffer(ctx.currentState, _)))

  private def selectNext(buffer: Buffer): Option[Buffer] =
    if buffer.allSelections.isEmpty then expandCarets(buffer)
    else if buffer.editing.cursors.size >= CursorLimit then None
    else
      for
        reference <- referenceSelection(buffer)
        search = searchFor(buffer, reference)
        found <- nextUnselected(buffer, search, reference)
      yield withCursors(buffer, Cursor(found), buffer.editing.cursors.toList, search)

  private def skip(buffer: Buffer): Option[Buffer] =
    for
      reference <- buffer.editing.cursors.head.selection
      search = searchFor(buffer, reference)
      found <- nextUnselected(buffer, search, reference)
    yield withCursors(buffer, Cursor(found), buffer.editing.cursors.tail, search)

  private def selectAll(buffer: Buffer): Option[Buffer] =
    val content = buffer.document.content
    val reference = referenceSelection(buffer)
      .map(selection => selection -> searchFor(buffer, selection))
      .orElse(wordAt(content, buffer.editing.cursors.head.position).map(word => word -> wholeWordSearch(content, word)))
    for
      (selection, search) <- reference
      scan                <- scanFor(content, search)
    yield
      val (selectionStart, _) = offsetsOf(content, selection)
      val others = scan
        .spansFrom(0)
        .filterNot(_.start == selectionStart)
        .take(CursorLimit - 1)
        .map(span => Cursor(selectionOf(content, span)))
        .toList
      withCursors(buffer, Cursor(selection), others, search)

  /** Every bare caret takes the word it is in or beside; carets in no word stay bare. */
  private def expandCarets(buffer: Buffer): Option[Buffer] =
    val content  = buffer.document.content
    val expanded = buffer.editing.cursors.map(cursor => cursor -> wordAt(content, cursor.position))
    expanded.toList.collectFirst { case (_, Some(word)) => word }.map { firstWord =>
      val cursors = expanded.map((cursor, word) => word.fold(cursor)(Cursor(_)))
      val rest    = cursors.tail.filterNot(_ == cursors.head).distinct
      withCursors(buffer, cursors.head, rest, wholeWordSearch(content, firstWord))
    }

  private def referenceSelection(buffer: Buffer): Option[Selection] =
    buffer.editing.cursors.head.selection.orElse(buffer.allSelections.headOption)

  /** The choice kept from earlier presses while the selection still reads as what they were looking for. */
  private def searchFor(buffer: Buffer, selection: Selection): OccurrenceSearch =
    val text = textOf(buffer.document.content, selection)
    buffer.editing.occurrenceSearch.filter(_.text == text).getOrElse(OccurrenceSearch(text, wholeWord = false))

  private def wholeWordSearch(content: Rope, word: Selection): OccurrenceSearch =
    OccurrenceSearch(textOf(content, word), wholeWord = true)

  /** The first match after `reference`, wrapping round the document, that no cursor selects yet. */
  private def nextUnselected(buffer: Buffer, search: OccurrenceSearch, reference: Selection): Option[Selection] =
    val content           = buffer.document.content
    val (_, referenceEnd) = offsetsOf(content, reference)
    val taken             = buffer.allSelections.map(selection => offsetsOf(content, selection)._1).toSet
    scanFor(content, search).flatMap { scan =>
      val lap = scan.spansFrom(referenceEnd) ++ scan.spansFrom(0).takeWhile(_.start < referenceEnd)
      lap.find(span => !taken.contains(span.start)).map(selectionOf(content, _))
    }

  private def scanFor(content: Rope, search: OccurrenceSearch): Option[FindScan] =
    FindPattern
      .compile(search.text, FindOptions(matchCase = true, wholeWord = search.wholeWord))
      .toOption
      .map(FindScan(content, _))

  private def withCursors(buffer: Buffer, first: Cursor, rest: List[Cursor], search: OccurrenceSearch): Buffer =
    buffer.copy(editing = EditingState(NonEmptyList(first, rest), Some(search)))

  private def selectionOf(content: Rope, span: FindSpan): Selection =
    Selection(positionAt(content, span.start), positionAt(content, span.end))

  private def positionAt(content: Rope, offset: Int): CursorPosition =
    val (line, column) = content.offsetToLineColumn(offset)
    CursorPosition(line, column)

  private def offsetsOf(content: Rope, selection: Selection): (Int, Int) =
    (
      content.lineColumnToOffset(selection.start.line, selection.start.column),
      content.lineColumnToOffset(selection.end.line, selection.end.column)
    )

  private def textOf(content: Rope, selection: Selection): String =
    val (start, end) = offsetsOf(content, selection)
    content.sliceString(start, end)

  /** The run of letters, digits and underscores `position` is inside or touching. */
  private def wordAt(content: Rope, position: CursorPosition): Option[Selection] =
    content.getLine(position.line).flatMap { line =>
      val column = math.min(position.column, line.length)

      @tailrec def wordStart(index: Int): Int =
        if index > 0 && isWordCodePoint(line.codePointBefore(index)) then
          wordStart(index - Character.charCount(line.codePointBefore(index)))
        else index

      @tailrec def wordEnd(index: Int): Int =
        if index < line.length && isWordCodePoint(line.codePointAt(index)) then
          wordEnd(index + Character.charCount(line.codePointAt(index)))
        else index

      val start = wordStart(column)
      val end   = wordEnd(column)
      Option.when(start < end)(Selection(CursorPosition(position.line, start), CursorPosition(position.line, end)))
    }

  private def isWordCodePoint(codePoint: Int): Boolean =
    Character.isLetterOrDigit(codePoint) || codePoint == '_' ||
      (Character.getType(codePoint) match
        case Character.NON_SPACING_MARK | Character.COMBINING_SPACING_MARK => true
        case _                                                             => false)
