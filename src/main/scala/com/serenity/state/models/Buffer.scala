package com.serenity.state.models

import java.nio.file.Path

import cats.Order
import cats.data.NonEmptyList
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.{RichTextDocument, RichTextFidelity, RichTextStyle}
import com.serenity.rope.Rope

final case class BufferId(value: Int)

object BufferId:
  given Order[BufferId] = Order.by(_.value)

enum TypographyRole:
  case Code
  case Prose
  case MarkdownSource
  case MarkdownPreview
  case Ui
  case Mixed

  def usesTextFont: Boolean =
    this match
      case Prose | MarkdownSource | MarkdownPreview | Mixed => true
      case Code | Ui                                        => false

/** An anchor/focus pair with an order-independent `start`/`end` and containment check, shared by every buffer range
  * that tracks "where the user started" separately from "where they are now" (`#1053`). Both derive from the same
  * [[CursorPosition]] `Ordering` that `DocumentNavigation` compares positions with (`#1065`).
  */
trait DirectedRange:
  def anchor: CursorPosition
  def focus: CursorPosition

  def start: CursorPosition =
    if summon[Ordering[CursorPosition]].lteq(anchor, focus) then anchor else focus

  def end: CursorPosition =
    if start == anchor then focus else anchor

  def contains(cursor: CursorPosition): Boolean =
    val ordering = summon[Ordering[CursorPosition]]
    ordering.lteq(start, cursor) && ordering.lteq(cursor, end)

final case class Selection(anchor: CursorPosition, focus: CursorPosition) extends DirectedRange

final case class DocumentComment(anchor: CursorPosition, focus: CursorPosition, text: String) extends DirectedRange

final case class VerticalCursorState(cursor: CursorPosition, preferredColumn: Int, preferredXPx: Float)

/** A buffer's on-disk identity and content -- what makes it "this file", independent of how it's being edited or
  * displayed. Split out by #1002 so `Buffer` itself no longer spans unrelated subdomains.
  */
final case class Document(
    content: Rope,
    filePath: Option[Path] = None,
    isDirty: Boolean = false,
    language: Option[LanguageId] = None,
    isNewEmpty: Boolean = false
)

/** A buffer's cursor/selection state. `Buffer.cursorList`/`withCursorList` convert this to and from the uniform
  * per-cursor [[Cursor]] shape the reducer operates over.
  */
final case class EditingState(
    cursors: List[CursorPosition] = List(CursorPosition(0, 0)),
    selection: Option[Selection] = None,
    selections: List[Selection] = Nil,
    preferredColumn: Option[Int] = None,
    preferredXPx: Option[Float] = None,
    multiCursorVerticalStates: List[VerticalCursorState] = Nil
)

/** User-authored markers anchored to buffer positions, independent of the document's own content. */
final case class Annotations(
    bookmarks: List[CursorPosition] = Nil,
    documentComments: List[DocumentComment] = Nil
)

/** Rich-text authoring state layered on top of the buffer's plain-text `Rope` content. */
final case class RichTextState(
    richTextDocument: Option[RichTextDocument] = None,
    richTextFidelity: Option[RichTextFidelity] = None,
    insertionRichTextStyle: Option[RichTextStyle] = None
)

final case class Buffer(
    id: BufferId,
    document: Document,
    editing: EditingState = EditingState(),
    viewport: Viewport = Viewport.default,
    findState: Option[FindState] = None,
    annotations: Annotations = Annotations(),
    richText: RichTextState = RichTextState(),
    /** Bumped synchronously whenever an edit lands on this buffer while it has a live markdown preview. Compared
      * against `markdownPreviewCommittedGeneration` to tell the renderer whether an edit burst is still in flight.
      */
    markdownPreviewEditGeneration: Long = 0L,
    /** Set by a debounced, cancelable job ~150ms after the edit burst that produced `markdownPreviewEditGeneration`
      * settles. While the two differ, the renderer reuses its last markdown preview image instead of re-running the
      * expensive HTML/CSS layout pass on every keystroke.
      */
    markdownPreviewCommittedGeneration: Long = 0L
):

  def typographyRole: TypographyRole =
    document.language match
      case None                      => TypographyRole.Prose
      case Some(LanguageId.Markdown) => TypographyRole.MarkdownSource
      case Some(_)                   => TypographyRole.Code

  def usesTextFont: Boolean =
    typographyRole.usesTextFont

  def allSelections: List[Selection] =
    if editing.selections.nonEmpty then editing.selections else editing.selection.toList

  def primarySelection: Option[Selection] =
    allSelections.headOption

  def clearSelections: Buffer =
    copy(editing = editing.copy(selection = None, selections = Nil))

  /** This buffer's cursors as one uniform list, converting `selections`/`selection`/`multiCursorVerticalStates` into
    * each cursor's own optional selection anchor and preferred vertical-navigation state -- see [[Cursor]]. Order and
    * membership exactly mirror `cursors`/`allSelections`; nothing is sorted or deduplicated here.
    */
  def cursorList: NonEmptyList[Cursor] =
    if allSelections.nonEmpty then NonEmptyList.fromListUnsafe(allSelections.map(Cursor(_)))
    else if editing.cursors.sizeIs > 1 then
      NonEmptyList.fromListUnsafe(editing.cursors.map { position =>
        editing.multiCursorVerticalStates.find(_.cursor == position) match
          case Some(state) => Cursor(position, None, Some(state.preferredColumn), Some(state.preferredXPx))
          case None        => Cursor(position)
      })
    else
      NonEmptyList.one(
        Cursor(
          editing.cursors.headOption.getOrElse(CursorPosition(0, 0)),
          None,
          editing.preferredColumn,
          editing.preferredXPx
        )
      )

  /** The inverse of [[cursorList]]: repackages a cursor list back into this buffer's five cursor-shaped fields, leaving
    * everything else untouched. Does not sort or deduplicate -- callers hand back the list in the order and membership
    * they want stored, exactly as `cursorList` handed it to them.
    */
  def withCursorList(updated: NonEmptyList[Cursor]): Buffer =
    val list = updated.toList
    (list.exists(_.selectionAnchor.isDefined), list) match
      case (true, cursor :: Nil) =>
        copy(editing =
          EditingState(
            cursors = List(cursor.position),
            selection = cursor.selection,
            selections = Nil,
            preferredColumn = None,
            preferredXPx = None,
            multiCursorVerticalStates = Nil
          )
        )
      case (true, many) =>
        copy(editing =
          EditingState(
            cursors = many.map(_.position),
            selection = None,
            selections = many.map(cursor => cursor.selection.getOrElse(Selection(cursor.position, cursor.position))),
            preferredColumn = None,
            preferredXPx = None,
            multiCursorVerticalStates = Nil
          )
        )
      case (false, cursor :: Nil) =>
        copy(editing =
          EditingState(
            cursors = List(cursor.position),
            selection = None,
            selections = Nil,
            preferredColumn = cursor.preferredColumn,
            preferredXPx = cursor.preferredXPx,
            multiCursorVerticalStates = Nil
          )
        )
      case (false, many) =>
        copy(editing =
          EditingState(
            cursors = many.map(_.position),
            selection = None,
            selections = Nil,
            preferredColumn = None,
            preferredXPx = None,
            multiCursorVerticalStates = many.flatMap { cursor =>
              cursor.preferredColumn.map(column =>
                VerticalCursorState(cursor.position, column, cursor.preferredXPx.getOrElse(0f))
              )
            }
          )
        )

  /** True when closing this buffer may lose user-authored content. */
  def hasUnsavedChanges: Boolean =
    document.isDirty || (document.filePath.isEmpty && !document.isNewEmpty)

  /** The buffer state after an edit lands: swaps in the new content, marks the document dirty, replaces the cursor list
    * while clearing selection state, resets preferred-column/X tracking to the new primary cursor, and drops stale
    * multi-cursor vertical state. `documentComments` and `richTextDocument` default to their current, unadjusted values
    * -- pass the caller's remapped ones when the edit needs to carry them forward.
    *
    * Centralises the five near-identical post-edit `copy` blocks in `EditorEventReducer` (`#1072`), which had already
    * drifted: the merged-deletion path silently kept a stale `richTextDocument` (and stale `multiCursorVerticalStates`)
    * that every other edit path cleared or updated. Both are now handled uniformly.
    */
  def withEditedContent(
    content: Rope,
    cursors: List[CursorPosition],
    documentComments: List[DocumentComment] = annotations.documentComments,
    richTextDocument: Option[RichTextDocument] = richText.richTextDocument
  ): Buffer =
    copy(
      document = document.copy(content = content, isDirty = true, isNewEmpty = false),
      editing = editing.copy(
        cursors = cursors,
        selection = None,
        selections = Nil,
        preferredColumn = Some(cursors.primaryCursor.column),
        preferredXPx = None,
        multiCursorVerticalStates = Nil
      ),
      annotations = annotations.copy(documentComments = documentComments),
      richText = richText.copy(richTextDocument = richTextDocument)
    )

object Buffer:
  def empty(id: BufferId)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Document(Rope.empty))

  def newEmpty(id: BufferId)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Document(Rope.empty, isNewEmpty = true))

  def fromString(id: BufferId, content: String)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Document(Rope(content)))

  def fromFile(id: BufferId, path: Path, content: String)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Document(Rope(content), filePath = Some(path)))
