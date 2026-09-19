package com.serenity.state.models

import java.nio.file.Path

import cats.Order
import cats.data.NonEmptyList
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.{RichTextDocument, RichTextFidelity, RichTextStyle}
import com.serenity.rope.Rope

opaque type BufferId = Int

object BufferId:
  def apply(value: Int): BufferId      = value
  def unapply(id: BufferId): Some[Int] = Some(id)

  extension (id: BufferId) def value: Int = id

  given Order[BufferId] = Order.by(identity)

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

/** A buffer's cursor/selection state: one entry per live cursor, each carrying its own position, in-flight selection
  * anchor and preferred vertical-navigation column/pixel-x (`#1577`). Before `#1577` this was five separate parallel
  * lists/options (`cursors`, `selection`, `selections`, `preferredColumn`, `preferredXPx`,
  * `multiCursorVerticalStates`), which let a single cursor's own state drift across collections that had to be kept in
  * sync by hand; `NonEmptyList[Cursor]` makes that drift impossible by construction.
  */
final case class EditingState(cursors: NonEmptyList[Cursor] = NonEmptyList.one(Cursor(CursorPosition(0, 0)))):
  def cursorPositions: List[CursorPosition] = cursors.toList.map(_.position)

  /** Replaces the primary (first) cursor's full state while leaving every other live cursor untouched. */
  def withPrimary(cursor: Cursor): EditingState = EditingState(NonEmptyList(cursor, cursors.tail))

object EditingState:

  /** Bare cursor positions, with no selection or preferred-column/x state -- the shape session restore and most
    * post-edit cursor placement need.
    */
  def apply(positions: List[CursorPosition]): EditingState =
    NonEmptyList.fromList(positions) match
      case Some(nel) => EditingState(nel.map(Cursor(_)))
      case None      => EditingState()

  def fromCursors(cursors: List[Cursor]): EditingState =
    NonEmptyList.fromList(cursors) match
      case Some(nel) => EditingState(nel)
      case None      => EditingState()

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
    editing.cursors.toList.flatMap(_.selection)

  def primarySelection: Option[Selection] =
    editing.cursors.head.selection

  def clearSelections: Buffer =
    copy(editing = EditingState(editing.cursors.map(_.copy(selectionAnchor = None))))

  /** This buffer's cursors as one uniform list -- trivial now that `EditingState` itself stores cursors this way
    * (`#1577`); kept as a named entry point since `EditorEventReducer`/`EditorNavigationEventReducer` document their
    * whole dispatch in terms of it.
    */
  def cursorList: NonEmptyList[Cursor] = editing.cursors

  /** The inverse of [[cursorList]]: stores a cursor list back as this buffer's editing state. */
  def withCursorList(updated: NonEmptyList[Cursor]): Buffer =
    copy(editing = EditingState(updated))

  /** True when closing this buffer may lose user-authored content. */
  def hasUnsavedChanges: Boolean =
    document.isDirty || (document.filePath.isEmpty && !document.isNewEmpty)

  /** The buffer state after an edit lands: swaps in the new content, marks the document dirty, and replaces the cursor
    * list with bare positions, clearing every cursor's selection and preferred-column/x state. `documentComments` and
    * `richTextDocument` default to their current, unadjusted values -- pass the caller's remapped ones when the edit
    * needs to carry them forward.
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
      editing = EditingState(cursors),
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
