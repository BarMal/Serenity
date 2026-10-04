package com.serenity.state.models

import java.nio.file.Path

import cats.Order
import cats.data.NonEmptyList
import com.serenity.io.{DocumentFormat, DocumentRevision, FileType}
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.{RichTextDocument, RichTextFidelity, RichTextStyle}
import com.serenity.rope.Rope
import com.serenity.text.{LineEnding, TextEncoding}

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

/** A to-do marker at a single buffer position, carrying a short note about what still needs writing there -- Neo's
  * "placeholder" QoL feature: drop a mark and a sticky note, keep writing, come back and resolve it later.
  */
final case class Placeholder(position: CursorPosition, note: String)

/** A beautiful-but-in-the-way passage cut from the manuscript rather than deleted outright -- Neo's "darlings" QoL
  * feature. `originalPosition` records where it was cut from, but restoring inserts at the cursor rather than trying to
  * reopen that exact spot: the document may well have changed shape since the cut.
  */
final case class Darling(text: String, originalPosition: CursorPosition)

/** A buffer's on-disk identity and content -- what makes it "this file", independent of how it's being edited or
  * displayed. Split out by #1002 so `Buffer` itself no longer spans unrelated subdomains.
  */
final case class Document(
    content: Rope,
    filePath: Option[Path] = None,
    isDirty: Boolean = false,
    language: Option[LanguageId] = None,
    isNewEmpty: Boolean = false,
    // Recorded on load so saving can reproduce the file it came from; `Rope` has already normalised the content
    // itself to LF by the time it reaches here.
    lineEnding: LineEnding = LineEnding.default,
    // Recorded on load for the same reason as `lineEnding` (#1627); a BOM is stripped from `content` and only
    // `hasBom` remembers it.
    encoding: TextEncoding = TextEncoding.default,
    hasBom: Boolean = false,
    // Captured from `DocumentStorageProvider` on every successful open and save (#1623), so a later save or
    // focus-in re-check can tell whether the on-disk file changed underneath this buffer since it was last
    // read, rather than only detecting a stale write after silently overwriting external changes.
    revision: Option[DocumentRevision] = None,
    // Bumped on every `content` change (`withContent`, #1663). Paired with `RichTextState.richTextSyncedVersion`
    // so `Buffer.richTextInSync` can tell in O(1) whether a stored `richTextDocument` still describes this
    // `content`, instead of re-deriving and comparing the whole plain text on every check.
    contentVersion: Long = 0L
):
  /** The only sanctioned way to change `content`: keeps `contentVersion` monotonically increasing so a
    * `richTextDocument` stamped against the old version is correctly seen as stale by `Buffer.richTextInSync`, without
    * re-comparing any text.
    */
  def withContent(newContent: Rope): Document =
    copy(content = newContent, contentVersion = contentVersion + 1, isDirty = true, isNewEmpty = false)

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
    documentComments: List[DocumentComment] = Nil,
    placeholders: List[Placeholder] = Nil,
    darlings: List[Darling] = Nil,
    notes: Map[NoteKey, Notes] = Map.empty
)

/** Rich-text authoring state layered on top of the buffer's plain-text `Rope` content. */
final case class RichTextState(
    richTextDocument: Option[RichTextDocument] = None,
    richTextFidelity: Option[RichTextFidelity] = None,
    insertionRichTextStyle: Option[RichTextStyle] = None,
    // The `Document.contentVersion` `richTextDocument` is known to match, or `None` if it either isn't set or
    // wasn't stamped as verified against the buffer's current content (#1663). `None` is always the safe default:
    // every reader that once re-derived and string-compared the whole plain text now instead treats an unstamped
    // document as stale and rebuilds it, exactly as it would have on a genuine mismatch.
    richTextSyncedVersion: Option[Long] = None
):
  /** Attaches `document` (or clears it, via `None`) as the paragraph-shaped view of a buffer whose content is at
    * `contentVersion`, stamping the sync version alongside it so `Buffer.richTextInSync` can trust the pairing without
    * re-checking any text. This is the one place that sets `richTextDocument` and `richTextSyncedVersion` together, so
    * every call site attaching a document (freshly built, or carried forward already verified) goes through the same
    * pairing instead of risking a `richTextDocument` stamped with a stale or missing version.
    */
  def withSyncedDocument(document: Option[RichTextDocument], contentVersion: Long): RichTextState =
    copy(richTextDocument = document, richTextSyncedVersion = document.map(_ => contentVersion))

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
    markdownPreviewCommittedGeneration: Long = 0L,
    /** A hidden buffer holds text the user authors without being a document of its own -- a chapter note. It lives in
      * `Persisted.buffers` so an editor pane can show and edit it, but is never listed as an open document: it is not
      * in `bufferOrder`, never prompts to be saved, and is not searched with the project's files.
      */
    hidden: Boolean = false
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

  /** Moves cursors and selection ends past the last line onto it, and drops bookmarks, comments and placeholders past
    * it -- for content replaced from disk (a reload, or a session restore that prefers the disk), which may be shorter
    * than the positions recorded against the old content.
    */
  def clampedToContent: Buffer =
    val lineCount                         = document.content.lineCount
    def inRange(position: CursorPosition) = position.line < lineCount
    def clamp(position: CursorPosition) =
      if inRange(position) then position
      else
        val lastLine = (lineCount - 1).max(0)
        CursorPosition(lastLine, document.content.getLine(lastLine).fold(0)(_.length))
    copy(
      editing = EditingState(
        editing.cursors.map(cursor =>
          cursor.copy(position = clamp(cursor.position), selectionAnchor = cursor.selectionAnchor.map(clamp))
        )
      ),
      annotations = annotations.copy(
        bookmarks = annotations.bookmarks.filter(inRange),
        documentComments =
          annotations.documentComments.filter(comment => inRange(comment.anchor) && inRange(comment.focus)),
        placeholders = annotations.placeholders.filter(placeholder => inRange(placeholder.position))
      )
    )

  /** True when closing this buffer may lose user-authored content. A hidden buffer is never closed on its own, and the
    * session keeps its text, so it never asks to be saved.
    */
  def hasUnsavedChanges: Boolean =
    !hidden && (document.isDirty || (document.filePath.isEmpty && !document.isNewEmpty))

  /** True when saving to this buffer's own file would silently drop its formatting. An untitled buffer has no format
    * yet, so its save (always a Save As) is judged against the path chosen then instead.
    */
  def formattingLostOnSave: Boolean =
    document.filePath.exists(path =>
      DocumentFormat.wouldLoseFormatting(richText.richTextDocument.exists(_.hasFormatting), FileType.fromPath(path))
    )

  /** The buffer state after an edit lands: swaps in the new content, marks the document dirty, and replaces the cursor
    * list with bare positions, clearing every cursor's selection and preferred-column/x state. `adjustedAnnotations`
    * and `richTextDocument` default to their current, unadjusted values -- pass the caller's remapped ones when the
    * edit needs to carry them forward. Every real caller does pass an explicit `richTextDocument` (`None` when there is
    * none, or the result of re-deriving it against the new `content`); the default exists for a caller with no rich
    * text to carry, so it stamps whatever `richTextDocument` it ends up with as synced to the *new* content version --
    * a caller relying on the default while genuinely changing content on a buffer that has a `richTextDocument` would
    * wrongly mark that unrelated-to-this-edit document as still matching, exactly the drift `#1663` moved away from
    * checking by full-text comparison. Only safe when the default is left untouched by every caller, as it is.
    *
    * Centralises the five near-identical post-edit `copy` blocks in `EditorEventReducer` (`#1072`), which had already
    * drifted: the merged-deletion path silently kept a stale `richTextDocument` (and stale `multiCursorVerticalStates`)
    * that every other edit path cleared or updated. Both are now handled uniformly.
    */
  def withEditedContent(
    content: Rope,
    cursors: List[CursorPosition],
    adjustedAnnotations: Annotations = annotations,
    richTextDocument: Option[RichTextDocument] = richText.richTextDocument
  ): Buffer =
    val updatedDocument = document.withContent(content)
    copy(
      document = updatedDocument,
      editing = EditingState(cursors),
      annotations = adjustedAnnotations,
      richText = richText.withSyncedDocument(richTextDocument, updatedDocument.contentVersion)
    )

  /** `O(1)`: whether `richText.richTextDocument` is known to describe `document.content` exactly, replacing a
    * `matchesPlainText` re-comparison of the whole plain text on every check (`#1663`). Relies on every writer of
    * `richTextDocument` going through [[RichTextState.withSyncedDocument]] (directly, or via [[withEditedContent]]) so
    * the stamped version and `document.contentVersion` only ever agree when the pairing is actually still valid.
    */
  def richTextInSync: Boolean =
    richText.richTextDocument.isDefined && richText.richTextSyncedVersion.contains(document.contentVersion)

object Buffer:
  def empty(id: BufferId)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Document(Rope.empty))

  def newEmpty(id: BufferId)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Document(Rope.empty, isNewEmpty = true))

  def fromString(id: BufferId, content: String)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Document(Rope(content)))

  def fromFile(id: BufferId, path: Path, content: String)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Document(Rope(content), filePath = Some(path)))
