package com.serenity.state.models

import java.nio.file.Path

import cats.Order
import com.serenity.animation.AnimationState
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.{RichTextDocument, RichTextFidelity, RichTextStyle}
import com.serenity.rope.Rope

case class BufferId(value: Int)

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

case class Selection(anchor: CursorPosition, focus: CursorPosition):

  def start: CursorPosition =
    if anchor.line < focus.line || (anchor.line == focus.line && anchor.column <= focus.column) then anchor
    else focus

  def end: CursorPosition =
    if start == anchor then focus else anchor

case class DocumentComment(anchor: CursorPosition, focus: CursorPosition, text: String):

  def start: CursorPosition =
    if anchor.line < focus.line || (anchor.line == focus.line && anchor.column <= focus.column) then anchor
    else focus

  def end: CursorPosition =
    if start == anchor then focus else anchor

  def contains(cursor: CursorPosition): Boolean =
    val afterStart =
      cursor.line > start.line || (cursor.line == start.line && cursor.column >= start.column)
    val beforeEnd =
      cursor.line < end.line || (cursor.line == end.line && cursor.column <= end.column)
    afterStart && beforeEnd

case class VerticalCursorState(cursor: CursorPosition, preferredColumn: Int, preferredXPx: Float)

case class Buffer(
    id: BufferId,
    content: Rope,
    filePath: Option[Path] = None,
    isDirty: Boolean = false,
    language: Option[LanguageId] = None,
    isNewEmpty: Boolean = false,
    animations: AnimationState = AnimationState.empty,
    cursors: List[CursorPosition] = List(CursorPosition(0, 0)),
    selection: Option[Selection] = None,
    preferredColumn: Option[Int] = None,
    preferredXPx: Option[Float] = None,
    multiCursorVerticalStates: List[VerticalCursorState] = Nil,
    viewport: Viewport = Viewport.default,
    findState: Option[FindState] = None,
    selections: List[Selection] = Nil,
    bookmarks: List[CursorPosition] = Nil,
    documentComments: List[DocumentComment] = Nil,
    richTextDocument: Option[RichTextDocument] = None,
    richTextFidelity: Option[RichTextFidelity] = None,
    insertionRichTextStyle: Option[RichTextStyle] = None,
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
    language match
      case None                      => TypographyRole.Prose
      case Some(LanguageId.Markdown) => TypographyRole.MarkdownSource
      case Some(_)                   => TypographyRole.Code

  def usesTextFont: Boolean =
    typographyRole.usesTextFont

  def allSelections: List[Selection] =
    if selections.nonEmpty then selections else selection.toList

  def primarySelection: Option[Selection] =
    allSelections.headOption

  def clearSelections: Buffer =
    copy(selection = None, selections = Nil)

  /** True when closing this buffer may lose user-authored content. */
  def hasUnsavedChanges: Boolean =
    isDirty || (filePath.isEmpty && !isNewEmpty)

  // The compiler-generated equals walks every one of this class's ~19 fields with no fast path -- paid once per
  // open buffer on every dispatched event via AppState's buffers map. Short-circuiting on reference identity
  // covers the common "this buffer wasn't touched by this event" case in O(1). Comparing via productIterator
  // rather than hand-listing fields avoids the risk of silently dropping a field from the comparison as the case
  // class evolves.
  override def equals(obj: Any): Boolean =
    obj match
      case that: AnyRef if this.asInstanceOf[AnyRef].eq(that) => true
      case that: Buffer                                       => productIterator.sameElements(that.productIterator)
      case _                                                  => false

object Buffer:
  def empty(id: BufferId)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Rope.empty)

  def newEmpty(id: BufferId)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Rope.empty, isNewEmpty = true)

  def fromString(id: BufferId, content: String)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Rope(content))

  def fromFile(id: BufferId, path: Path, content: String)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Rope(content), Some(path))
