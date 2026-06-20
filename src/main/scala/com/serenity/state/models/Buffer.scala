package com.serenity.state.models

import java.nio.file.Path

import cats.Order
import com.serenity.animation.AnimationState
import com.serenity.lsp.config.LanguageId
import com.serenity.richtext.RichTextDocument
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
    richTextDocument: Option[RichTextDocument] = None
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

object Buffer:
  def empty(id: BufferId)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Rope.empty)

  def newEmpty(id: BufferId)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Rope.empty, isNewEmpty = true)

  def fromString(id: BufferId, content: String)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Rope(content))

  def fromFile(id: BufferId, path: Path, content: String)(using com.serenity.rope.Balance): Buffer =
    Buffer(id, Rope(content), Some(path))
