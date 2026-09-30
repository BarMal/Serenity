package com.serenity.ui.widget

import com.serenity.text.TextEditing

/** What a text field reports back after an input, beyond its own new state. */
enum TextFieldOutcome:
  case Changed(text: String)
  case Submitted(text: String)
  case Dismissed

/** A single-line text input with a caret and an optional selection, stepping over whole grapheme clusters and words the
  * way the editor itself does. `anchor` is where a selection started; the selection runs from it to `caret`.
  *
  * Offsets are UTF-16 indices into `text`, always on a grapheme boundary.
  */
final case class TextField(text: String = "", caret: Int = 0, anchor: Option[Int] = None):

  def selection: Option[(Int, Int)] =
    anchor.filter(_ != caret).map(start => (math.min(start, caret), math.max(start, caret)))

  def selectedText: String = selection.fold("")((start, end) => text.substring(start, end))

  def update(input: WidgetInput): (TextField, Option[TextFieldOutcome]) =
    input match
      case WidgetInput.Insert(char) if !char.isControl => changed(replacingSelection(char.toString))
      case WidgetInput.InsertText(inserted)            => changed(replacingSelection(singleLine(inserted)))
      case WidgetInput.DeleteBackward                  => changed(deleting(TextEditing.previousGraphemeBoundary))
      case WidgetInput.DeleteForward                   => changed(deleting(TextEditing.nextGraphemeBoundary))
      case WidgetInput.DeleteWordBackward              => changed(deleting(TextEditing.previousWordBoundary))
      case WidgetInput.DeleteWordForward               => changed(deleting(TextEditing.nextWordBoundary))
      case WidgetInput.Left      => (collapsedOr(_._1)(TextEditing.previousGraphemeBoundary(text, caret)), None)
      case WidgetInput.Right     => (collapsedOr(_._2)(TextEditing.nextGraphemeBoundary(text, caret)), None)
      case WidgetInput.WordLeft  => (movedTo(TextEditing.previousWordBoundary(text, caret)), None)
      case WidgetInput.WordRight => (movedTo(TextEditing.nextWordBoundary(text, caret)), None)
      case WidgetInput.First     => (movedTo(0), None)
      case WidgetInput.Last      => (movedTo(text.length), None)
      case WidgetInput.SelectAll => (copy(caret = text.length, anchor = Some(0)), None)
      case WidgetInput.Activate  => (this, Some(TextFieldOutcome.Submitted(text)))
      case WidgetInput.Dismiss   => (this, Some(TextFieldOutcome.Dismissed))
      case _                     => (this, None)

  /** Moves the caret to `offset` (snapped to a grapheme boundary), extending the selection when `extend` is set. */
  def movedTo(offset: Int, extend: Boolean = false): TextField =
    val target = TextEditing.graphemeBoundaryBeforeOrAt(text, offset.max(0).min(text.length))
    copy(caret = target, anchor = if extend then anchor.orElse(Some(caret)) else None)

  private def collapsedOr(edge: ((Int, Int)) => Int)(step: => Int): TextField =
    selection.fold(movedTo(step))(range => movedTo(edge(range)))

  private def replacingSelection(inserted: String): TextField =
    val (start, end) = selection.getOrElse((caret, caret))
    TextField(text.substring(0, start) + inserted + text.substring(end), start + inserted.length, None)

  private def deleting(boundary: (String, Int) => Int): TextField =
    selection match
      case Some(_) => replacingSelection("")
      case None =>
        val other        = boundary(text, caret)
        val (start, end) = (math.min(other, caret), math.max(other, caret))
        TextField(text.substring(0, start) + text.substring(end), start, None)

  private def changed(next: TextField): (TextField, Option[TextFieldOutcome]) =
    (next, Option.when(next.text != text)(TextFieldOutcome.Changed(next.text)))

  private def singleLine(inserted: String): String =
    inserted.linesIterator.mkString(" ")

object TextField:

  def of(text: String): TextField = TextField(text, text.length, None)
