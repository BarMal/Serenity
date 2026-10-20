package com.serenity.input

import java.awt.event.InputMethodEvent
import java.awt.font.TextAttribute
import java.text.AttributedCharacterIterator

import scala.annotation.tailrec

import com.serenity.keystroke.events.{Event, ImeCancelled, ImeCommit, ImeComposing}
import com.serenity.state.models.CompositionClause

/** An AWT `InputMethodEvent` carries a committed prefix followed by the pre-edit, so one event can both commit and keep
  * composing. It becomes the editor's events in that order: the commit first, then the composition that goes on from it
  * (#2008).
  */
object ImeEventTranslation:

  def fromInputMethodEvent(event: InputMethodEvent): List[Event] =
    if event.getID != InputMethodEvent.INPUT_METHOD_TEXT_CHANGED then Nil
    else
      val text      = Option(event.getText)
      val all       = text.fold("")(characters)
      val committed = event.getCommittedCharacterCount.max(0).min(all.length)
      val composed  = all.substring(committed)
      val commit    = Option.when(committed > 0)(ImeCommit(all.substring(0, committed)))
      val composing = text.filter(_ => composed.nonEmpty).map { iterator =>
        ImeComposing(composed, clausesOf(iterator, committed), caretIn(event, committed, composed.length))
      }
      if commit.isEmpty && composing.isEmpty then List(ImeCancelled) else commit.toList ++ composing.toList

  private def characters(text: AttributedCharacterIterator): String =
    (text.getBeginIndex until text.getEndIndex).map { index =>
      text.setIndex(index)
      text.current
    }.mkString

  private def caretIn(event: InputMethodEvent, committed: Int, length: Int): Option[Int] =
    Option(event.getCaret).map(hit => (hit.getInsertionIndex - committed).max(0).min(length))

  private def clausesOf(text: AttributedCharacterIterator, from: Int): List[CompositionClause] =
    @tailrec def loop(index: Int, acc: List[CompositionClause]): List[CompositionClause] =
      if index >= text.getEndIndex then acc.reverse
      else
        text.setIndex(index)
        val limit = text.getRunLimit.max(index + 1).min(text.getEndIndex)
        val selected =
          Option(text.getAttribute(TextAttribute.INPUT_METHOD_HIGHLIGHT)).flatMap(ImeDiagnostics.highlightOf)
        loop(limit, CompositionClause(index - from, limit - from, selected.exists(_.isSelected)) :: acc)
    loop(from, Nil)
