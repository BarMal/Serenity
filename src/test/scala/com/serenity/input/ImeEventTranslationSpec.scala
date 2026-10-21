package com.serenity.input

import java.awt.event.InputMethodEvent
import java.awt.font.{TextAttribute, TextHitInfo}
import java.awt.im.InputMethodHighlight
import java.text.AttributedString
import javax.swing.JPanel

import com.serenity.keystroke.events.{ImeCancelled, ImeCommit, ImeComposing}
import com.serenity.state.models.CompositionClause
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How an AWT `InputMethodEvent`, whose text is a committed prefix followed by the pre-edit, becomes the editor's
  * events (#2008).
  */
class ImeEventTranslationSpec extends AnyFlatSpec with Matchers:

  private val Source = new JPanel

  private def event(
    text: String,
    committed: Int,
    highlights: List[(Int, Int, Boolean)] = Nil,
    caret: Option[Int] = None
  ): InputMethodEvent =
    val attributed = new AttributedString(text)
    highlights.foreach { (start, end, selected) =>
      attributed.addAttribute(
        TextAttribute.INPUT_METHOD_HIGHLIGHT,
        new InputMethodHighlight(selected, InputMethodHighlight.CONVERTED_TEXT),
        start,
        end
      )
    }
    new InputMethodEvent(
      Source,
      InputMethodEvent.INPUT_METHOD_TEXT_CHANGED,
      1L,
      if text.isEmpty then null else attributed.getIterator,
      committed,
      caret.map(TextHitInfo.leading).orNull,
      null
    )

  "ImeEventTranslation" should "turn pre-edit text into ImeComposing with its clauses and caret" in {
    val translated = ImeEventTranslation.fromInputMethodEvent(
      event("かんじ", committed = 0, highlights = List((0, 2, true), (2, 3, false)), caret = Some(2))
    )

    translated shouldBe List(
      ImeComposing(
        "かんじ",
        List(CompositionClause(0, 2, selected = true), CompositionClause(2, 3, selected = false)),
        Some(2)
      )
    )
  }

  it should "treat pre-edit text with no highlight as one unselected clause" in {
    ImeEventTranslation.fromInputMethodEvent(event("かな", committed = 0)) shouldBe
      List(ImeComposing("かな", List(CompositionClause(0, 2, selected = false)), None))
  }

  it should "turn committed text alone into ImeCommit" in {
    ImeEventTranslation.fromInputMethodEvent(event("漢字", committed = 2)) shouldBe List(ImeCommit("漢字"))
  }

  it should "commit the prefix and then compose the rest, with clauses and caret relative to the rest" in {
    val translated = ImeEventTranslation.fromInputMethodEvent(
      event("漢字かな", committed = 2, highlights = List((2, 4, true)), caret = Some(3))
    )

    translated shouldBe List(
      ImeCommit("漢字"),
      ImeComposing("かな", List(CompositionClause(0, 2, selected = true)), Some(1))
    )
  }

  it should "keep a code point outside the BMP whole in a commit" in {
    ImeEventTranslation.fromInputMethodEvent(event("𠮷", committed = 2)) shouldBe
      List(ImeCommit("𠮷"))
  }

  it should "turn an event with no text and nothing committed into ImeCancelled" in {
    ImeEventTranslation.fromInputMethodEvent(event("", committed = 0)) shouldBe List(ImeCancelled)
  }

  it should "say nothing for a caret-only event, which carries no text" in {
    val caretOnly = new InputMethodEvent(Source, InputMethodEvent.CARET_POSITION_CHANGED, TextHitInfo.leading(1), null)

    ImeEventTranslation.fromInputMethodEvent(caretOnly) shouldBe Nil
  }
