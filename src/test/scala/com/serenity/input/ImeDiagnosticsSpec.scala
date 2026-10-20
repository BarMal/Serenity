package com.serenity.input

import java.awt.event.{InputMethodEvent, KeyEvent}
import java.awt.font.TextHitInfo
import java.awt.im.InputMethodHighlight
import java.text.AttributedString
import javax.swing.JPanel

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The opt-in IME evidence log (issue #2008): what it reads from a flag, what it records for each event, and the
  * minimal `InputMethodRequests` it installs.
  */
class ImeDiagnosticsSpec extends AnyFlatSpec with Matchers:

  private val Source = new JPanel

  private def composing(text: String, committed: Int, selected: Boolean, caret: Option[TextHitInfo]): InputMethodEvent =
    val attributed = new AttributedString(text)
    if text.length > committed then
      attributed.addAttribute(
        java.awt.font.TextAttribute.INPUT_METHOD_HIGHLIGHT,
        new InputMethodHighlight(selected, InputMethodHighlight.CONVERTED_TEXT),
        committed,
        text.length
      )
    new InputMethodEvent(
      Source,
      InputMethodEvent.INPUT_METHOD_TEXT_CHANGED,
      7L,
      attributed.getIterator,
      committed,
      caret.orNull,
      null
    )

  "ImeDiagnostics.enabled" should "be off when the variable is absent" in {
    ImeDiagnostics.enabled(Map.empty) shouldBe false
  }

  it should "be on for 1 and true and off for anything else" in {
    ImeDiagnostics.enabled(Map(ImeDiagnostics.EnvVar -> "1")) shouldBe true
    ImeDiagnostics.enabled(Map(ImeDiagnostics.EnvVar -> "true")) shouldBe true
    ImeDiagnostics.enabled(Map(ImeDiagnostics.EnvVar -> "0")) shouldBe false
    ImeDiagnostics.enabled(Map(ImeDiagnostics.EnvVar -> "")) shouldBe false
  }

  "ImeDiagnostics.describe(InputMethodEvent)" should "record id, committed count, highlight state, caret and time" in {
    val line = ImeDiagnostics.describe(
      composing("abあい", committed = 2, selected = true, Some(TextHitInfo.leading(3))),
      nowMillis = 1234L
    )
    line should include("t=1234")
    line should include("IME id=INPUT_METHOD_TEXT_CHANGED")
    line should include("committed=2")
    line should include("length=4")
    line should include("[2,4) CONVERTED selected")
    line should include("caret=3 leading")
  }

  it should "record a committed-only event with no caret" in {
    val line = ImeDiagnostics.describe(composing("ab", committed = 2, selected = false, None), nowMillis = 1L)
    line should include("committed=2")
    line should include("caret=none")
    line should not include "CONVERTED"
  }

  it should "report an event with no text as empty" in {
    val event = new InputMethodEvent(Source, InputMethodEvent.CARET_POSITION_CHANGED, TextHitInfo.trailing(0), null)
    val line  = ImeDiagnostics.describe(event, nowMillis = 1L)
    line should include("IME id=CARET_POSITION_CHANGED")
    line should include("length=0")
    line should include("caret=0 trailing")
  }

  it should "log code points, so astral characters show as one" in {
    val line = ImeDiagnostics.describe(composing("𠮷", committed = 2, selected = false, None), nowMillis = 1L)
    line should include("codePoints=[20BB7]")
  }

  "ImeDiagnostics.describe(KeyEvent)" should "record KEY_TYPED with its char as a code point" in {
    val line = ImeDiagnostics.describe(KeyEvent(Source, KeyEvent.KEY_TYPED, 2L, 0, KeyEvent.VK_UNDEFINED, 'あ'), 99L)
    line should include("t=99")
    line should include("KEY_TYPED")
    line should include("char=3042")
  }

  "ImeDiagnostics.requests" should "answer with an empty document and no committed text to cancel" in {
    val requests = ImeDiagnostics.requests(new JPanel, _ => ())
    requests.getCommittedTextLength shouldBe 0
    requests.getInsertPositionOffset shouldBe 0
    requests.cancelLatestCommittedText(null) shouldBe null
    requests.getSelectedText(null).getEndIndex shouldBe 0
    requests.getCommittedText(0, 0, null).getEndIndex shouldBe 0
  }

  it should "give a text location even when the component is not on screen" in {
    ImeDiagnostics.requests(new JPanel, _ => ()).getTextLocation(null) should not be null
  }

  it should "log each request it answers" in {
    val seen = scala.collection.mutable.ListBuffer.empty[String]
    ImeDiagnostics.requests(new JPanel, seen += _).getTextLocation(null)
    seen.toList.exists(_.contains("getTextLocation")) shouldBe true
  }
