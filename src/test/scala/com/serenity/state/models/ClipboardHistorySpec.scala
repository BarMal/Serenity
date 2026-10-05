package com.serenity.state.models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ClipboardHistorySpec extends AnyFlatSpec with Matchers:

  private def selection(text: String): ClipboardEntry = ClipboardEntry(text, wholeLine = false)
  private def line(text: String): ClipboardEntry      = ClipboardEntry(text, wholeLine = true)

  "ClipboardHistory.recorded" should "keep the newest entries first, dropping the oldest past its capacity" in {
    val texts   = (1 to ClipboardHistory.Capacity + 5).map(n => s"copy $n")
    val history = texts.foldLeft(ClipboardHistory.empty)((history, text) => history.recorded(selection(text)))

    history.entries should have size ClipboardHistory.Capacity
    history.entries.headOption shouldBe Some(selection(s"copy ${ClipboardHistory.Capacity + 5}"))
    history.entries.lastOption shouldBe Some(selection("copy 6"))
  }

  it should "move a repeated text to the front instead of keeping it twice, in its newest shape" in {
    val history =
      ClipboardHistory.empty.recorded(line("alpha")).recorded(selection("beta")).recorded(selection("alpha"))

    history.entries shouldBe List(selection("alpha"), selection("beta"))
  }

  it should "keep an empty text only as a whole-line copy" in {
    ClipboardHistory.empty.recorded(selection("")).entries shouldBe Nil
    ClipboardHistory.empty.recorded(line("")).entries shouldBe List(line(""))
  }

  "ClipboardHistory.imported" should "leave Serenity's own newest copy alone, keeping its whole-line shape" in {
    val history = ClipboardHistory.empty.recorded(line("alpha"))

    history.imported("alpha") shouldBe history
    history.imported("alpha").entryFor("alpha") shouldBe line("alpha")
  }

  it should "record text copied elsewhere as a plain entry" in {
    val history = ClipboardHistory.empty.recorded(line("alpha")).imported("from another app")

    history.entries shouldBe List(selection("from another app"), line("alpha"))
    history.entryFor("from another app") shouldBe selection("from another app")
  }

  "ClipboardHistory.entryFor" should "paste text as lines only while it is the newest whole-line copy" in {
    val history = ClipboardHistory.empty.recorded(line("alpha")).recorded(selection("beta"))

    history.entryFor("beta") shouldBe selection("beta")
    history.entryFor("alpha") shouldBe selection("alpha")
  }
