package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tab and Shift+Tab take their unit of indentation from the indentation settings (#1996). */
class EditorIndentationSettingsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateWith(
    indentation: IndentationSettings,
    text: String,
    cursors: List[CursorPosition],
    selection: Option[Selection] = None
  ): AppState =
    val buffer =
      Buffer
        .fromString(bufferId, text)
        .copy(editing = EditingStateFixtures(cursors = cursors, selection = selection))
    val base = AppState.initial
    base.copy(persisted =
      base.persisted.copy(
        buffers = Map(bufferId -> buffer),
        config = base.persisted.config.withIndentation(indentation)
      )
    )

  private def bufferAfter(event: TextEntryEvent, state: AppState): Buffer =
    EditorEventReducer.reduce(event, paneId, state).state.persisted.buffers(bufferId)

  private def textAfter(event: TextEntryEvent, state: AppState): String =
    bufferAfter(event, state).document.content.collect()

  private val twoSpaces = IndentationSettings(tabSize = 2)
  private val tabs      = IndentationSettings(tabSize = 8, insertSpaces = false)

  "TabKey" should "insert tab-size spaces at the cursor" in {
    val after = bufferAfter(TabKey, stateWith(twoSpaces, "ab", List(CursorPosition(0, 1))))

    after.document.content.collect() shouldBe "a  b"
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 3))
  }

  it should "insert one tab character when spaces are off" in {
    val after = bufferAfter(TabKey, stateWith(tabs, "ab", List(CursorPosition(0, 1))))

    after.document.content.collect() shouldBe "a\tb"
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 2))
  }

  it should "insert the unit at every cursor" in {
    val state = stateWith(tabs, "ab\ncd", List(CursorPosition(0, 1), CursorPosition(1, 1)))

    textAfter(TabKey, state) shouldBe "a\tb\nc\td"
  }

  it should "indent each selected line by the unit" in {
    val state = stateWith(
      tabs,
      "one\ntwo\nthree",
      List(CursorPosition(1, 3)),
      Some(Selection(CursorPosition(0, 0), CursorPosition(1, 3)))
    )

    textAfter(TabKey, state) shouldBe "\tone\n\ttwo\nthree"
  }

  it should "indent selected lines by tab-size spaces" in {
    val state = stateWith(
      twoSpaces,
      "one\ntwo",
      List(CursorPosition(1, 3)),
      Some(Selection(CursorPosition(0, 0), CursorPosition(1, 3)))
    )

    textAfter(TabKey, state) shouldBe "  one\n  two"
  }

  "ReverseTabKey" should "remove at most tab-size leading spaces" in {
    val after = bufferAfter(ReverseTabKey, stateWith(twoSpaces, "      abc", List(CursorPosition(0, 9))))

    after.document.content.collect() shouldBe "    abc"
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 7))
  }

  it should "remove up to a wider tab size of leading spaces" in {
    val wide = IndentationSettings(tabSize = 8)

    textAfter(ReverseTabKey, stateWith(wide, "          abc", List(CursorPosition(0, 13)))) shouldBe "  abc"
  }

  it should "remove a leading tab character whatever the tab size" in {
    textAfter(ReverseTabKey, stateWith(tabs, "\t\tabc", List(CursorPosition(0, 5)))) shouldBe "\tabc"
  }
