package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tab and Shift+Tab indent by the buffer's own indentation, falling back to the global settings (#1996). */
class DocumentIndentationEditingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private val global = IndentationSettings(tabSize = 4)

  private def stateWith(
    indentation: DocumentIndentation,
    text: String,
    cursors: List[CursorPosition],
    selection: Option[Selection] = None
  ): AppState =
    val fresh = Buffer.fromString(bufferId, text)
    val buffer = fresh.copy(
      document = fresh.document.copy(indentation = indentation),
      editing = EditingStateFixtures(cursors = cursors, selection = selection)
    )
    val base = AppState.initial
    base.copy(persisted =
      base.persisted.copy(buffers = Map(bufferId -> buffer), config = base.persisted.config.withIndentation(global))
    )

  private def textAfter(event: TextEntryEvent, state: AppState): String =
    EditorEventReducer.reduce(event, paneId, state).state.persisted.buffers(bufferId).document.content.collect()

  private val detectedTabs   = DocumentIndentation(detected = IndentationHint(insertSpaces = Some(false)))
  private val detectedTwo    = DocumentIndentation(detected = IndentationHint.of(IndentationSettings(tabSize = 2)))
  private val chosenEightTab = detectedTwo.withChosen(Some(IndentationSettings(tabSize = 8, insertSpaces = false)))

  "TabKey" should "insert the tab character when the buffer is indented with tabs, whatever the global settings" in {
    textAfter(TabKey, stateWith(detectedTabs, "ab", List(CursorPosition(0, 1)))) shouldBe "a\tb"
  }

  it should "insert the buffer's detected width in spaces" in {
    textAfter(TabKey, stateWith(detectedTwo, "ab", List(CursorPosition(0, 1)))) shouldBe "a  b"
  }

  it should "insert the global width when the buffer has nothing of its own" in {
    textAfter(TabKey, stateWith(DocumentIndentation(), "ab", List(CursorPosition(0, 1)))) shouldBe "a    b"
  }

  it should "prefer the user's choice to what was detected" in {
    textAfter(TabKey, stateWith(chosenEightTab, "ab", List(CursorPosition(0, 1)))) shouldBe "a\tb"
  }

  it should "indent selected lines by the buffer's unit" in {
    val state = stateWith(
      detectedTwo,
      "one\ntwo",
      List(CursorPosition(1, 3)),
      Some(Selection(CursorPosition(0, 0), CursorPosition(1, 3)))
    )

    textAfter(TabKey, state) shouldBe "  one\n  two"
  }

  it should "insert the buffer's unit at every cursor" in {
    val state = stateWith(detectedTabs, "ab\ncd", List(CursorPosition(0, 1), CursorPosition(1, 1)))

    textAfter(TabKey, state) shouldBe "a\tb\nc\td"
  }

  "ReverseTabKey" should "remove at most the buffer's width of leading spaces" in {
    textAfter(ReverseTabKey, stateWith(detectedTwo, "      abc", List(CursorPosition(0, 9)))) shouldBe "    abc"
    textAfter(ReverseTabKey, stateWith(DocumentIndentation(), "      abc", List(CursorPosition(0, 9)))) shouldBe
      "  abc"
  }

  it should "remove a leading tab from a buffer indented with tabs" in {
    textAfter(ReverseTabKey, stateWith(detectedTabs, "\t\tabc", List(CursorPosition(0, 5)))) shouldBe "\tabc"
  }
