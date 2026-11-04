package com.serenity.state.manager

import com.serenity.keystroke.events.{ScrollDown, ScrollUp, TextEntryEvent}
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, EditorPaneComponent}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2570 items 4 and 5: in column mode the vertical wheel turns whole pages, the same pages the caret anchors to and
  * the painter draws, and leaves the caret where it is. Stepping `topLine` by logical lines put the view between pages,
  * so the painter flowed columns from a line no page starts on.
  */
class ColumnModeWheelSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  import ColumnPagingFixture.{bufferId, paneId}

  private val fixture = ColumnPagingFixture(scale = 1, columnCount = 2)

  private def top(state: AppState): (Int, Int) =
    val viewport = state.persisted.buffers(bufferId).viewport
    (viewport.topLine, viewport.topVisualLine)

  private def pageTop(index: Int): (Int, Int) =
    val viewport = fixture.pages(index).viewport
    (viewport.topLine, viewport.topVisualLine)

  private def wheel(state: AppState, event: TextEntryEvent): AppState =
    new EditorPaneComponent(paneId, fixture.wrapCache).processEvent(event, state) match
      case ComponentResult.ReducerUpdate(result) => result.state
      case other                                 => fail(s"expected a reduction, got $other")

  private def wheelAll(state: AppState, events: TextEntryEvent*): AppState = events.foldLeft(state)(wheel)

  "A wheel notch down in column mode" should "turn to the start of the next page" in {
    top(wheel(fixture.state, ScrollDown(3))) shouldBe pageTop(1)
  }

  it should "turn one page per notch, whatever the notch's line count" in {
    top(wheelAll(fixture.state, ScrollDown(1), ScrollDown(7))) shouldBe pageTop(2)
  }

  it should "leave the caret where it was" in {
    val before = fixture.state.persisted.buffers(bufferId).editing

    wheel(fixture.state, ScrollDown(3)).persisted.buffers(bufferId).editing shouldBe before
  }

  it should "rest on the last page once there" in {
    val last   = fixture.pages.length - 1
    val atLast = wheelAll(fixture.state, List.fill(fixture.pages.length + 3)(ScrollDown(3))*)

    top(atLast) shouldBe pageTop(last)
  }

  "A wheel notch up in column mode" should "turn back to the start of the previous page" in {
    top(wheelAll(fixture.state, ScrollDown(3), ScrollDown(3), ScrollUp(3))) shouldBe pageTop(1)
  }

  it should "rest on the first page" in {
    top(wheelAll(fixture.state, ScrollDown(3), ScrollUp(3), ScrollUp(3))) shouldBe pageTop(0)
  }

  "Every wheel step in column mode" should "land on a page the caret could anchor to" in {
    val tops   = fixture.pages.map(page => (page.viewport.topLine, page.viewport.topVisualLine)).toSet
    val events = List(ScrollDown(3), ScrollDown(1), ScrollUp(3), ScrollDown(5), ScrollDown(3), ScrollUp(1))
    val visited = events.scanLeft(fixture.state)(wheel).map(top)

    visited.filterNot(tops.contains) shouldBe Nil
  }
