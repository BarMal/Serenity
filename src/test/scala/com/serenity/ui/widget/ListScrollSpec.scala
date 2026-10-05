package com.serenity.ui.widget

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The scroll model every list panel shares: a stored offset that only a keyboard move ties back to the selection. */
class ListScrollSpec extends AnyFlatSpec with Matchers:

  private val items = 100
  private val rows  = 10

  "A list scroll" should "move only as far as it must to show a selection below the viewport" in {
    ListScroll().shownOffset(items, Some(14), rows) shouldBe 5
  }

  it should "stay put while the selection moves back up within the rows shown" in {
    val scrolled = ListScroll().revealing(items, Some(14), rows)

    scrolled.revealing(items, Some(13), rows) shouldBe scrolled
    scrolled.shownOffset(items, Some(6), rows) shouldBe 5
  }

  it should "move up only as far as it must to show a selection above the viewport" in {
    ListScroll(offset = 20).shownOffset(items, Some(17), rows) shouldBe 17
  }

  it should "scroll from what is shown without moving to the selection, and stop following it" in {
    val following = ListScroll().revealing(items, Some(14), rows)

    val wheeled = following.scrolledBy(30, items, Some(14), rows)

    wheeled shouldBe ListScroll(offset = 35, followsSelection = false)
    wheeled.shownOffset(items, Some(14), rows) shouldBe 35
  }

  it should "stop scrolling at the top and at the last full page" in {
    ListScroll().scrolledBy(-3, items, None, rows).offset shouldBe 0
    ListScroll().scrolledBy(500, items, None, rows).offset shouldBe items - rows
    ListScroll().scrolledBy(5, 4, None, rows).offset shouldBe 0
  }

  it should "bring a scrolled-away selection back into view once a keyboard move follows it again" in {
    val wheeled = ListScroll(offset = 60, followsSelection = false)

    wheeled.revealing(items, Some(14), rows) shouldBe ListScroll(offset = 14)
  }

  it should "keep a stored offset inside the list after the list shrinks" in {
    ListScroll(offset = 90, followsSelection = false).shownOffset(20, None, rows) shouldBe 10
  }

  it should "list only the window of indexes the viewport shows" in {
    ListScroll(offset = 95, followsSelection = false).window(items, None, rows) shouldBe (90 until 100)
    ListScroll().window(3, Some(1), rows) shouldBe (0 until 3)
  }
