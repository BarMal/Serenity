package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TabStopsSpec extends AnyFlatSpec with Matchers:

  "TabStops.cellsAt" should "span to the next multiple of the tab size" in {
    TabStops.cellsAt(0, 4) shouldBe 4
    TabStops.cellsAt(1, 4) shouldBe 3
    TabStops.cellsAt(3, 4) shouldBe 1
    TabStops.cellsAt(4, 4) shouldBe 4
    TabStops.cellsAt(5, 8) shouldBe 3
  }

  it should "treat a tab size below one as one" in {
    TabStops.cellsAt(3, 0) shouldBe 1
  }

  "TabStops.advance" should "give a tab its stop's width and any other codepoint its display width" in {
    TabStops.advance('\t', 2, 4) shouldBe 2
    TabStops.advance('a', 2, 4) shouldBe 1
    TabStops.advance(0x4e2d, 2, 4) shouldBe 2
  }
