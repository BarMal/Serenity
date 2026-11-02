package com.serenity.ui.layout

import com.serenity.config.{DockedCornerStyle, PaneHeaderCorners}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: which corners of each piece of chrome round, as a pure function of where it sits. */
class CornerRoundingSpec extends AnyFlatSpec with Matchers:

  private val window = ViewportSize(90, 18)

  // The mock-up window: a 13-wide left panel and a 20-wide right panel from the top down to the status bar (row 17).
  private val leftPanel  = LayoutRect(0, 0, 13, 17)
  private val rightPanel = LayoutRect(70, 0, 20, 17)
  private val statusBar  = LayoutRect(0, 17, 90, 1)
  private val tabStrip   = LayoutRect(0, 0, 90, 1)

  private def corners(
    element: ChromeElement,
    rect: LayoutRect,
    style: DockedCornerStyle = DockedCornerStyle.Floating,
    header: PaneHeaderCorners = PaneHeaderCorners.Rounded
  ) = CornerRounding.corners(element, rect, window, style, header)

  "CornerRounding.corners" should "round every corner of a floating surface, whatever the docked style" in {
    val popup = LayoutRect(20, 5, 30, 8)
    List(DockedCornerStyle.Floating, DockedCornerStyle.Flush).foreach { style =>
      corners(ChromeElement.FloatingSurface, popup, style) shouldBe RoundedCorners.All
    }
  }

  it should "round every corner of a docked panel, the status bar and the tab strip when docked chrome floats" in
    List(
      ChromeElement.DockedPanel -> leftPanel,
      ChromeElement.DockedPanel -> rightPanel,
      ChromeElement.StatusBar   -> statusBar,
      ChromeElement.TabStrip    -> tabStrip
    ).foreach {
      case (element, rect) =>
        withClue(s"$element $rect: ")(corners(element, rect) shouldBe RoundedCorners.All)
    }

  it should "round only the corners off the window's edge when docked chrome is flush" in {
    val flush = DockedCornerStyle.Flush
    corners(ChromeElement.DockedPanel, leftPanel, flush) shouldBe
      RoundedCorners(topLeft = false, topRight = false, bottomRight = true, bottomLeft = false)
    corners(ChromeElement.DockedPanel, rightPanel, flush) shouldBe
      RoundedCorners(topLeft = false, topRight = false, bottomRight = false, bottomLeft = true)
  }

  it should "leave the status bar and the tab strip square when docked chrome is flush, since both ends touch the window" in {
    val flush = DockedCornerStyle.Flush
    corners(ChromeElement.StatusBar, statusBar, flush) shouldBe RoundedCorners.None
    corners(ChromeElement.TabStrip, tabStrip, flush) shouldBe RoundedCorners.None
  }

  it should "round a flush panel's top corners when a tab strip sits above it, not the window edge" in {
    val belowStrip = LayoutRect(0, 1, 13, 16)
    corners(ChromeElement.DockedPanel, belowStrip, DockedCornerStyle.Flush) shouldBe
      RoundedCorners(topLeft = false, topRight = true, bottomRight = true, bottomLeft = false)
  }

  it should "round a pane header's top corners, and only those, when asked to, in either docked style" in {
    val header = LayoutRect(14, 0, 55, 1)
    List(DockedCornerStyle.Floating, DockedCornerStyle.Flush).foreach { style =>
      corners(ChromeElement.PaneHeader, header, style, PaneHeaderCorners.Rounded) shouldBe RoundedCorners.Top
      corners(ChromeElement.PaneHeader, header, style, PaneHeaderCorners.Square) shouldBe RoundedCorners.None
    }
  }

  "CornerRounding.effectiveRadiusPx" should "never exceed half the smaller side, so a small surface cannot invert its arc" in {
    CornerRounding.effectiveRadiusPx(12, widthPx = 200, heightPx = 100) shouldBe 12
    CornerRounding.effectiveRadiusPx(32, widthPx = 200, heightPx = 40) shouldBe 20
    CornerRounding.effectiveRadiusPx(12, widthPx = 9, heightPx = 17) shouldBe 4
    CornerRounding.effectiveRadiusPx(0, widthPx = 200, heightPx = 100) shouldBe 0
    CornerRounding.effectiveRadiusPx(12, widthPx = 0, heightPx = 0) shouldBe 0
  }

  "RoundedCorners" should "say whether any corner rounds" in {
    RoundedCorners.All.any shouldBe true
    RoundedCorners.Top.any shouldBe true
    RoundedCorners.None.any shouldBe false
  }
