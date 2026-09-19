package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.PixelPoint
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Caret-glide (issue #1085 phase 2): [[CursorGlideGeometry.paneRelativePosition]] is the pane-relative pixel position
  * `CursorViewport.seedCursorGlide` tweens between -- relative to the pane's own content origin, not its absolute
  * on-screen rect (see `CursorGlideGeometry`'s own doc comment for why). It reuses the same font/wrap measurement
  * `CursorViewport.adjustForCursor` already does, so these two never disagree about where a wrapped line's rows are.
  */
class CursorGlideGeometrySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(1)

  private def bufferWith(
    content: String,
    viewport: Viewport = Viewport(visibleLines = 10, visibleColumns = 80)
  ): Buffer =
    Buffer.fromString(bufferId, content).copy(viewport = viewport)

  "paneRelativePosition" should "place a cursor at the buffer's own top-left at pane-relative (0, 0)" in {
    val buffer = bufferWith("hello world")

    val position = CursorGlideGeometry.paneRelativePosition(buffer, AppConfig.default, CursorPosition(0, 0))

    position shouldBe PixelPoint(0, 0)
  }

  it should "move the x offset right as the column advances on the same line" in {
    val buffer = bufferWith("hello world")

    val atStart = CursorGlideGeometry.paneRelativePosition(buffer, AppConfig.default, CursorPosition(0, 0))
    val atFive  = CursorGlideGeometry.paneRelativePosition(buffer, AppConfig.default, CursorPosition(0, 5))

    atFive.xPx should be > atStart.xPx
    atFive.yPx shouldBe atStart.yPx
  }

  it should "move the y offset down by one row per logical line when word wrap can't apply" in {
    val buffer = bufferWith("one\ntwo\nthree")

    val line0 = CursorGlideGeometry.paneRelativePosition(buffer, AppConfig.default, CursorPosition(0, 0))
    val line1 = CursorGlideGeometry.paneRelativePosition(buffer, AppConfig.default, CursorPosition(1, 0))
    val line2 = CursorGlideGeometry.paneRelativePosition(buffer, AppConfig.default, CursorPosition(2, 0))

    line0.yPx shouldBe 0
    line1.yPx should be > line0.yPx
    line2.yPx should be > line1.yPx
    line1.xPx shouldBe line0.xPx
  }

  it should "measure the y offset relative to the viewport's current scroll, not the buffer's start" in {
    val buffer = bufferWith(
      "one\ntwo\nthree\nfour\nfive",
      viewport = Viewport(topLine = 2, visibleLines = 3, visibleColumns = 80)
    )

    val atTopOfViewport = CursorGlideGeometry.paneRelativePosition(buffer, AppConfig.default, CursorPosition(2, 0))
    val oneRowDown      = CursorGlideGeometry.paneRelativePosition(buffer, AppConfig.default, CursorPosition(3, 0))

    atTopOfViewport.yPx shouldBe 0
    oneRowDown.yPx should be > atTopOfViewport.yPx
  }

  it should "account for word-wrapped continuation rows within a single logical line" in {
    val longLine = "a" * 300
    val buffer = bufferWith(
      longLine,
      viewport = Viewport(visibleLines = 20, visibleColumns = 20)
    )
    val config = AppConfig.default

    val atStart  = CursorGlideGeometry.paneRelativePosition(buffer, config, CursorPosition(0, 0))
    val farRight = CursorGlideGeometry.paneRelativePosition(buffer, config, CursorPosition(0, 250))

    // A column 250 columns into a wrapped line lands several wrapped rows down, not on the same row as column 0.
    farRight.yPx should be > atStart.yPx
  }
