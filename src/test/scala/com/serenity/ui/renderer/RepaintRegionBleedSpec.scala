package com.serenity.ui.renderer

import com.serenity.PersistedFrameFixture
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A glyph's antialiasing can land a pixel outside the column its row is laid out in (a serif italic on Windows and
  * macOS), and a frame keeps those pixels with the row ([[RendererChromePlan.withBleed]]). The window shows only the
  * rects a frame names, so the rect of a row it draws again reaches as far, or that pixel stays as the last frame left
  * it: an orphan beside rows that have since moved on (#2016).
  */
class RepaintRegionBleedSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  "The repaint region of an edited row" should "reach past the pane's columns as far as the rows a frame keeps" in {
    val before  = stateOf(proseLines.mkString("\n"), CursorPosition(1, 0))
    val after   = typedAt(before, proseLines, line = 1, text = "x")
    val session = Session()
    val plan    = session.settle(before)
    val _       = session.draw(after, DamageProducer.forTransition(before, after))

    val content = plan.paneLayouts.getOrElse(paneId, fail("expected the pane's layout")).contentRect
    val leftPx  = cellMetrics.toPixelX(content.x)
    val rightPx = cellMetrics.toPixelX(content.right)
    val region  = session.repaintRegion.getOrElse(fail("expected a bounded repaint"))

    withClue(s"region $region, pane columns $leftPx-$rightPx: ") {
      region.exists(rect =>
        rect.xPx <= leftPx - RendererChromePlan.BleedPx && rect.rightPx >= rightPx + RendererChromePlan.BleedPx
      ) shouldBe true
    }
  }
