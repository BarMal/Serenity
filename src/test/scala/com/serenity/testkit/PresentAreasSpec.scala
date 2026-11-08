package com.serenity.testkit

import java.awt.Rectangle

import com.serenity.ui.layout.PixelRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The window copies two kinds of area from the frame it drew onto the screen: the rects the planner names, and the
  * areas the surface recorded painting (a glyph's ink can pass the rect of its row). A spec that presents only the
  * first shows stale pixels the window would not.
  */
class PresentAreasSpec extends AnyFlatSpec with Matchers:

  private val planner  = Some(List(PixelRect(-1, 0, 310, 20)))
  private val recorded = List(Rectangle(300, 5, 20, 10))

  "The areas a frame presents" should "be the whole frame when the planner names no rects" in {
    PresentAreas.of(None, recorded, 1.0, 640, 600) shouldBe None
  }

  it should "be the planner's rects in device pixels, and the recorded paint" in {
    PresentAreas.of(planner, recorded, 1.0, 640, 600) shouldBe Some(
      List(Rectangle(0, 0, 309, 20), Rectangle(300, 5, 20, 10))
    )
  }

  it should "cover a pixel only the recorded paint reaches, and miss it without the recorded paint" in {
    def covers(areas: Option[List[Rectangle]]): Boolean = areas.exists(_.exists(_.contains(315, 8)))
    covers(PresentAreas.of(planner, recorded, 1.0, 640, 600)) shouldBe true
    covers(PresentAreas.of(planner, Nil, 1.0, 640, 600)) shouldBe false
  }

  it should "scale the planner's rects and clip everything to the frame" in {
    PresentAreas.of(planner, List(Rectangle(1270, 590, 30, 30)), 2.0, 1280, 600) shouldBe Some(
      List(Rectangle(0, 0, 618, 40), Rectangle(1270, 590, 10, 10))
    )
  }

  it should "leave out recorded paint wholly outside the frame" in {
    PresentAreas.of(planner, List(Rectangle(700, 10, 5, 5)), 1.0, 640, 600) shouldBe Some(
      List(Rectangle(0, 0, 309, 20))
    )
  }
