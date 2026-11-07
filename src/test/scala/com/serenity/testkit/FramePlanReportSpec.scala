package com.serenity.testkit

import java.awt.Rectangle
import java.awt.image.BufferedImage

import com.serenity.state.models.PaneId
import com.serenity.ui.layout.PixelRect
import com.serenity.ui.renderer.{ChromeRepaint, FramePlan, PixelBlit}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FramePlanReportSpec extends AnyFlatSpec with Matchers:

  private def plan(
    dirty: Map[PaneId, Set[Int]],
    preserved: List[PixelRect] = Nil,
    blits: List[PixelBlit] = Nil,
    region: Option[List[PixelRect]] = None
  ): FramePlan = FramePlan(dirty, ChromeRepaint.Whole, blits, preserved, region)

  "A frame that drew everything" should "say there was no plan" in {
    FramePlanReport.describe(None) should include("no plan")
  }

  "A plan" should "name each pane's dirty rows in order" in {
    val report = FramePlanReport.describe(Some(plan(Map(PaneId(1) -> Set(9, 2, 3), PaneId(0) -> Set(7)))))
    report should include("pane 0: 7")
    report should include("pane 1: 2, 3, 9")
    report.indexOf("pane 0") should be < report.indexOf("pane 1")
  }

  it should "say so when a pane drew no rows" in {
    FramePlanReport.describe(Some(plan(Map(PaneId(0) -> Set.empty)))) should include("pane 0: none")
  }

  it should "give the preserved rects as vertical bands, merging those that touch" in {
    val preserved = List(PixelRect(0, 0, 100, 10), PixelRect(0, 10, 100, 10), PixelRect(0, 40, 100, 5))
    val report    = FramePlanReport.describe(Some(plan(Map.empty, preserved)))
    report should include("preserved y 0-20, 40-45")
  }

  "A mismatch dump given a plan" should "carry it under the line naming the differing pixels" in {
    val actual   = BufferedImage(4, 4, BufferedImage.TYPE_INT_RGB)
    val expected = BufferedImage(4, 4, BufferedImage.TYPE_INT_RGB)
    actual.setRGB(1, 1, 0xffffff)
    val line  = FrameMismatchDump.write("plan-report", actual, expected, Seq((1, 1)), Some("frame plan: dirty rows {}"))
    val lines = line.linesIterator.toList
    lines.head should include("1 px differ")
    lines(1) shouldBe "frame plan: dirty rows {}"
  }

  it should "count blits and say what the screen repaints" in {
    val report = FramePlanReport.describe(
      Some(
        plan(Map.empty, blits = List(PixelBlit(PixelRect(0, 0, 5, 5), 3)), region = Some(List(PixelRect(1, 2, 3, 4))))
      )
    )
    report should include("blits 1")
    report should include("repaint 1 rect")
  }

  it should "list the repaint rects, in logical pixels, and not only count them" in {
    val report = FramePlanReport.describe(
      Some(plan(Map.empty, region = Some(List(PixelRect(-1, 0, 310, 229), PixelRect(0, 585, 637, 15)))))
    )
    report should include("repaint 2 rect(s)")
    report should include("-1,0 310x229")
    report should include("0,585 637x15")
  }

  "The device area of a repaint rect" should "be what the window presents: floored, ceilinged and clipped to the frame" in {
    FramePlanReport.deviceRect(PixelRect(-1, 0, 310, 229), 1.0, 640, 600) shouldBe Some(Rectangle(0, 0, 309, 229))
    FramePlanReport.deviceRect(PixelRect(-1, 0, 310, 229), 2.0, 1280, 1200) shouldBe Some(Rectangle(0, 0, 618, 458))
    FramePlanReport.deviceRect(PixelRect(0, 0, 10, 10), 1.5, 640, 600) shouldBe Some(Rectangle(0, 0, 15, 15))
  }

  it should "be nothing for a rect wholly outside the frame" in {
    FramePlanReport.deviceRect(PixelRect(700, 0, 10, 10), 1.0, 640, 600) shouldBe None
  }

  "The coverage of a repaint region" should "say which differing pixels lie outside every rect the window presented" in {
    val region = Some(List(PixelRect(-1, 0, 310, 229)))
    val report = FramePlanReport.coverage(Some(plan(Map.empty, region = region)), Seq((309, 49), (5, 5)), 1.0, 640, 600)
    report should include("1 of 2 differing pixels lie outside every repaint rect")
    report should include("(309,49) outside")
    report should include("(5,5) inside rect 1")
  }

  it should "work in device pixels at a scale above one" in {
    val region = Some(List(PixelRect(-1, 0, 310, 229)))
    val report =
      FramePlanReport.coverage(Some(plan(Map.empty, region = region)), Seq((617, 100), (618, 100)), 2.0, 1280, 1200)
    report should include("1 of 2 differing pixels lie outside every repaint rect")
    report should include("(617,100) inside rect 1")
    report should include("(618,100) outside")
  }

  it should "say that the whole frame was presented when there is no region" in {
    val report = FramePlanReport.coverage(Some(plan(Map.empty, region = None)), Seq((309, 49)), 1.0, 640, 600)
    report should include("the whole frame was presented")
    report should not include "outside"
  }

  it should "say so when the frame had no plan" in {
    FramePlanReport.coverage(None, Seq((1, 1)), 1.0, 640, 600) should include("no plan")
  }

  it should "show only the first few pixels and count the rest" in {
    val region = Some(List(PixelRect(0, 0, 1, 1)))
    val pixels = (10 until 20).map(x => (x, 3))
    val report = FramePlanReport.coverage(Some(plan(Map.empty, region = region)), pixels, 1.0, 640, 600)
    report should include("10 of 10 differing pixels lie outside every repaint rect")
    report should include("(10,3) outside")
    report should include("and 4 more")
    report should not include "(19,3)"
  }
