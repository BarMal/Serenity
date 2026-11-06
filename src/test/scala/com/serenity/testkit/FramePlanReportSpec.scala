package com.serenity.testkit

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
