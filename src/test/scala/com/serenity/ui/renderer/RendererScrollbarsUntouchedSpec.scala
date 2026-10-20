package com.serenity.ui.renderer

import java.lang.management.ManagementFactory

import com.serenity.ui.layout.PixelRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererScrollbarsUntouchedSpec extends AnyFlatSpec with Matchers:

  private val thumb = PixelRect(100, 0, 12, 1024)
  private val bands = (0 until 1024 by 32).toVector.map(top => PixelRect(100, top, 12, 32))

  private def planKeeping(preserved: List[PixelRect], blits: List[PixelBlit] = Nil): FramePlan =
    FramePlan(Map.empty, ChromeRepaint.Whole, blits, preserved, None)

  /** Every row of 16 px across the strip's width, bar the rows in `repainted`. */
  private def rowsKeptExcept(repainted: Set[Int]): List[PixelRect] =
    (0 until 64).filterNot(repainted).map(row => PixelRect(0, row * 16, 400, 16)).toList

  private def perBand(rects: Vector[PixelRect], plan: FramePlan): Vector[PixelRect] =
    rects.filter(rect => plan.blits.isEmpty && PixelRect.uncoveredWithin(rect, plan.preserved).isEmpty)

  "RendererScrollbars.untouchedRects" should "keep every band when the whole frame is preserved" in {
    RendererScrollbars.untouchedRects(bands, Some(planKeeping(rowsKeptExcept(Set.empty)))) shouldBe bands
  }

  it should "drop only the bands a repainted row reaches" in {
    val plan = planKeeping(rowsKeptExcept(Set(5, 40)))
    val kept = RendererScrollbars.untouchedRects(bands, Some(plan))
    kept shouldBe perBand(bands, plan)
    kept.size shouldBe bands.size - 2
  }

  it should "drop a band reached by a row that straddles two of them" in {
    val plan = planKeeping(List(PixelRect(0, 0, 400, 20), PixelRect(0, 36, 400, 988)))
    RendererScrollbars.untouchedRects(bands, Some(plan)) shouldBe perBand(bands, plan)
  }

  it should "agree with a band-by-band reading when kept rectangles cover the strip only together" in {
    val plan = planKeeping(
      List(PixelRect(0, 0, 106, 1024), PixelRect(106, 0, 300, 512), PixelRect(104, 512, 300, 256))
    )
    RendererScrollbars.untouchedRects(bands, Some(plan)) shouldBe perBand(bands, plan)
    RendererScrollbars.untouchedRects(bands, Some(plan)).size shouldBe 24
  }

  it should "drop every band when the frame copies pixels" in {
    val blit = PixelBlit(PixelRect(0, 0, 10, 10), 16)
    RendererScrollbars.untouchedRects(bands, Some(planKeeping(rowsKeptExcept(Set.empty), List(blit)))) shouldBe empty
  }

  it should "drop every band when the frame has no plan to reuse pixels from" in {
    RendererScrollbars.untouchedRects(bands, None) shouldBe empty
  }

  it should "not look at the preserved rectangles once per band" in {
    val plan = planKeeping(rowsKeptExcept(Set(5, 40)))
    val bean = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
    val id   = Thread.currentThread().threadId()
    def allocated(work: => Any): Long =
      (1 to 200).foreach(_ => work)
      val before = bean.getThreadAllocatedBytes(id)
      (1 to 200).foreach(_ => work)
      bean.getThreadAllocatedBytes(id) - before
    val whole  = allocated(PixelRect.uncoveredWithin(thumb, plan.preserved))
    val banded = allocated(RendererScrollbars.untouchedRects(bands, Some(plan)))
    banded should be < whole / 4
  }
