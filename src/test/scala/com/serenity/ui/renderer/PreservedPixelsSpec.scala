package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.MockRenderSurface
import com.serenity.state.models.PaneId
import com.serenity.ui.layout.{CalculatedLayout, CellMetrics, LayoutRect, PixelRect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A redrawn row's band is cleared, and the ink a kept neighbour put into it (a descender a pixel past its own band) is
  * lost with it unless the neighbour is painted back under the clip that keeps it off the kept pixels. That has to hold
  * for a frame that moves no rows as much as for one that does.
  */
class PreservedPixelsSpec extends AnyFlatSpec with Matchers:

  private val paneId = PaneId(0)
  private val font   = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val kept   = List(PixelRect(0, 0, 100, 20), PixelRect(0, 60, 100, 40))

  private def contextOver(surface: MockRenderSurface): RenderContext =
    val metrics = CellMetrics.fromFont(font)
    RenderContext(
      surface = surface,
      layout = CalculatedLayout(
        editorPanelRect = LayoutRect(0, 0, surface.width, surface.height),
        leftSpacerRect = LayoutRect(0, 0, 0, 0),
        rightSpacerRect = LayoutRect(0, 0, 0, 0)
      ),
      codeFont = font,
      textFont = font,
      uiFont = font,
      cellMetrics = metrics,
      uiMetrics = metrics
    )

  private def clippingSurface: MockRenderSurface =
    new MockRenderSurface(40, 10):
      override def addressesPixels: Boolean = true
      override def keepsOffPixels: Boolean  = true

  private def planKeeping(bands: List[PixelRect], blits: List[PixelBlit] = Nil): FramePlan =
    FramePlan(Map(paneId -> Set(2, 3)), ChromeRepaint.Whole, blits, Nil, None, keptBands = bands)

  private val dirty = Map(paneId -> Set(2, 3))

  "A frame that copies no rows" should "paint the rows beside the redrawn ones too, under the clip, on a surface that can clip" in {
    val keptOff = PreservedPixels.of(contextOver(clippingSurface), Some(planKeeping(kept)))
    keptOff.rowsToPaint(dirty) shouldBe Map(paneId -> Set(1, 2, 3, 4))
  }

  "A frame that copies rows" should "paint the rows beside the redrawn ones too, as before" in {
    val moved   = List(PixelBlit(PixelRect(0, 0, 10, 10), 5))
    val keptOff = PreservedPixels.of(contextOver(clippingSurface), Some(planKeeping(kept, moved)))
    keptOff.rowsToPaint(dirty) shouldBe Map(paneId -> Set(1, 2, 3, 4))
  }

  "A surface that cannot clip" should "paint only the redrawn rows, whether or not the frame copies rows" in {
    val plain = contextOver(new MockRenderSurface(40, 10))
    PreservedPixels.of(plain, Some(planKeeping(kept))).rowsToPaint(dirty) shouldBe dirty
    PreservedPixels
      .of(plain, Some(planKeeping(kept, List(PixelBlit(PixelRect(0, 0, 10, 10), 5)))))
      .rowsToPaint(dirty) shouldBe dirty
  }

  "A frame that keeps nothing" should "paint only the redrawn rows" in {
    PreservedPixels.of(contextOver(clippingSurface), Some(planKeeping(Nil))).rowsToPaint(dirty) shouldBe dirty
    PreservedPixels.of(contextOver(clippingSurface), None).rowsToPaint(dirty) shouldBe dirty
  }

  "The rows beside the redrawn ones" should "not go past the first or last row" in {
    val keptOff = PreservedPixels.of(contextOver(clippingSurface), Some(planKeeping(kept)))
    keptOff.rowsToPaint(Map(paneId -> Set(0))) shouldBe Map(paneId -> Set(0, 1))
  }
