package com.serenity.ui.renderer

import java.awt.Font

import com.serenity.MockRenderSurface
import com.serenity.ui.layout.{CalculatedLayout, CellMetrics, LayoutRect, PixelRect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class KeptRowFacingLineSpec extends AnyFlatSpec with Matchers:

  private val font = Font(Font.MONOSPACED, Font.PLAIN, 12)

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

  private val clipping = contextOver(new MockRenderSurface(40, 10):
    override def addressesPixels: Boolean = true
    override def keepsOffPixels: Boolean  = true)

  private val plain = contextOver(new MockRenderSurface(40, 10))

  private val kept = PixelRect(-1, 100, 402, 20)

  "A kept row" should "give up its first line where a redrawn row abuts it from above" in {
    RendererChromePlan.withoutLineFacing(clipping, List(PixelRect(0, 80, 400, 20)))(kept) shouldBe
      PixelRect(-1, 101, 402, 19)
  }

  it should "give up its last line where a redrawn row abuts it from below" in {
    RendererChromePlan.withoutLineFacing(clipping, List(PixelRect(0, 120, 400, 20)))(kept) shouldBe
      PixelRect(-1, 100, 402, 19)
  }

  it should "give up a line on each side between two redrawn rows" in {
    val redrawn = List(PixelRect(0, 80, 400, 20), PixelRect(0, 120, 400, 20))
    RendererChromePlan.withoutLineFacing(clipping, redrawn)(kept) shouldBe PixelRect(-1, 101, 402, 18)
  }

  it should "keep its lines where no redrawn row abuts it, or one only beside it, in another column" in {
    RendererChromePlan.withoutLineFacing(clipping, List(PixelRect(0, 50, 400, 20)))(kept) shouldBe kept
    RendererChromePlan.withoutLineFacing(clipping, List(PixelRect(500, 80, 100, 20)))(kept) shouldBe kept
  }

  it should "keep its lines on a surface that cannot clip, where the row beside it is not painted again" in {
    RendererChromePlan.withoutLineFacing(plain, List(PixelRect(0, 80, 400, 20)))(kept) shouldBe kept
  }
