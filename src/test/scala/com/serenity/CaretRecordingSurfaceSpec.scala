package com.serenity

import java.awt.{Color, Dimension, Font}
import javax.swing.JPanel

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.renderer.CaretRecordingSurface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CaretRecordingSurfaceSpec extends AnyFlatSpec with Matchers:

  private val font    = Font(Font.MONOSPACED, Font.PLAIN, 10)
  private val metrics = CellMetrics.fromFont(font)

  private def canvas: JPanel =
    val panel = JPanel()
    panel.setPreferredSize(Dimension(400, 300))
    panel.setSize(Dimension(400, 300))
    panel

  "CaretRecordingSurface" should "record caret fills in paint order with their colours" in {
    val surface = CaretRecordingSurface.forCanvas(metrics, font, canvas)
    surface.fillPixelRect(10, 20, 2, 14, RenderColor.fromAwt(Color.WHITE))
    surface.fillPixelRect(50, 60, 2, 14, RenderColor.fromAwt(new Color(255, 0, 0, 90)))
    surface.flush()

    surface.recordedFills shouldBe List(
      CaretRecordingSurface.CaretFill(PixelRect(10, 20, 2, 14), Color.WHITE),
      CaretRecordingSurface.CaretFill(PixelRect(50, 60, 2, 14), new Color(255, 0, 0, 90))
    )
  }

  it should "record a degenerate fill at the one-pixel minimum a Java2D surface paints" in {
    val surface = CaretRecordingSurface.forCanvas(metrics, font, canvas)
    surface.fillPixelRect(5, 5, 0, 0, RenderColor.fromAwt(Color.WHITE))

    surface.recordedFills.map(_.rect) shouldBe List(PixelRect(5, 5, 1, 1))
  }

  it should "report the canvas's own logical viewport" in {
    val surface = CaretRecordingSurface.forCanvas(metrics, font, canvas)

    surface.viewportWidth shouldBe 400 / metrics.charWidth
    surface.viewportHeight shouldBe 300 / metrics.lineHeight
  }
end CaretRecordingSurfaceSpec
