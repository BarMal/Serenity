package com.serenity.ui.skia

import java.awt.{Dimension, Font}
import javax.swing.JPanel

import com.serenity.ui.layout.CellMetrics
import com.serenity.ui.renderer.{FontSpec, SurfaceGeometry}
import com.serenity.ui.terminal.SwingCanvasGeometry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A caret recorder is made for every cursor frame, so the window keeps the canvas behind it from one frame to the next
  * while nothing it measures against has changed (the cursor-only frame is the cheapest the renderer draws, and a
  * native raster, text caches and a surface for each would cost more than the frame).
  */
class SkiaCaretCanvasesSpec extends AnyFlatSpec with Matchers:

  private val font    = FontSpec.fromAwt(Font(Font.MONOSPACED, Font.PLAIN, 12))
  private val metrics = CellMetrics.fromFont(Font(Font.MONOSPACED, Font.PLAIN, 12))

  private def geometry(width: Int, height: Int): SurfaceGeometry =
    val canvas = JPanel()
    canvas.setPreferredSize(Dimension(width, height))
    canvas.setSize(Dimension(width, height))
    SwingCanvasGeometry.of(canvas)

  "SkiaCaretCanvases" should "hand back the same canvas for the same geometry, metrics and font" in {
    val canvases = new SkiaCaretCanvases
    val window   = geometry(400, 300)
    val first    = canvases.forGeometry(metrics, font, window)
    val second   = canvases.forGeometry(metrics, font, window)

    second should be theSameInstanceAs first
  }

  it should "make a new canvas when the window's size changes" in {
    val canvases = new SkiaCaretCanvases
    val first    = canvases.forGeometry(metrics, font, geometry(400, 300))
    val second   = canvases.forGeometry(metrics, font, geometry(500, 300))

    second should not be theSameInstanceAs(first)
    second.viewportWidth shouldBe 500 / metrics.charWidth
  }

  it should "make a new canvas when the font or the metrics change" in {
    val canvases = new SkiaCaretCanvases
    val window   = geometry(400, 300)
    val first    = canvases.forGeometry(metrics, font, window)
    val bigger   = FontSpec.fromAwt(Font(Font.MONOSPACED, Font.PLAIN, 20))
    val second   = canvases.forGeometry(metrics, bigger, window)
    val coarser  = canvases.forGeometry(metrics.copy(charWidth = metrics.charWidth + 1), bigger, window)

    second should not be theSameInstanceAs(first)
    coarser should not be theSameInstanceAs(second)
  }

  it should "keep a canvas of its own for each window" in {
    val window = geometry(400, 300)
    val one    = new SkiaCaretCanvases().forGeometry(metrics, font, window)
    val other  = new SkiaCaretCanvases().forGeometry(metrics, font, window)

    other should not be theSameInstanceAs(one)
  }

  it should "stay usable after a recorder flushes, because the canvas outlives the frame" in {
    val canvases = new SkiaCaretCanvases
    val window   = geometry(400, 300)
    val first    = canvases.forGeometry(metrics, font, window)
    first.flush()
    val again = canvases.forGeometry(metrics, font, window)

    again should be theSameInstanceAs first
    again.viewportWidth shouldBe 400 / metrics.charWidth
  }

end SkiaCaretCanvasesSpec
