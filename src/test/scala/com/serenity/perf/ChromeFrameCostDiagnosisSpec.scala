package com.serenity.perf

import java.awt.{Font, GraphicsEnvironment}
import java.awt.image.BufferedImage

import com.serenity.MockRenderSurface
import com.serenity.state.manager.RenderCaches
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, RoundedCorners}
import com.serenity.ui.renderer.Java2DRenderSurface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Prints where the plain cell-grid frame's extra cost with rounded pane headers goes. Temporary: it times, in one JVM
  * and interleaved, the real frames with the corner settings turned off one at a time, and the pane header fill on its
  * own against the fill it replaced.
  */
class ChromeFrameCostDiagnosisSpec extends AnyFlatSpec with Matchers:

  private def microsPerCall(calls: Int)(body: () => Unit): Double =
    val start = System.nanoTime()
    var i     = 0
    while i < calls do
      body()
      i += 1
    (System.nanoTime() - start) / 1000.0 / calls

  "The chrome frame cost" should "be printed per variant" in {
    assume(!GraphicsEnvironment.isHeadless, "needs a display")
    val frames =
      (CellGridBenchmarks.benchmarks() ++ CellGridBenchmarks.chromeVariantBenchmarks())
        .filter(_.name.startsWith("render.cell_grid.frame_ascii"))
    val rounds = 6
    val results = for round <- 1 to rounds; frame <- frames yield
      frame.run()
      val perFrame = microsPerCall(3000)(() => { frame.run(); () })
      (frame.name, round, perFrame)
    results.groupBy(_._1).toList.sortBy(_._1).foreach {
      case (name, rows) => println(f"DIAG frame $name%-60s us/frame per round: ${rows.map(r => f"${r._3}%.1f").mkString(" ")}")
    }

    val metrics = CellMetrics.fromFont(Font(Font.MONOSPACED, Font.PLAIN, 14))
    val image   = new BufferedImage(1200, 100, BufferedImage.TYPE_INT_RGB)
    val caches  = RenderCaches.create()
    val surface = new Java2DRenderSurface(image, metrics, Font(Font.MONOSPACED, Font.PLAIN, 14), _ => (), textCaches = caches.surfaceText)
    val outlines = surface.panelOutlines.getOrElse(fail("no outlines"))
    val colour   = RenderColor.fromAwt(java.awt.Color.LIGHT_GRAY)
    surface.setBackgroundColor(colour)
    val rounded = (1 to 5).map(_ => microsPerCall(200000)(() => { outlines.fillRoundRect(2, 0, 60, 1, RoundedCorners.Top, 12, colour); () }))
    val plain   = (1 to 5).map(_ => microsPerCall(200000)(() => surface.putString(2, 0, " " * 60)))
    println(f"DIAG header fill rounded us/call: ${rounded.map(v => f"$v%.2f").mkString(" ")}")
    println(f"DIAG header fill putString us/call: ${plain.map(v => f"$v%.2f").mkString(" ")}")
    val _ = new MockRenderSurface(1, 1)
  }
