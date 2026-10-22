package com.serenity.ui.benchmark

import java.awt.{GraphicsEnvironment, Window}

import com.serenity.ui.skia.{LayerSurfaceFactory, SynchronousSkiaLayerSurfaceFactory}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The harness itself, on a display (Xvfb in CI): where Skiko has no GL context it draws with its software redrawer, so
  * what is checked is the mechanics, the figures' shape and the 'GPU NOT USED' guard, never a GPU's speed.
  */
class RenderBenchmarkRunnerSpec extends AnyFlatSpec with Matchers:

  private val small = RenderBenchmarkOptions(widthPx = 640, heightPx = 400, frames = 3, warmup = 1)

  private def onLinuxWithDisplay(): Unit =
    assume(!GraphicsEnvironment.isHeadless, "The benchmark opens real windows, which needs a display")
    assume(
      System.getProperty("os.name", "").toLowerCase.contains("linux"),
      "other systems draw the layer with their GPU API, which this check leaves to the owner's machine"
    )

  "RenderBenchmarkRunner.run" should "time every scenario on both backends, with exactly the frames asked for" in {
    onLinuxWithDisplay()
    val report = RenderBenchmarkRunner.run(small, SynchronousSkiaLayerSurfaceFactory)
    report.results.map(_.scenario) shouldBe Scenario.values.toVector
    report.results.foreach { result =>
      withClue(result.scenario.label) {
        result.java2d.count shouldBe 3
        result.java2d.dropped shouldBe 0
        result.skia.map(_.count) shouldBe Some(3)
        result.skia.map(_.dropped) shouldBe Some(0)
        result.java2d.p50Ms should be > 0.0
      }
    }
    (report.environment.widthPx, report.environment.heightPx) shouldBe (640, 400)
  }

  it should "name the render API and refuse a verdict when Skiko drew in software" in {
    onLinuxWithDisplay()
    val report = RenderBenchmarkRunner.run(small, SynchronousSkiaLayerSurfaceFactory)
    report.environment.skiaRenderApi.isRight shouldBe true
    if !report.environment.gpuUsed then
      report.verdict match
        case Verdict.NotMeasured(reason) => reason should include("GPU NOT USED")
        case other                       => fail(s"a software run got a verdict: $other")
  }

  it should "report the layer as unavailable, and still time Java2D, when the window fell back to Java2D" in {
    onLinuxWithDisplay()
    val failing = new LayerSurfaceFactory(_ => throw new IllegalStateException("no GL context"))
    val report  = RenderBenchmarkRunner.run(small, failing)
    report.environment.skiaRenderApi.isLeft shouldBe true
    report.results.foreach(result => result.skia shouldBe None)
    report.results.foreach(result => result.java2d.count shouldBe 3)
    report.verdict match
      case Verdict.NotMeasured(reason) => reason should include("Skia layer unavailable")
      case other                       => fail(s"expected no verdict, got $other")
  }

  it should "not time Skia, and say why, when the native library did not load" in {
    onLinuxWithDisplay()
    val report = RenderBenchmarkRunner.run(small, SynchronousSkiaLayerSurfaceFactory, skiaProbe = Left("no libGL"))
    report.environment.skiaRenderApi shouldBe Left("no libGL")
    report.results.foreach(result => result.skia shouldBe None)
  }

  it should "leave no window open" in {
    onLinuxWithDisplay()
    val _ = RenderBenchmarkRunner.run(small, SynchronousSkiaLayerSurfaceFactory)
    Window.getWindows.filter(_.isShowing) shouldBe empty
  }
