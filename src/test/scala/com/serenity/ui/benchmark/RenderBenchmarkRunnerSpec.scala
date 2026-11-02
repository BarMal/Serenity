package com.serenity.ui.benchmark

import java.awt.{GraphicsEnvironment, Window}

import com.serenity.testkit.DisplayExclusive
import com.serenity.ui.renderer.FrameSurfaceFactory
import com.serenity.ui.skia.{LayerSurfaceFactory, SkiaRasterSurfaceFactory, SynchronousSkiaLayerSurfaceFactory}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The harness itself, on a display (Xvfb on Linux, a desktop session on Windows and macOS): what is checked is the
  * mechanics, the figures' shape and the 'GPU NOT USED' guard, never a GPU's speed.
  */
class RenderBenchmarkRunnerSpec extends AnyFlatSpec with Matchers:

  private val small = RenderBenchmarkOptions(widthPx = 640, heightPx = 400, frames = 3, warmup = 1)

  /** The windows the run opens show on the shared display, so other specs' screen captures must not overlap them. */
  private def runExclusively(
    options: RenderBenchmarkOptions,
    skiaFactory: FrameSurfaceFactory,
    skiaProbe: Either[String, Unit] = Right(()),
    startup: String = "",
    rasterFactory: Option[FrameSurfaceFactory] = None
  ): BenchmarkReport =
    DisplayExclusive(RenderBenchmarkRunner.run(options, skiaFactory, skiaProbe, startup, rasterFactory))

  /** The report with every window still showing when the run is done, listed while the lock is still held, so none is
    * another spec's.
    */
  private def runExclusivelyListingOpenWindows(
    options: RenderBenchmarkOptions,
    skiaFactory: FrameSurfaceFactory,
    rasterFactory: Option[FrameSurfaceFactory] = None
  ): (BenchmarkReport, List[Window]) =
    DisplayExclusive.withShowingWindowsAfter(
      RenderBenchmarkRunner.run(options, skiaFactory, Right(()), "", rasterFactory)
    )

  private def withDisplay(): Unit =
    assume(!GraphicsEnvironment.isHeadless, "The benchmark opens real windows, which needs a display")

  "RenderBenchmarkRunner.run" should "time every scenario on both backends, with exactly the frames asked for" in {
    withDisplay()
    val report = runExclusively(small, SynchronousSkiaLayerSurfaceFactory)
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

  it should "record the size each window was drawn at, and time the resize when the window can be resized" in {
    withDisplay()
    val report = runExclusively(small, SynchronousSkiaLayerSurfaceFactory)
    report.environment.drawnWindows.map(_._1) shouldBe List("Java2D", "Skia layer")
    report.environment.drawnWindows.foreach { (arm, size) =>
      withClue(arm)((size.width, size.height) should not be (0, 0))
    }
    report.environment.notTimed shouldBe empty
    report.results.map(_.scenario) should contain(Scenario.ResizeWindow)
  }

  it should "record the scale the Skia layer drew at" in {
    withDisplay()
    val report = runExclusively(small, SynchronousSkiaLayerSurfaceFactory)
    report.environment.skiaDeviceScale.getOrElse(fail("no Skia layer scale")) should be > 0.0
  }

  it should "build its scenes as the app does" in {
    withDisplay()
    val report = runExclusively(small, SynchronousSkiaLayerSurfaceFactory)
    report.environment.scenesLikeTheApp shouldBe true
  }

  // Changed with #2465 M1, by the owner's decision (c6572431154): a cold frame no longer builds the faces again, since
  // the app makes them once. The test used to require a face built in every cold frame.
  it should "measure what the Skia caches did in each cold frame: no face built, a shaper made, text shaped" in {
    withDisplay()
    val report = runExclusively(small, SynchronousSkiaLayerSurfaceFactory)
    val phases = report.environment.coldPhases.getOrElse(fail("no cold-frame phases"))
    phases.frames shouldBe 3
    phases.facesPerFrame shouldBe 0.0
    phases.faceMsPerFrame shouldBe 0.0
    phases.shapersPerFrame shouldBe 1.0
    phases.shapingsPerFrame should be > 0.0
  }

  it should "split each backend's cold frame into layout and drawing, whose sum is within the render call" in {
    withDisplay()
    val report = runExclusively(small, SynchronousSkiaLayerSurfaceFactory)
    val splits = List(
      "Java2D"     -> report.environment.java2dColdSplit,
      "Skia layer" -> report.environment.skiaColdSplit
    )
    splits.foreach { (arm, split) =>
      withClue(arm) {
        val measured = split.getOrElse(fail("no cold-frame split"))
        measured.frames shouldBe 3
        measured.layoutMs should be > 0.0
        measured.drawMs should be > 0.0
        (measured.layoutMs + measured.drawMs) should be <= measured.renderMs
        measured.otherMs should be >= 0.0
        measured.presentMs should be >= 0.0
      }
    }
  }

  it should "check the fallback runs against TextLine on the fonts it draws with, and find them identical" in {
    withDisplay()
    val report    = runExclusively(small, SynchronousSkiaLayerSurfaceFactory)
    val agreement = report.environment.fallbackAgreement.getOrElse(fail("no fallback agreement"))
    agreement.checked should be > 0
    agreement.identical shouldBe agreement.checked
  }

  it should "round every scenario's time up to the display's refresh intervals" in {
    withDisplay()
    val report = runExclusively(small, SynchronousSkiaLayerSurfaceFactory)
    report.environment.refresh.hz should be > 0.0
    report.results.foreach { result =>
      withClue(result.scenario.label) {
        result.java2d.p50Frames should be >= 1
        result.java2d.missedRefresh should be <= result.java2d.count
      }
    }
  }

  it should "end with one Skia layer, say which toolkit drew, and count the layer's draws" in {
    withDisplay()
    val report = runExclusively(small, SynchronousSkiaLayerSurfaceFactory, startup = "X11 (test)")
    report.environment.skiaLayers shouldBe Some(1)
    report.environment.toolkit should include("Toolkit")
    report.environment.startup shouldBe "X11 (test)"
    val draws = report.environment.skiaDraws.getOrElse(fail("the layer's draws were not counted"))
    draws.publishes should be > 0
    draws.renders should be >= 1
    draws.renders should be <= (draws.publishes * 3)
  }

  it should "name the render API and refuse a verdict when Skiko drew in software" in {
    withDisplay()
    val report = runExclusively(small, SynchronousSkiaLayerSurfaceFactory)
    report.environment.skiaRenderApi.isRight shouldBe true
    if !report.environment.gpuUsed then
      report.verdict match
        case Verdict.NotMeasured(reason) => reason should include("GPU NOT USED")
        case other                       => fail(s"a software run got a verdict: $other")
  }

  it should "report the layer as unavailable, and still time Java2D, when the window fell back to Java2D" in {
    withDisplay()
    val failing = new LayerSurfaceFactory(_ => throw new IllegalStateException("no GL context"))
    val report  = runExclusively(small, failing)
    report.environment.skiaRenderApi.isLeft shouldBe true
    report.results.foreach(result => result.skia shouldBe None)
    report.environment.skiaLayers shouldBe None
    report.results.foreach(result => result.java2d.count shouldBe 3)
    report.verdict match
      case Verdict.NotMeasured(reason) => reason should include("Skia layer unavailable")
      case other                       => fail(s"expected no verdict, got $other")
  }

  it should "not time Skia, and say why, when the native library did not load" in {
    withDisplay()
    val report = runExclusively(small, SynchronousSkiaLayerSurfaceFactory, skiaProbe = Left("no libGL"))
    report.environment.skiaRenderApi shouldBe Left("no libGL")
    report.results.foreach(result => result.skia shouldBe None)
  }

  it should "time the Skia raster arm with exactly the frames asked for, beside the other two" in {
    withDisplay()
    val report =
      runExclusively(small, SynchronousSkiaLayerSurfaceFactory, rasterFactory = Some(SkiaRasterSurfaceFactory))
    report.results.foreach { result =>
      withClue(result.scenario.label) {
        result.raster.map(_.count) shouldBe Some(3)
        result.raster.map(_.dropped) shouldBe Some(0)
        result.raster.map(_.p50Ms).getOrElse(0.0) should be > 0.0
        result.java2d.count shouldBe 3
      }
    }
    report.environment.rasterNote shouldBe None
  }

  it should "leave the raster arm out, and say so, when none was asked for" in {
    withDisplay()
    val report = runExclusively(small, SynchronousSkiaLayerSurfaceFactory)
    report.results.foreach(result => result.raster shouldBe None)
    report.environment.rasterNote shouldBe Some("no raster arm was asked for")
  }

  it should "report a raster arm that fails, and still report the other two" in {
    withDisplay()
    val failing: FrameSurfaceFactory = _ => throw new IllegalStateException("no raster surface")
    val (report, open) =
      runExclusivelyListingOpenWindows(small, SynchronousSkiaLayerSurfaceFactory, rasterFactory = Some(failing))
    report.results.foreach { result =>
      result.raster shouldBe None
      result.java2d.count shouldBe 3
      result.skia.map(_.count) shouldBe Some(3)
    }
    report.environment.rasterNote.getOrElse("") should include("no raster surface")
    open shouldBe empty
  }

  it should "leave no window open" in {
    withDisplay()
    val (_, open) = runExclusivelyListingOpenWindows(small, SynchronousSkiaLayerSurfaceFactory)
    open shouldBe empty
  }
