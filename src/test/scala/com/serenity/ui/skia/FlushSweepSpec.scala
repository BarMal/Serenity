package com.serenity.ui.skia

import java.awt.GraphicsEnvironment

import org.jetbrains.skia.Picture
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FlushSweepSpec extends AnyFlatSpec with Matchers:

  import FlushSweep.*

  private def opCount(picture: Picture): Int =
    try picture.getApproximateOpCount
    finally picture.close()

  private def summary(rows: Int, p50: Double): Summary =
    Summary(rows, Variant.Same, draws = 10, minMs = p50, p50Ms = p50, p95Ms = p50, maxMs = p50)

  "FlushSweep.summarise" should "take the nearest-rank median and 95th percentile of a measurement" in {
    val result = summarise(Measurement(8, Variant.Same, Vector.tabulate(100)(i => (i + 1).toDouble)))
    result shouldBe Summary(8, Variant.Same, draws = 100, minMs = 1.0, p50Ms = 50.0, p95Ms = 95.0, maxMs = 100.0)
  }

  it should "not depend on the order the draws were measured in" in {
    val ascending = Vector.tabulate(40)(i => (i + 1).toDouble)
    val shuffled  = ascending.reverse.grouped(3).toVector.flatMap(_.reverse)
    summarise(Measurement(4, Variant.Same, shuffled)) shouldBe summarise(Measurement(4, Variant.Same, ascending))
  }

  it should "summarise a single draw as that draw" in {
    summarise(Measurement(1, Variant.OneRowChanged, Vector(7.5))) shouldBe
      Summary(1, Variant.OneRowChanged, draws = 1, minMs = 7.5, p50Ms = 7.5, p95Ms = 7.5, maxMs = 7.5)
  }

  "FlushSweep.fit" should "recover the intercept and the cost per row of a straight line" in {
    val line  = List(1, 10, 20, 50).map(rows => summary(rows, 3.0 + 0.5 * rows))
    val found = fit(line).getOrElse(fail("no fit"))
    found.interceptMs shouldBe 3.0 +- 1e-9
    found.slopeMsPerRow shouldBe 0.5 +- 1e-9
  }

  it should "have nothing to say about a single row count" in {
    fit(List(summary(5, 4.0), summary(5, 4.5))) shouldBe None
  }

  it should "fit each variant on its own" in {
    val same    = List(1, 10).map(rows => summary(rows, 2.0 + rows))
    val changed = List(1, 10).map(rows => summary(rows, 20.0).copy(variant = Variant.OneRowChanged))
    fit(same ++ changed, Variant.OneRowChanged).map(_.slopeMsPerRow) shouldBe Some(0.0)
    fit(same ++ changed, Variant.Same).map(_.slopeMsPerRow) shouldBe Some(1.0)
  }

  "FlushSweep.report" should "name the render API, list every row count and give the fit" in {
    val lines = report("OPENGL", List(1, 10).map(rows => summary(rows, 2.0 + rows)))
    lines.head should include("OPENGL")
    lines.exists(_.contains("p50")) shouldBe true
    lines.count(_.trim.startsWith("1 ")) shouldBe 1
    lines.count(_.trim.startsWith("10 ")) shouldBe 1
    lines.mkString("\n") should include("intercept 2.00 ms")
    lines.mkString("\n") should include("1.00 ms per row")
  }

  "FlushSweep.vsync" should "read on and off, and nothing else, from the argument" in {
    vsyncRequested(Some("on")) shouldBe Right(true)
    vsyncRequested(Some("off")) shouldBe Right(false)
    vsyncRequested(None) shouldBe Right(false)
    vsyncRequested(Some("maybe")).isLeft shouldBe true
  }

  it should "set Skiko's property the way the app does at startup, off unless asked" in {
    vsyncProperty(enabled = false, env = Map.empty, properties = Map.empty) shouldBe
      Some("skiko.vsync.enabled" -> "false")
    vsyncProperty(enabled = true, env = Map.empty, properties = Map.empty) shouldBe
      Some("skiko.vsync.enabled" -> "true")
  }

  it should "leave a value the user already set, in the property or the environment" in {
    vsyncProperty(enabled = false, env = Map.empty, properties = Map("skiko.vsync.enabled" -> "true")) shouldBe None
    vsyncProperty(enabled = false, env = Map("SKIKO_VSYNC_ENABLED" -> "1"), properties = Map.empty) shouldBe None
  }

  "FlushSweep.scene" should "draw more ops for more rows" in {
    val few  = opCount(scene(rows = 2, variant = Variant.Same, drawIndex = 0, widthPx = 400, heightPx = 300))
    val many = opCount(scene(rows = 12, variant = Variant.Same, drawIndex = 0, widthPx = 400, heightPx = 300))
    many should be > few
  }

  it should "differ between draws only for the variant that changes a row" in {
    def bytes(variant: Variant, index: Int): Vector[Byte] =
      val picture = scene(rows = 6, variant, index, 400, 300)
      try picture.serializeToData().getBytes.toVector
      finally picture.close()
    bytes(Variant.Same, 0) shouldBe bytes(Variant.Same, 1)
    bytes(Variant.OneRowChanged, 0) should not be bytes(Variant.OneRowChanged, 1)
  }

  "FlushSweep.run" should "time every draw of every row count on a hosted layer" in {
    assume(!GraphicsEnvironment.isHeadless, "A hosted layer needs a display")
    val outcome =
      FlushSweep.run(rowCounts = List(1, 3), draws = 3, variants = List(Variant.Same, Variant.OneRowChanged))
    val (renderApi, summaries) = outcome.fold(message => fail(message), identity)
    renderApi should not be empty
    summaries.map(s => (s.rows, s.variant)).toSet shouldBe
      Set((1, Variant.Same), (3, Variant.Same), (1, Variant.OneRowChanged), (3, Variant.OneRowChanged))
    summaries.foreach { each =>
      each.draws shouldBe 3
      each.minMs should be > 0.0
    }
  }
