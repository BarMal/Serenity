package com.serenity.perf

import com.serenity.perf.RendererBenchmarkReport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererBenchmarkReportSpec extends AnyFlatSpec with Matchers:

  "Renderer.parseAll" should "read a comma separated list in the order given, without repeats" in {
    Renderer.parseAll("java2d, skia ,skia-raster,skia") shouldBe
      Right(List(Renderer.Java2D, Renderer.Skia, Renderer.SkiaRaster))
  }

  it should "reject an unknown name and an empty list" in {
    Renderer.parseAll("java2d,metal").left.map(_.contains("metal")) shouldBe Left(true)
    Renderer.parseAll(" , ").isLeft shouldBe true
  }

  "Each renderer" should "be selected by the launch flags a user would type" in {
    Renderer.Java2D.flags shouldBe List("--renderer=java2d")
    Renderer.Skia.flags shouldBe List("--renderer=skia")
    Renderer.SkiaRaster.flags shouldBe List("--renderer=skia", "--wayland-render=skia-raster")
  }

  "A child's output" should "round trip its environment and its frames, ignoring any other line" in {
    val environment = Map("backend" -> "skia", "api" -> "OPENGL", "scale" -> "2.0")
    val frames      = List(Sample("typing", 4_000_000L, 2048L), Sample("scroll", 9_000_000L, 0L))
    val output =
      "[info] starting" :: environmentLine(environment) :: frames.map(sampleLine) ::: List("some other line")

    parseChild(output) shouldBe ChildReport(environment, frames)
  }

  it should "skip a frame line that is cut off or not numeric" in {
    parseChild(List("SAMPLE\ttyping\t12", "SAMPLE\ttyping\tx\t3", "SAMPLE\ttyping\t5\t6")).samples shouldBe
      List(Sample("typing", 5L, 6L))
  }

  "checked" should "accept a child whose startup chose the renderer asked for" in {
    val report = ChildReport(Map("backend" -> "skia"), Nil)
    checked(Renderer.Skia, report) shouldBe Right(report)
  }

  it should "refuse a child that fell back, and say why startup chose what it did" in {
    val report = ChildReport(Map("backend" -> "java2d", "reason" -> "skia needs X11; running on Wayland"), Nil)
    checked(Renderer.Skia, report) shouldBe Left("startup chose java2d: skia needs X11; running on Wayland")
  }

  "percentile" should "take the nearest rank of sorted values" in {
    val values = (1 to 100).map(_.toDouble)
    percentile(values, 0.50) shouldBe 50.0
    percentile(values, 0.95) shouldBe 95.0
    percentile(values, 0.99) shouldBe 99.0
    percentile(Vector.empty, 0.5) shouldBe 0.0
    percentile(Vector(7.0), 0.99) shouldBe 7.0
  }

  "rows" should "pool the frames of every round for a renderer, per scenario" in {
    val environment = Map("api" -> "OPENGL")
    def round(typing: List[Long]): ChildReport =
      ChildReport(environment, typing.map(nanos => Sample("typing", nanos * 1_000_000L, 1024L * 4)))
    val pooled = rows(Renderer.Skia, List(round(List(1, 2, 3, 4, 5)), round(List(6, 7, 8, 9, 10))))

    pooled should have size 1
    val row = pooled.head
    (row.renderer, row.api, row.scenario, row.frames) shouldBe ("skia", "OPENGL", "typing", 10)
    (row.p50Ms, row.p95Ms, row.p99Ms) shouldBe (5.0, 10.0, 10.0)
    row.allocatedKiB shouldBe 4.0
  }

  "table" should "print one header, one rule, a line per row and why a renderer is missing" in {
    val row   = Row("java2d", "n/a", "typing", 200, 3.456, 5.0, 7.891, 812.4)
    val lines = table(List(row), List(Unavailable("skia-raster", "Wayland only")))

    lines.head should include("renderer")
    lines.head should include("p99 ms")
    lines(1).forall(c => c == '-' || c == ' ') shouldBe true
    lines(2) should (include("java2d") and include("3.46") and include("7.89") and include("812"))
    lines.last shouldBe "skia-raster: unavailable here, Wayland only"
    lines should have size 4
  }
