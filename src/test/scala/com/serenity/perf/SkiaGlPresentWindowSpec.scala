package com.serenity.perf

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaGlPresentWindowSpec extends AnyFlatSpec with Matchers:

  private val floatRule = """WINDOW_TITLE='([^']*)'""".r

  "The present benchmark's window title" should "be one bench/hyprland-float.sh floats at the size it asks for" in {
    val script = Files.readAllLines(Paths.get("bench", "hyprland-float.sh")).asScala
    val title  = script.flatMap(line => floatRule.findFirstMatchIn(line).map(_.group(1))).headOption
    val regex  = title.getOrElse(fail("hyprland-float.sh names no WINDOW_TITLE"))

    SkiaGlPresentBenchmarks.WindowTitle.matches(s"$regex.*") shouldBe true
  }

  "SkiaGlPresentBenchmarks.requestedSize" should "default to the laptop benchmarks' logical size" in {
    SkiaGlPresentBenchmarks.requestedSize(Map.empty) shouldBe
      (LaptopFrameBenchmarks.LogicalWidthPx, LaptopFrameBenchmarks.LogicalHeightPx)
  }

  it should "take a logical size given as WIDTHxHEIGHT" in {
    SkiaGlPresentBenchmarks.requestedSize(Map("serenity.sdl3.window" -> "1200x800")) shouldBe (1200, 800)
  }

  it should "fall back to the default for a size it cannot read" in {
    val default = SkiaGlPresentBenchmarks.requestedSize(Map.empty)
    List("1200", "axb", "0x800", "-5x10", "").foreach { text =>
      SkiaGlPresentBenchmarks.requestedSize(Map("serenity.sdl3.window" -> text)) shouldBe default
    }
  }

  "SkiaGlPresentBenchmarks.sizeNote" should "say nothing when the window has the pixels that were asked for" in {
    SkiaGlPresentBenchmarks.sizeNote(requestedLogical = (1500, 1000), pixels = (3000, 2000), scale = 2.0) shouldBe None
  }

  it should "say so, and how to float the window, when the compositor gave it another size" in {
    val note = SkiaGlPresentBenchmarks.sizeNote(requestedLogical = (1500, 1000), pixels = (1480, 1852), scale = 2.0)
    note.getOrElse(fail("no note for a window of another size")) should (
      include("1480x1852") and include("3000x2000") and include("hyprland-float.sh on 1500 1000")
    )
  }

  it should "allow a pixel of rounding either way" in {
    SkiaGlPresentBenchmarks.sizeNote(requestedLogical = (1500, 1000), pixels = (3001, 1999), scale = 2.0) shouldBe None
  }
