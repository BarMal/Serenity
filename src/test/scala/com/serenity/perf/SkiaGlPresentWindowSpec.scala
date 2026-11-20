package com.serenity.perf

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaGlPresentWindowSpec extends AnyFlatSpec with Matchers:

  private val floatRule = """WINDOW_TITLE='([^']*)'""".r

  "The present benchmark's window title" should "be matched in full by the title bench/hyprland-float.sh floats" in {
    val script = Files.readAllLines(Paths.get("bench", "hyprland-float.sh")).asScala
    val title  = script.flatMap(line => floatRule.findFirstMatchIn(line).map(_.group(1))).headOption
    val regex  = title.getOrElse(fail("hyprland-float.sh names no WINDOW_TITLE"))

    // Hyprland matches a rule's regex against the whole title, so a title with more after the pattern is not floated.
    SdlPresentWindow.Title.matches(regex) shouldBe true
  }

  "WindowRequest.parse" should "default to the laptop benchmarks' logical size" in {
    WindowRequest.parse(Map.empty) shouldBe
      WindowRequest.Logical(LaptopFrameBenchmarks.LogicalWidthPx, LaptopFrameBenchmarks.LogicalHeightPx)
  }

  it should "take a logical size given as WIDTHxHEIGHT" in {
    WindowRequest.parse(Map("serenity.sdl3.window" -> "1200x800")) shouldBe WindowRequest.Logical(1200, 800)
  }

  it should "take fullscreen, whatever the case and spacing" in
    List("fullscreen", " Fullscreen ", "FULLSCREEN").foreach { text =>
      WindowRequest.parse(Map("serenity.sdl3.window" -> text)) shouldBe WindowRequest.Fullscreen
    }

  it should "fall back to the default for a size it cannot read" in {
    val default = WindowRequest.parse(Map.empty)
    List("1200", "axb", "0x800", "-5x10", "", "full").foreach { text =>
      WindowRequest.parse(Map("serenity.sdl3.window" -> text)) shouldBe default
    }
  }

  "SizeSettling.settled" should "give the size once the last few readings agree" in {
    SizeSettling.settled(Vector((96, 64), (3000, 2000), (3000, 2000), (3000, 2000)), steady = 3) shouldBe
      Some((3000, 2000))
  }

  it should "give nothing while the size is still changing" in {
    SizeSettling.settled(Vector((3000, 2000), (3000, 1900), (3000, 2000)), steady = 3) shouldBe None
  }

  it should "give nothing before there are enough readings" in {
    SizeSettling.settled(Vector((3000, 2000), (3000, 2000)), steady = 3) shouldBe None
  }

  it should "not take the size it started at for the answer while it is asked to leave it" in {
    val before = Vector.fill(5)((1500, 1000))
    SizeSettling.settled(before, steady = 3, awayFrom = Some((1500, 1000))) shouldBe None
    SizeSettling.settled(before ++ Vector.fill(3)((3000, 2000)), steady = 3, awayFrom = Some((1500, 1000))) shouldBe
      Some((3000, 2000))
  }

  "SdlPresentWindow.sizeNote" should "say nothing when the window has the pixels that were asked for" in {
    SdlPresentWindow.sizeNote(
      requestedLogical = (1500, 1000),
      pixels = (3000, 2000),
      scale = 2.0,
      fix = "x"
    ) shouldBe None
  }

  it should "say so, and how to float the window, when the compositor gave it another size" in {
    val note = SdlPresentWindow.sizeNote(
      requestedLogical = (1500, 1000),
      pixels = (1480, 1852),
      scale = 2.0,
      fix = "float it with `bench/hyprland-float.sh on 1500 1000`"
    )
    note.getOrElse(fail("no note for a window of another size")) should (
      include("1480x1852") and include("3000x2000") and include("hyprland-float.sh on 1500 1000")
    )
  }

  it should "allow a pixel of rounding either way" in {
    SdlPresentWindow.sizeNote(
      requestedLogical = (1500, 1000),
      pixels = (3001, 1999),
      scale = 2.0,
      fix = "x"
    ) shouldBe None
  }
