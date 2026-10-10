package com.serenity.ui.renderer

import scala.collection.mutable.ListBuffer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererSquiggleSpec extends AnyFlatSpec with Matchers:

  private def segments(startXPx: Int, widthPx: Int, topPx: Int, amplitude: Int): List[(Int, Int, Int)] =
    val found = ListBuffer.empty[(Int, Int, Int)]
    RendererHighlights.forEachSquiggleSegment(startXPx, widthPx, topPx, amplitude)((x, y, width) =>
      found += ((x, y, width))
    )
    found.toList

  "forEachSquiggleSegment" should "trace a zig-zag that goes down to the amplitude and back" in {
    segments(startXPx = 10, widthPx = 7, topPx = 100, amplitude = 2) shouldBe
      List((10, 100, 2), (12, 101, 2), (14, 102, 2), (16, 101, 1))
  }

  it should "climb back to the top after a full period" in {
    segments(startXPx = 0, widthPx = 16, topPx = 0, amplitude = 1).map(_._2) shouldBe List(0, 1, 0, 1, 0, 1, 0, 1)
  }

  it should "still mark a word narrower than one step" in {
    segments(startXPx = 5, widthPx = 0, topPx = 3, amplitude = 2) shouldBe List((5, 3, 1))
  }

  it should "cover the whole width without overlapping segments" in {
    val found = segments(startXPx = 4, widthPx = 33, topPx = 0, amplitude = 2)
    found.map(_._3).sum shouldBe 33
    found.zip(found.drop(1)).foreach { case ((x, _, width), (nextX, _, _)) => x + width shouldBe nextX }
  }
