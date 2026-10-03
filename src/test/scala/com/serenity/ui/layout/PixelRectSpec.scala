package com.serenity.ui.layout

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PixelRectSpec extends AnyFlatSpec with Matchers:

  "PixelRect.intersects" should "hold for overlapping rectangles" in {
    PixelRect(0, 0, 10, 10).intersects(PixelRect(5, 5, 10, 10)) shouldBe true
  }

  it should "be symmetric" in {
    val a = PixelRect(0, 0, 10, 10)
    val b = PixelRect(5, 5, 10, 10)
    a.intersects(b) shouldBe b.intersects(a)
  }

  it should "not hold for disjoint rectangles" in {
    PixelRect(0, 0, 10, 10).intersects(PixelRect(20, 20, 5, 5)) shouldBe false
  }

  it should "not hold for rectangles that only touch at an edge" in {
    PixelRect(0, 0, 10, 10).intersects(PixelRect(10, 0, 10, 10)) shouldBe false
  }

  it should "hold when one rectangle fully contains the other" in {
    PixelRect(0, 0, 20, 20).intersects(PixelRect(5, 5, 2, 2)) shouldBe true
  }

  "PixelRect.uncoveredWithin" should "return the bounds whole when nothing covers them" in {
    PixelRect.uncoveredWithin(PixelRect(0, 0, 10, 10), Nil) shouldBe List(PixelRect(0, 0, 10, 10))
  }

  it should "return nothing when a hole covers the bounds entirely" in {
    PixelRect.uncoveredWithin(PixelRect(0, 0, 10, 10), List(PixelRect(-5, -5, 30, 30))) shouldBe Nil
  }

  it should "cover exactly the pixels no hole covers, with no overlap" in {
    val bounds = PixelRect(0, 0, 40, 30)
    val holes = List(
      PixelRect(5, 5, 10, 4),
      PixelRect(5, 9, 30, 4),
      PixelRect(30, 20, 20, 20),
      PixelRect(12, 6, 3, 3),
      PixelRect(-4, 25, 8, 2),
      PixelRect(10, 15, 0, 5)
    )
    val pieces = PixelRect.uncoveredWithin(bounds, holes)
    val coverage =
      for
        x <- bounds.xPx until bounds.rightPx
        y <- bounds.yPx until bounds.bottomPx
      yield
        val point = PixelRect(x, y, 1, 1)
        (holes.exists(_.intersects(point)), pieces.count(_.intersects(point)))
    coverage.foreach((inHole, pieceCount) => pieceCount shouldBe (if inHole then 0 else 1))
    pieces.forall(piece => piece.widthPx > 0 && piece.heightPx > 0) shouldBe true
  }

  it should "merge full-width bands between stacked row holes into single rectangles" in {
    val rows   = (0 until 10).map(row => PixelRect(0, row * 10, 100, 9)).toList
    val pieces = PixelRect.uncoveredWithin(PixelRect(0, 0, 100, 100), rows)
    pieces.size shouldBe 10
  }
