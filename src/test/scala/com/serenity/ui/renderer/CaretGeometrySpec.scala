package com.serenity.ui.renderer

import java.awt.Rectangle

import com.serenity.state.models.CaretStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The shapes of a styled caret, as pure geometry both backends draw. The body is the caret rect the layout placed. */
class CaretGeometrySpec extends AnyFlatSpec with Matchers:

  private val body = new Rectangle(100, 50, 2, 20)

  private def corners(style: CaretStyle): List[Vector[(Double, Double)]] =
    CaretGeometry.polygons(body, style).map(_.points)

  "CaretGeometry" should "leave a plain caret as its own rect" in {
    CaretGeometry.bounds(body, CaretStyle.Plain) shouldBe body
    corners(CaretStyle.Plain) shouldBe List(Vector((100.0, 70.0), (102.0, 70.0), (102.0, 50.0), (100.0, 50.0)))
  }

  it should "double the width of a bold caret, keeping its left edge and height" in {
    CaretGeometry.bounds(body, CaretStyle(bold = true)) shouldBe new Rectangle(100, 50, 4, 20)
  }

  it should "slant an italic caret about its base" in {
    corners(CaretStyle(italic = true)) shouldBe List(
      Vector((100.0, 70.0), (102.0, 70.0), (106.0, 50.0), (104.0, 50.0))
    )
    CaretGeometry.bounds(body, CaretStyle(italic = true)) shouldBe new Rectangle(100, 50, 6, 20)
  }

  it should "slant the widened body of a bold italic caret" in {
    corners(CaretStyle(bold = true, italic = true)) shouldBe List(
      Vector((100.0, 70.0), (104.0, 70.0), (108.0, 50.0), (104.0, 50.0))
    )
    CaretGeometry.bounds(body, CaretStyle(bold = true, italic = true)) shouldBe new Rectangle(100, 50, 8, 20)
  }

  it should "add a rule along the base of an underlined caret, centred on it" in {
    corners(CaretStyle(underline = true)).tail shouldBe List(
      Vector((96.0, 70.0), (106.0, 70.0), (106.0, 69.0), (96.0, 69.0))
    )
    CaretGeometry.bounds(body, CaretStyle(underline = true)) shouldBe new Rectangle(96, 50, 10, 20)
  }

  it should "add a rule through the middle of a struck-through caret" in {
    corners(CaretStyle(strikethrough = true)).tail shouldBe List(
      Vector((96.0, 60.0), (106.0, 60.0), (106.0, 59.0), (96.0, 59.0))
    )
    CaretGeometry.bounds(body, CaretStyle(strikethrough = true)) shouldBe new Rectangle(96, 50, 10, 20)
  }

  it should "cover every shape of a caret with all four styles in its bounds" in {
    val style  = CaretStyle(italic = true, bold = true, underline = true, strikethrough = true)
    val bounds = CaretGeometry.bounds(body, style)

    for
      polygon <- CaretGeometry.polygons(body, style)
      (x, y)  <- polygon.points
    do
      x should (be >= bounds.getMinX and be <= bounds.getMaxX)
      y should (be >= bounds.getMinY and be <= bounds.getMaxY)
  }

  it should "scale its rules with the caret, never thinner than a pixel" in {
    val tall = new Rectangle(0, 0, 8, 100)
    CaretGeometry.bounds(tall, CaretStyle(underline = true)).getWidth shouldBe 50.0 +- 0.0
    CaretGeometry.bounds(new Rectangle(0, 0, 1, 2), CaretStyle(underline = true)).getHeight shouldBe 2.0
  }
end CaretGeometrySpec
