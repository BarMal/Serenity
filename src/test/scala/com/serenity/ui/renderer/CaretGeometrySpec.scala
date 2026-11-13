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

  it should "have no shadow shapes unless the caret is shadowed" in {
    CaretGeometry.shadowPolygons(body, CaretStyle.Plain) shouldBe Nil
    CaretGeometry.shadowPolygons(body, CaretStyle(italic = true, underline = true)) shouldBe Nil
  }

  it should "shadow the caret body one pixel down and to the right" in {
    CaretGeometry.shadowPolygons(body, CaretStyle(shadow = true)).map(_.points) shouldBe
      List(Vector((101.0, 71.0), (103.0, 71.0), (103.0, 51.0), (101.0, 51.0)))
    CaretGeometry.bounds(body, CaretStyle(shadow = true)) shouldBe new Rectangle(100, 50, 3, 21)
  }

  it should "shadow the slanted and widened body, and not its rules" in {
    val style = CaretStyle(bold = true, italic = true, underline = true, strikethrough = true, shadow = true)

    CaretGeometry.shadowPolygons(body, style).map(_.points) shouldBe
      List(Vector((101.0, 71.0), (105.0, 71.0), (109.0, 51.0), (105.0, 51.0)))
  }

  it should "cover every shape of a caret with all four styles in its bounds" in {
    val style  = CaretStyle(italic = true, bold = true, underline = true, strikethrough = true, shadow = true)
    val bounds = CaretGeometry.bounds(body, style)

    for
      polygon <- CaretGeometry.polygons(body, style) ++ CaretGeometry.shadowPolygons(body, style)
      (x, y)  <- polygon.points
    do
      x should (be >= bounds.getMinX and be <= bounds.getMaxX)
      y should (be >= bounds.getMinY and be <= bounds.getMaxY)
  }

  it should "scale its rules with the caret, never thinner than a pixel" in {
    val tall = new Rectangle(0, 0, 8, 100)
    CaretGeometry.bounds(tall, CaretStyle(underline = true)).getWidth shouldBe 50.0
    CaretGeometry.bounds(new Rectangle(0, 0, 1, 2), CaretStyle(underline = true)).getHeight shouldBe 2.0
  }
end CaretGeometrySpec
