package com.serenity.ui.skia

import com.serenity.ui.renderer.DeviceArea
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DeviceAreasSpec extends AnyFlatSpec with Matchers:

  private def pixelsOf(areas: List[DeviceArea]): Set[(Int, Int)] =
    areas
      .flatMap(area =>
        for y <- area.top until area.top + area.height; x <- area.left until area.left + area.width yield (x, y)
      )
      .toSet

  private def randomAreas(random: scala.util.Random): List[DeviceArea] =
    List.fill(random.nextInt(5))(
      DeviceArea(random.nextInt(40), random.nextInt(40), random.nextInt(30), random.nextInt(30))
    )

  "Intersecting two lists of areas" should "hold the pixels both hold" in {
    val random = new scala.util.Random(1L)
    (1 to 300).foreach { _ =>
      val (a, b) = (randomAreas(random), randomAreas(random))
      pixelsOf(DeviceAreas.intersect(a, b)) shouldBe pixelsOf(a).intersect(pixelsOf(b))
    }
  }

  "Subtracting a list of areas" should "leave the pixels of the first that the second does not hold" in {
    val random = new scala.util.Random(2L)
    (1 to 300).foreach { _ =>
      val (a, b) = (randomAreas(random), randomAreas(random))
      pixelsOf(DeviceAreas.subtract(a, b)) shouldBe pixelsOf(a).diff(pixelsOf(b))
    }
  }

  it should "leave the first list alone when nothing overlaps" in {
    DeviceAreas.subtract(List(DeviceArea(0, 0, 10, 10)), List(DeviceArea(20, 20, 5, 5))) shouldBe
      List(DeviceArea(0, 0, 10, 10))
  }

  "Shifting an area" should "move it down by the rows given" in {
    DeviceAreas.shifted(DeviceArea(3, 10, 8, 4), -6) shouldBe DeviceArea(3, 4, 8, 4)
  }
