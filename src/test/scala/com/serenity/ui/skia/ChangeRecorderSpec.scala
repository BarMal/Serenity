package com.serenity.ui.skia

import com.serenity.ui.renderer.DeviceArea
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChangeRecorderSpec extends AnyFlatSpec with Matchers:

  private def recorder = new ChangeRecorder(1.0, 1.0, 4000, 4000)

  private def pixelsOf(areas: List[DeviceArea]): Set[(Int, Int)] =
    areas
      .flatMap(area =>
        for y <- area.top until area.top + area.height; x <- area.left until area.left + area.width yield (x, y)
      )
      .toSet

  "A ChangeRecorder" should "record a column of cells that each continue the one above as a single area" in {
    val changes = recorder
    (0 until 1000).foreach(row => changes.painted(DeviceArea(5, row * 3, 4, 4)))
    changes.paintRecords shouldBe List(DeviceArea(5, 0, 4, 2997 + 4))
  }

  it should "record a row of cells that each continue the one before as a single area" in {
    val changes = recorder
    (0 until 1000).foreach(column => changes.painted(DeviceArea(column * 3, 7, 4, 5)))
    changes.paintRecords shouldBe List(DeviceArea(0, 7, 2997 + 4, 5))
  }

  it should "leave areas that do not continue each other as they are" in {
    val changes = recorder
    val areas =
      List(DeviceArea(0, 0, 4, 4), DeviceArea(0, 10, 4, 4), DeviceArea(20, 10, 4, 4), DeviceArea(21, 10, 3, 5))
    areas.foreach(changes.painted)
    pixelsOf(changes.paintRecords) shouldBe pixelsOf(areas)
  }

  it should "cover exactly the pixels it was told about, however the areas fall" in {
    val random  = new scala.util.Random(2383L)
    val changes = recorder
    val areas =
      List.fill(500)(DeviceArea(random.nextInt(60), random.nextInt(60), 1 + random.nextInt(6), 1 + random.nextInt(6)))
    areas.foreach(changes.painted)
    pixelsOf(changes.paintRecords) shouldBe pixelsOf(areas)
  }
