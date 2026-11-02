package com.serenity.ui.skia

import com.serenity.ui.renderer.DeviceArea
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RasterCopyPlanSpec extends AnyFlatSpec with Matchers:

  private val whole  = DeviceArea(0, 0, 100, 80)
  private val top    = DeviceArea(0, 0, 100, 10)
  private val middle = DeviceArea(0, 30, 100, 10)
  private val bottom = DeviceArea(0, 70, 100, 10)

  private val imageA = new Object
  private val imageB = new Object
  private val imageC = new Object

  private def filled(image: AnyRef, damage: Option[List[DeviceArea]]) = RasterCopyPlan.Filled(image, damage)

  "RasterCopyPlan.areas" should "copy the whole frame when nothing has been shown before" in {
    RasterCopyPlan.areas(imageA, None, None, Some(List(top)), whole) shouldBe List(whole)
  }

  it should "copy the whole frame into an image it has not filled in the last two frames" in {
    val last       = Some(filled(imageA, Some(List(top))))
    val beforeLast = Some(filled(imageB, Some(List(top))))
    RasterCopyPlan.areas(imageC, last, beforeLast, Some(List(middle)), whole) shouldBe List(whole)
  }

  it should "copy this frame's damage and the previous frame's into the image from two frames ago" in {
    val last       = Some(filled(imageA, Some(List(middle))))
    val beforeLast = Some(filled(imageB, Some(List(top))))
    RasterCopyPlan.areas(imageB, last, beforeLast, Some(List(bottom)), whole) should contain theSameElementsAs
      List(bottom, middle)
  }

  it should "copy only this frame's damage into the image the previous frame filled" in {
    val last = Some(filled(imageA, Some(List(middle))))
    RasterCopyPlan.areas(imageA, last, None, Some(List(bottom)), whole) shouldBe List(bottom)
  }

  it should "copy the whole frame when this frame's damage is unknown" in {
    val last       = Some(filled(imageA, Some(List(middle))))
    val beforeLast = Some(filled(imageB, Some(List(top))))
    RasterCopyPlan.areas(imageB, last, beforeLast, None, whole) shouldBe List(whole)
  }

  it should "copy the whole frame when the previous frame's damage is unknown" in {
    val last       = Some(filled(imageA, None))
    val beforeLast = Some(filled(imageB, Some(List(top))))
    RasterCopyPlan.areas(imageB, last, beforeLast, Some(List(bottom)), whole) shouldBe List(whole)
  }

  it should "copy nothing when neither frame changed anything" in {
    val last       = Some(filled(imageA, Some(Nil)))
    val beforeLast = Some(filled(imageB, Some(Nil)))
    RasterCopyPlan.areas(imageB, last, beforeLast, Some(Nil), whole) shouldBe Nil
  }

  it should "leave out empty areas" in {
    val last = Some(filled(imageA, Some(Nil)))
    RasterCopyPlan.areas(imageA, last, None, Some(List(DeviceArea(5, 5, 0, 10), bottom)), whole) shouldBe List(bottom)
  }

  private def pixelsOf(areas: List[DeviceArea]): List[(Int, Int)] =
    areas.flatMap(area =>
      for y <- area.top until area.top + area.height; x <- area.left until area.left + area.width yield (x, y)
    )

  private def twoFramesOld(now: List[DeviceArea], previous: List[DeviceArea]): List[DeviceArea] =
    RasterCopyPlan.areas(
      imageB,
      Some(filled(imageA, Some(previous))),
      Some(filled(imageB, Some(previous))),
      Some(now),
      whole
    )

  it should "copy a scroll area once when both frames scrolled it" in {
    val scrolled = DeviceArea(0, 10, 100, 60)
    twoFramesOld(List(scrolled), List(scrolled)) shouldBe List(scrolled)
  }

  it should "copy the pixels two overlapping areas share once" in {
    val first  = DeviceArea(0, 0, 50, 50)
    val second = DeviceArea(25, 25, 50, 50)
    val copied = pixelsOf(twoFramesOld(List(first), List(second)))
    copied.distinct shouldBe copied
    copied.toSet shouldBe (pixelsOf(List(first)) ++ pixelsOf(List(second))).toSet
  }

  it should "join areas that touch into one" in {
    twoFramesOld(List(DeviceArea(0, 0, 100, 10)), List(DeviceArea(0, 10, 100, 10))) shouldBe
      List(DeviceArea(0, 0, 100, 20))
  }

  it should "join the pieces of one column that areas beside it split into bands" in {
    val column = List(DeviceArea(0, 0, 4, 10), DeviceArea(0, 10, 4, 10), DeviceArea(0, 20, 4, 10))
    val beside = DeviceArea(10, 10, 5, 5)
    RasterCopyPlan.disjoint(column :+ beside) should contain theSameElementsAs
      List(DeviceArea(0, 0, 4, 30), beside)
  }

  it should "copy exactly the union of any damage, with no pixel twice" in {
    val random = new scala.util.Random(2380L)
    def area() = DeviceArea(random.nextInt(60), random.nextInt(60), random.nextInt(40), random.nextInt(40))
    (1 to 300).foreach { _ =>
      val now      = List.fill(random.nextInt(5))(area())
      val previous = List.fill(random.nextInt(5))(area())
      val copied   = pixelsOf(twoFramesOld(now, previous))
      copied.distinct shouldBe copied
      copied.toSet shouldBe pixelsOf((now ++ previous).filterNot(_.isEmpty)).toSet
    }
  }
