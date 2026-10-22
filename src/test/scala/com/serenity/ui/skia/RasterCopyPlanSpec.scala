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
