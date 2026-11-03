package com.serenity.ui.skia

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaPhysicalFamilySpec extends AnyFlatSpec with Matchers:

  "SkiaTypefaces.physicalFamilyFor" should "name the families AWT's logical fonts are drawn in on Windows" in {
    SkiaTypefaces.physicalFamilyFor("Serif", "Windows 11") shouldBe Some("Times New Roman")
    SkiaTypefaces.physicalFamilyFor("SansSerif", "Windows 11") shouldBe Some("Arial")
    SkiaTypefaces.physicalFamilyFor("Dialog", "Windows 11") shouldBe Some("Arial")
    SkiaTypefaces.physicalFamilyFor("Monospaced", "Windows 11") shouldBe Some("Courier New")
    SkiaTypefaces.physicalFamilyFor("DialogInput", "Windows 11") shouldBe Some("Courier New")
  }

  it should "name the families AWT's logical fonts are drawn in on macOS" in {
    SkiaTypefaces.physicalFamilyFor("Serif", "Mac OS X") shouldBe Some("Times")
    SkiaTypefaces.physicalFamilyFor("SansSerif", "Mac OS X") shouldBe Some("Helvetica")
    SkiaTypefaces.physicalFamilyFor("Monospaced", "Mac OS X") shouldBe Some("Courier")
  }

  it should "leave Linux to fontconfig and unknown families alone" in {
    SkiaTypefaces.physicalFamilyFor("Serif", "Linux") shouldBe None
    SkiaTypefaces.physicalFamilyFor("Georgia", "Windows 11") shouldBe None
  }
