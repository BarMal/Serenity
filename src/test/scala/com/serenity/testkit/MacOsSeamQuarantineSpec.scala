package com.serenity.testkit

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MacOsSeamQuarantineSpec extends AnyFlatSpec with Matchers:

  "The macOS seam quarantine" should "apply on macOS" in {
    MacOsSeamQuarantine.appliesOn("Mac OS X") shouldBe true
  }

  it should "not apply on Linux" in {
    MacOsSeamQuarantine.appliesOn("Linux") shouldBe false
  }

  it should "not apply on any Windows" in
    List("Windows 11", "Windows 10", "Windows Server 2022").foreach { os =>
      withClue(os)(MacOsSeamQuarantine.appliesOn(os) shouldBe false)
    }

  it should "say where it is tracked, so a skipped run explains itself" in {
    MacOsSeamQuarantine.Reason should include("issues/2634")
  }
