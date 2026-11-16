package com.serenity.app

import com.serenity.config.PreferredWindowSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TrialWindowSizeSpec extends AnyFlatSpec with Matchers:

  "TrialWindowSize.forScreen" should "fill the screen, less the width the resize step grows the window by" in {
    TrialWindowSize.forScreen(3000, 2000, resizeStep = 80) shouldBe PreferredWindowSize(2920, 2000)
  }

  it should "never go below the smallest window the editor allows" in {
    TrialWindowSize.forScreen(300, 200, resizeStep = 80) shouldBe PreferredWindowSize(400, 300)
  }

  it should "leave room for the resize step's growth on the screens it can" in {
    val size = TrialWindowSize.forScreen(1920, 1080, resizeStep = 80)
    size.width + 80 shouldBe 1920
    size.height shouldBe 1080
  }
