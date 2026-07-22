package com.serenity

import com.serenity.animation.AnimationConfig
import com.serenity.config.AppConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MotionLanguageSpec extends AnyFlatSpec with Matchers:

  "Serenity motion scale" should "use a short shared progression for subtle, smooth, and expressive motion" in {
    val durations = List(AnimationConfig.subtle, AnimationConfig.smooth, AnimationConfig.quick).flatten.map(_.durationMs)

    durations shouldBe List(80, 160, 240)
  }

  it should "make typing immediate while retaining the configurable reduced-motion policy" in {
    val config = AppConfig.default

    config.characterAnimation shouldBe AnimationConfig.none
    config.scaledCharacterAnimation shouldBe AnimationConfig.none
  }
end MotionLanguageSpec
