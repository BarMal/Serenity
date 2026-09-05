package com.serenity.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VisualFlairLevelSpec extends AnyFlatSpec with Matchers:

  "VisualFlairLevel.fromConfigKey" should "parse every value's own config key" in {
    VisualFlairLevel.values.foreach { level =>
      VisualFlairLevel.fromConfigKey(level.configKey) shouldBe Some(level)
    }
  }

  it should "be case- and whitespace-insensitive" in {
    VisualFlairLevel.fromConfigKey(" Full ") shouldBe Some(VisualFlairLevel.Full)
    VisualFlairLevel.fromConfigKey("REDUCED") shouldBe Some(VisualFlairLevel.Reduced)
  }

  it should "accept common synonyms" in {
    VisualFlairLevel.fromConfigKey("disabled") shouldBe Some(VisualFlairLevel.Off)
    VisualFlairLevel.fromConfigKey("none") shouldBe Some(VisualFlairLevel.Off)
  }

  it should "reject an unknown value" in {
    VisualFlairLevel.fromConfigKey("ultra") shouldBe None
  }

  "VisualFlairLevel.default" should "be Full" in {
    VisualFlairLevel.default shouldBe VisualFlairLevel.Full
  }
