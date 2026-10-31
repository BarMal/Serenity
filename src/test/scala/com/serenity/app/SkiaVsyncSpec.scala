package com.serenity.app

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SkiaVsyncSpec extends AnyFlatSpec with Matchers:

  "SkiaVsync.property" should "set Skiko's vsync property to the configured value" in {
    SkiaVsync.property(enabled = false, env = Map.empty, properties = Map.empty) shouldBe
      Some("skiko.vsync.enabled" -> "false")
    SkiaVsync.property(enabled = true, env = Map.empty, properties = Map.empty) shouldBe
      Some("skiko.vsync.enabled" -> "true")
  }

  it should "leave the property alone when it was set on the command line" in {
    SkiaVsync.property(
      enabled = false,
      env = Map.empty,
      properties = Map("skiko.vsync.enabled" -> "true")
    ) shouldBe None
  }

  it should "leave Skiko alone when its own environment variable is set" in {
    SkiaVsync.property(
      enabled = false,
      env = Map("SKIKO_VSYNC_ENABLED" -> "true"),
      properties = Map.empty
    ) shouldBe None
  }

  it should "ignore blank overrides" in {
    SkiaVsync.property(
      enabled = true,
      env = Map("SKIKO_VSYNC_ENABLED" -> " "),
      properties = Map("skiko.vsync.enabled" -> "")
    ) shouldBe Some("skiko.vsync.enabled" -> "true")
  }
