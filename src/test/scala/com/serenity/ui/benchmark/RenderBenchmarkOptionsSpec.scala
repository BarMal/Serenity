package com.serenity.ui.benchmark

import com.serenity.app.LaunchOptions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RenderBenchmarkOptionsSpec extends AnyFlatSpec with Matchers:

  private def parsed(args: String*): LaunchOptions =
    LaunchOptions.parse(args.toList).fold(help => fail(help.toString), identity)

  "--render-benchmark" should "be off unless asked for" in {
    parsed().renderBenchmark shouldBe None
  }

  it should "run with the owner's defaults" in {
    parsed("--render-benchmark").renderBenchmark shouldBe
      Some(RenderBenchmarkOptions(widthPx = 1600, heightPx = 1000, frames = 200, warmup = 30))
  }

  it should "take a window size, a frame count and a warm-up" in {
    parsed(
      "--render-benchmark",
      "--render-benchmark-size",
      "1280x720",
      "--render-benchmark-frames",
      "50",
      "--render-benchmark-warmup",
      "5"
    ).renderBenchmark shouldBe Some(RenderBenchmarkOptions(1280, 720, 50, 5))
  }

  it should "reject a size that is not WxH" in
    List("1600", "1600x", "x900", "0x900", "axb", "1600x1000x2").foreach { size =>
      LaunchOptions.parse(List("--render-benchmark", "--render-benchmark-size", size)).isLeft shouldBe true
    }

  it should "reject a frame count or warm-up that is not a number, and a frame count of zero" in {
    LaunchOptions.parse(List("--render-benchmark", "--render-benchmark-frames", "many")).isLeft shouldBe true
    LaunchOptions.parse(List("--render-benchmark", "--render-benchmark-frames", "0")).isLeft shouldBe true
    LaunchOptions.parse(List("--render-benchmark", "--render-benchmark-warmup", "-1")).isLeft shouldBe true
  }

  it should "leave the other launch options as they were" in {
    val options = parsed("--render-benchmark", "--safe-mode")
    options.safeMode shouldBe true
    options.smokeTest shouldBe false
  }
