package com.serenity.app

import java.nio.file.Path

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LaunchRefusalSpec extends AnyFlatSpec with Matchers:

  private def options(args: String*): LaunchOptions =
    LaunchOptions.parse(args.toList).fold(help => fail(help.toString), identity)

  private val home = Path.of("/nonexistent-home-for-the-refusal-spec")

  "LaunchRefusal" should "refuse a start-up probe outside safe mode and an unmarked home" in {
    LaunchRefusal(options("--startup-probe"), home).exists(_.contains("--startup-probe")) shouldBe true
  }

  it should "refuse a renderer trial outside safe mode" in {
    LaunchRefusal(options("--renderer-trial=layer+image"), home).exists(_.contains("--renderer-trial")) shouldBe true
  }

  it should "let a probe or a trial in safe mode, and an ordinary launch, through" in {
    LaunchRefusal(options("--startup-probe", "--safe-mode"), home) shouldBe None
    LaunchRefusal(options("--renderer-trial=skia-raster", "--safe-mode"), home) shouldBe None
    LaunchRefusal(options("notes.md"), home) shouldBe None
  }
