package com.serenity.app

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OneShotSpec extends AnyFlatSpec with Matchers:

  private def options(args: String*): LaunchOptions =
    LaunchOptions.parse(args.toList).fold(help => fail(help.toString), identity)

  "OneShot" should "stand for the launches that do a job and quit without starting the editor" in {
    OneShot.unapply(options("--version")).isDefined shouldBe true
    OneShot.unapply(options("--optimize-renderer")).isDefined shouldBe true
  }

  it should "leave every other launch to start the editor" in {
    OneShot.unapply(options("notes.md")).isEmpty shouldBe true
    OneShot.unapply(options("--safe-mode")).isEmpty shouldBe true
    OneShot.unapply(options("--renderer-trial=skia-raster", "--safe-mode")).isEmpty shouldBe true
  }
