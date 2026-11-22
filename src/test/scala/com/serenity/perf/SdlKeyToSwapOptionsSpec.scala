package com.serenity.perf

import scala.concurrent.duration.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The `-D` properties of the SDL3 key-to-swap benchmark (#2380). A typo must not quietly run something else, because
  * the run is read as a measurement.
  */
class SdlKeyToSwapOptionsSpec extends AnyFlatSpec with Matchers:

  private def parse(properties: (String, String)*): Either[String, SdlKeyToSwapBenchmark.Options] =
    SdlKeyToSwapBenchmark.Options.parse(properties.toMap)

  "The options" should "default to 200 typed keys, in process, at both swap intervals" in {
    parse() shouldBe Right(
      SdlKeyToSwapBenchmark.Options(
        keys = 200,
        warmupKeys = 40,
        spacing = 5.millis,
        injector = SdlKeyToSwapBenchmark.Injector.InProcess,
        swapIntervals = List(1, 0)
      )
    )
  }

  it should "read every property" in {
    parse(
      "serenity.sdl3.keys"         -> "60",
      "serenity.sdl3.keys.warmup"  -> "5",
      "serenity.sdl3.keys.spacing" -> "12",
      "serenity.sdl3.keys.driver"  -> "compositor",
      "serenity.sdl3.swap"         -> "0"
    ) shouldBe Right(
      SdlKeyToSwapBenchmark.Options(
        keys = 60,
        warmupKeys = 5,
        spacing = 12.millis,
        injector = SdlKeyToSwapBenchmark.Injector.Compositor,
        swapIntervals = List(0)
      )
    )
  }

  it should "accept a list of swap intervals, in the order given" in {
    parse("serenity.sdl3.swap" -> "0, 1").map(_.swapIntervals) shouldBe Right(List(0, 1))
  }

  it should "refuse a key count that is not a positive number" in {
    parse("serenity.sdl3.keys" -> "many").left.map(_.contains("serenity.sdl3.keys")) shouldBe Left(true)
    parse("serenity.sdl3.keys" -> "0").isLeft shouldBe true
    parse("serenity.sdl3.keys" -> "-3").isLeft shouldBe true
  }

  it should "refuse a warm-up that is negative, but allow none" in {
    parse("serenity.sdl3.keys.warmup" -> "-1").isLeft shouldBe true
    parse("serenity.sdl3.keys.warmup" -> "0").map(_.warmupKeys) shouldBe Right(0)
  }

  it should "refuse a spacing that is not a positive number of milliseconds" in {
    parse("serenity.sdl3.keys.spacing" -> "0").isLeft shouldBe true
    parse("serenity.sdl3.keys.spacing" -> "fast").isLeft shouldBe true
  }

  it should "refuse an injector it does not know, naming the ones it does" in {
    val refusal = parse("serenity.sdl3.keys.driver" -> "xdotool")

    refusal.isLeft shouldBe true
    refusal.left.getOrElse("") should (include("inprocess") and include("compositor"))
  }

  it should "refuse a swap interval other than 0 or 1" in {
    parse("serenity.sdl3.swap" -> "2").isLeft shouldBe true
    parse("serenity.sdl3.swap" -> "1,x").isLeft shouldBe true
    parse("serenity.sdl3.swap" -> "").isLeft shouldBe true
  }
