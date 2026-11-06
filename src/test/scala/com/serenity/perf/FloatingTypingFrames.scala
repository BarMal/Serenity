package com.serenity.perf

import cats.effect.unsafe.implicits.global
import com.serenity.config.DockedCornerStyle
import com.serenity.{ChromeOn, Java2DFrames}

/** The typing frame in floating, rounded chrome, which has no flush base to compare with. */
private[perf] object FloatingTypingFrames:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  def benchmark(): BenchmarkRunner.Benchmark =
    lazy val typing =
      val (manager, _)    = LaptopFrameBenchmarks.proseStateManager(
        config = ChromeOn.config.withDockedCornerStyle(DockedCornerStyle.Floating)
      )
      val (before, after) = TypingTransition.of(manager)
      LaptopFrameBenchmarks.AlternatingTransition(Java2DFrames, before, after)
    BenchmarkRunner.Benchmark.building(typing)(
      BenchmarkRunner.Benchmark(
        "laptop.render.typing_frame_floating_2x",
        2,
        12,
        () => typing.verify("floating typing", LaptopFrameBenchmarks.MaxTypingRowsDrawn),
        () => typing.nextFrame()
      )
    )
