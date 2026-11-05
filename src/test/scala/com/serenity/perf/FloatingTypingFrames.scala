package com.serenity.perf

import cats.effect.unsafe.IORuntime
import com.serenity.ChromeOn
import com.serenity.config.DockedCornerStyle

/** The typing frame in floating, rounded chrome, which has no flush base to compare with. */
private[perf] object FloatingTypingFrames:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  def benchmark()(using IORuntime): BenchmarkRunner.Benchmark =
    val (manager, _) = LaptopFrameBenchmarks.proseStateManager(
      config = ChromeOn.config.withDockedCornerStyle(DockedCornerStyle.Floating)
    )
    val (before, after) = TypingTransition.of(manager)
    val typing          = LaptopFrameBenchmarks.AlternatingTransition(before, after)
    BenchmarkRunner.Benchmark(
      "laptop.render.typing_frame_floating_2x",
      2,
      12,
      () => typing.verify("floating typing", LaptopFrameBenchmarks.MaxTypingRowsDrawn),
      () => typing.nextFrame()
    )
