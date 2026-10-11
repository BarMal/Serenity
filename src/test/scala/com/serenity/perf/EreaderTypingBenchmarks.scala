package com.serenity.perf

import java.awt.image.BufferedImage

import cats.effect.unsafe.IORuntime
import com.serenity.config.AppConfig
import com.serenity.diagnostics.FrameCounter
import com.serenity.rope.Balance
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.Damage

/** A typing frame in an e-reader pane: 40-cell columns, each with its own line-number rail (#1863). The keystroke
  * reflows its paragraph, which wraps into some sixteen rows across the columns; every other row, and its rail band, is
  * kept.
  */
private[perf] object EreaderTypingBenchmarks:

  given Balance = Balance.default

  private val MaxRowsDrawn = 20L

  private val config = AppConfig.default.withColumnMode(true).withColumnTargetWidth(40).withLineNumbers(true)

  def benchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
    val (before, after) = TypingTransition.of(LaptopFrameBenchmarks.proseStateManager(config = config)._1)
    val forward         = DamageProducer.forTransition(before, after)
    val backward        = DamageProducer.forTransition(after, before)
    val frames          = LaptopFrameBenchmarks.PersistingFrames()
    val steps           = Iterator.continually(List(after -> forward, before -> backward)).flatten
    val _               = frames.render(before, Damage.Everything)

    def next(): BufferedImage =
      val (state, damage) = steps.next()
      frames.render(state, damage)

    def rowsDrawn(): Long =
      frames.counting {
        val _ = next()
        frames.lastFrame(FrameCounter.RowsDrawn)
      }

    List(
      BenchmarkRunner.Benchmark(
        "laptop.render.typing_frame_2x.ereader_railed",
        2,
        12,
        () =>
          assert(forward != Damage.Everything, "typing damaged the whole frame")
          assert(LaptopFrameBenchmarks.hasPixels(next()), "e-reader frame drew nothing")
          val drawn = rowsDrawn()
          assert(drawn <= MaxRowsDrawn, s"the typing frame redrew $drawn rows, more than the $MaxRowsDrawn allowed")
        ,
        () => next()
      )
    )
