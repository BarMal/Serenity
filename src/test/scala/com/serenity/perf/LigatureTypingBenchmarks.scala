package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.serenity.keystroke.events.InsertChar
import com.serenity.perf.LaptopFrameBenchmarks.{LogicalHeightPx, LogicalWidthPx}
import com.serenity.rope.Balance
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.{AppState, Damage}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import org.typelevel.log4cats.noop.NoOpLogger

/** `laptop.render.typing_frame_2x` with a ligature font, and with a letter the frame has never seen on every call.
  *
  * Its alternating twin undoes each keystroke, so after the first pair the edited row's layout and shaping come from
  * cache; here every keystroke is new text, which a ligature font must lay out and shape afresh. One call is the
  * keystroke through the `StateManager` and the damaged frame it reports, as a session meets them.
  */
private[perf] object LigatureTypingBenchmarks:

  given Balance = Balance.default

  /** The bundled code font as the app loads it, ligatures on, for both the code and the text font of the frame. */
  private def ligatureFont(using IORuntime): Font =
    FontLoader
      .loadCodeFont(FontConfig(fontSize = LaptopFrameBenchmarks.codeFont.getSize2D))(using NoOpLogger[IO])
      .unsafeRunSync()

  def benchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
    val font              = ligatureFont
    val metrics           = CellMetrics.fromFont(font)
    val grid              = ViewportSize(LogicalWidthPx / metrics.charWidth, LogicalHeightPx / metrics.lineHeight)
    val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager(grid = grid)
    def current: AppState = stateManager.getCurrentState.unsafeRunSync()
    val frames            = LaptopFrameBenchmarks.PersistingFrames(font, font, metrics, grid)
    val shown             = AtomicReference(current)
    val _                 = frames.render(shown.get, Damage.Everything)
    val letters           = BenchmarkFixtures.randomLetters(seed = 42L)
    def typeAndShow(): BufferedImage =
      stateManager.applyEvent(InsertChar(letters.next())).unsafeRunSync()
      val after  = current
      val before = shown.getAndSet(after)
      frames.render(after, DamageProducer.forTransition(before, after))
    List(
      BenchmarkRunner.Benchmark(
        "laptop.render.typing_frame_2x.ligature_font",
        3,
        BenchmarkIterationCounts.RandomTyping,
        () =>
          assert(FontLoader.ligaturesEnabled(font), "the benchmark font has no ligatures on")
          assert(LaptopFrameBenchmarks.hasPixels(typeAndShow()), "typing a letter drew nothing")
        ,
        () => typeAndShow(),
        fixedSampleCount = true
      )
    )
