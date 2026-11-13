package com.serenity.perf

import java.awt.Font
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.serenity.diagnostics.FrameCounts
import com.serenity.keystroke.events.{InsertChar, ScrollDown}
import com.serenity.rope.Balance
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.{AppState, Damage}
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, RendererFramePlanner, ScreenIdentity}
import com.serenity.{FrameBackend, FramePixels, SkiaFrames}
import org.typelevel.log4cats.noop.NoOpLogger

/** A typing frame and a scroll frame, drawn by Skia's CPU rasteriser, with a system monospace font and with the bundled
  * ligature font, in the same 1500x1000 logical window at 2x, so both meet the same states and the same damage.
  *
  * Not part of the perf gate: run it with `Test / runMain com.serenity.perf.SkiaFrameBenchmarks` (optionally followed
  * by benchmark name prefixes) under `-Dsun.java2d.uiScale=2`. The Skia numbers are for the raster surface the specs
  * use. A window's GPU canvas draws the same calls on the GPU, so they say what the surface's CPU-side work costs, not
  * what a GL or Metal frame costs.
  */
private[perf] object SkiaFrameBenchmarks:

  given Balance = Balance.default

  private val LogicalWidthPx  = LaptopFrameBenchmarks.LogicalWidthPx
  private val LogicalHeightPx = LaptopFrameBenchmarks.LogicalHeightPx
  private val DeviceScale     = LaptopFrameBenchmarks.DeviceScale
  private val textFont        = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont          = Font(Font.SANS_SERIF, Font.PLAIN, 12)

  /** A window's frame loop over one persisting image of `backend`, as `LaptopFrameBenchmarks.PersistingFrames` is:
    * every frame told what the transition into it damaged, so only damaged rows are redrawn.
    */
  final private[perf] class Frames(backend: FrameBackend, codeFont: Font, metrics: CellMetrics, grid: ViewportSize):
    private val frame     = backend.newFrame(deviceSize(LogicalWidthPx), deviceSize(LogicalHeightPx))
    private val caches    = RenderCaches.create()
    private val screen    = ScreenIdentity(this)
    private val uiMetrics = CellMetrics.fromFont(uiFont)

    private def deviceSize(logicalPx: Int): Int = math.ceil(logicalPx * DeviceScale).toInt

    def lastFrame: FrameCounts = caches.frameCounters.lastFrame

    /** Counts only the frames drawn in `work`, so the timed frames carry no counting at all. */
    def counting[A](work: => A): A =
      caches.frameCounters.setEnabled(true)
      try work
      finally caches.frameCounters.setEnabled(false)

    def render(state: AppState, damage: Damage): FramePixels =
      val surface = backend.surfaceOver(
        frame,
        metrics,
        codeFont,
        LogicalWidthPx,
        LogicalHeightPx,
        DeviceScale,
        true,
        caches
      )
      val _ = RendererFramePlanner.renderFrame(
        state,
        cursorVisible = false,
        surface,
        grid,
        caches.authoritativeScene.forState(state, grid, codeFont, textFont),
        codeFont,
        textFont,
        uiFont,
        metrics,
        uiMetrics,
        None,
        Some(FrameOutput(screen, new AtomicReference(None))),
        damage,
        caches = caches
      )
      frame

  private def hasPixels(frame: FramePixels): Boolean =
    val row = frame.heightPx / 2
    (0 until frame.widthPx).exists(x => frame.rgbAt(x, row) != frame.rgbAt(0, row))

  /** The first frame of a pair is a full one; the rest alternate `after` and `before`, each with its own damage. */
  final private class Alternating(frames: Frames, before: AppState, after: AppState):
    private val forward  = DamageProducer.forTransition(before, after)
    private val backward = DamageProducer.forTransition(after, before)
    private val steps    = Iterator.continually(List(after -> forward, before -> backward)).flatten
    private val _        = frames.render(before, Damage.Everything)

    def forwardDamage: Damage = forward

    def nextFrame(): FramePixels =
      val (state, damage) = steps.next()
      frames.render(state, damage)

  private def backendBenchmarks(label: String, backend: FrameBackend, codeFont: => Font)(using
    IORuntime
  ): List[BenchmarkRunner.Benchmark] =
    lazy val font    = codeFont
    lazy val metrics = CellMetrics.fromFont(font)
    lazy val grid =
      ViewportSize(LogicalWidthPx / metrics.charWidth, LogicalHeightPx / metrics.lineHeight)
    lazy val (stateManager, _)           = LaptopFrameBenchmarks.proseStateManager(grid = grid)
    def current: AppState                = stateManager.getCurrentState.unsafeRunSync()
    lazy val (beforeTyping, afterTyping) = TypingTransition.of(stateManager)
    lazy val afterScroll =
      val _ = afterTyping
      stateManager.applyEvent(ScrollDown(3)).unsafeRunSync()
      current
    lazy val typing = Alternating(Frames(backend, font, metrics, grid), beforeTyping, afterTyping)
    lazy val scroll = Alternating(Frames(backend, font, metrics, grid), afterTyping, afterScroll)
    val letters     = BenchmarkFixtures.randomLetters(seed = 42L)
    lazy val fresh  = Frames(backend, font, metrics, grid)
    lazy val shown  = AtomicReference(current)
    val primed      = AtomicBoolean(false)
    def typeAndShow(): FramePixels =
      if primed.compareAndSet(false, true) then
        val _ = fresh.render(shown.get, Damage.Everything)
      stateManager.applyEvent(InsertChar(letters.next())).unsafeRunSync()
      val next     = current
      val previous = shown.getAndSet(next)
      fresh.render(next, DamageProducer.forTransition(previous, next))
    BenchmarkRunner.Benchmark.sharing((typing, scroll, fresh, shown))(
      List(
        BenchmarkRunner.Benchmark(
          s"skia.compare.$label.typing_frame_2x",
          2,
          12,
          () =>
            assert(typing.forwardDamage != Damage.Everything, "typing damaged the whole frame")
            assert(hasPixels(typing.nextFrame()), "typing frame drew nothing")
          ,
          () => typing.nextFrame()
        ),
        BenchmarkRunner.Benchmark(
          s"skia.compare.$label.scroll_frame_2x",
          2,
          12,
          () =>
            assert(scroll.forwardDamage != Damage.Everything, "scrolling damaged the whole frame")
            assert(hasPixels(scroll.nextFrame()), "scroll frame drew nothing")
          ,
          () => scroll.nextFrame()
        ),
        BenchmarkRunner.Benchmark(
          s"skia.compare.$label.typing_new_letter_2x",
          3,
          BenchmarkIterationCounts.RandomTyping,
          () => assert(hasPixels(typeAndShow()), "typing a letter drew nothing"),
          () => typeAndShow(),
          fixedSampleCount = true
        )
      )
    )

  private def bundledFont(using IORuntime): Font =
    FontLoader
      .loadCodeFont(FontConfig(fontSize = LaptopFrameBenchmarks.codeFont.getSize2D))(using NoOpLogger[IO])
      .unsafeRunSync()

  def benchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
    val system = LaptopFrameBenchmarks.codeFont

    lazy val bundled = bundledFont
    backendBenchmarks("skia_raster.system_monospace", SkiaFrames, system) ++
      backendBenchmarks("skia_raster.bundled_ligature_font", SkiaFrames, bundled)

  def main(args: Array[String]): Unit =
    BenchmarkMain.run:
      import cats.effect.unsafe.implicits.global
      val results = BenchmarkRunner.runMatching(args.toList, benchmarks)
      BenchmarkRunner.printResults(results)
