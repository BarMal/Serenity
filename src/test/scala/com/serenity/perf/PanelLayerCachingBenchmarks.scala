package com.serenity.perf

import cats.effect.unsafe.IORuntime
import com.serenity.rope.Balance
import com.serenity.state.manager.DamageProducer
import com.serenity.state.manager.StateManagerTestFacade.updateState
import com.serenity.state.models.{AppState, Damage, SurfaceContent, SurfaceId}
import com.serenity.ui.layout.PanelPosition
import com.serenity.{DockedPanelFixtures, FramePixels}

/** Frames with a docked outline over the laptop prose, drawn with panel layer caching off and on (#1869). The two
  * variants are named apart so a run can take either by prefix (`laptop.render.pinned_panel.layer_cache_off`) and the
  * runs be interleaved A/B/A/B across processes under identical display and JVM settings.
  *
  * Three frames cover what caching changes: a typing frame, where the panel's pixels are kept; an idle frame, where
  * nothing repainted; and a frame whose damage is the panel itself.
  */
private[perf] object PanelLayerCachingBenchmarks:

  given Balance = Balance.default

  private val outlineId = SurfaceId("outline")

  def benchmarks(using IORuntime): List[BenchmarkRunner.Benchmark] =
    List(false, true).flatMap(variant)

  private def variant(layerCaching: Boolean)(using IORuntime): List[BenchmarkRunner.Benchmark] =
    val prefix                 = s"laptop.render.pinned_panel.layer_cache_${if layerCaching then "on" else "off"}"
    lazy val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager()
    lazy val docked =
      stateManager
        .updateState(DockedPanelFixtures.dock(_, outlineId, SurfaceContent.Outline(Nil), PanelPosition.Right, 40))
        .unsafeRunSync()
    lazy val (before, after) =
      val _ = docked
      TypingTransition.of(stateManager)
    lazy val typingBefore = withCaching(before, layerCaching)
    lazy val typingAfter  = withCaching(after, layerCaching)
    lazy val forward      = DamageProducer.forTransition(typingBefore, typingAfter)
    lazy val backward     = DamageProducer.forTransition(typingAfter, typingBefore)

    def benchmark(name: String, steps: => List[(AppState, Damage)]): BenchmarkRunner.Benchmark =
      lazy val frames = Alternating(steps)
      BenchmarkRunner.Benchmark.building(frames)(
        BenchmarkRunner.Benchmark(
          s"$prefix.$name.skia_raster",
          2,
          12,
          () => assert(LaptopFrameBenchmarks.hasPixels(frames.next()), s"$name drew nothing"),
          () => frames.next()
        )
      )

    List(
      benchmark("typing_frame_2x", List(typingAfter -> forward, typingBefore -> backward)),
      benchmark("idle_frame_2x", List(typingAfter -> Damage.Nothing)),
      benchmark("panel_damaged_frame_2x", List(typingAfter -> Damage.Surface(outlineId)))
    )

  private def withCaching(state: AppState, enabled: Boolean): AppState =
    state.copy(persisted = state.persisted.copy(config = state.persisted.config.withLayerCaching(enabled)))

  /** The first frame is drawn in full; after that each call draws the next step over the same persisting image. */
  final private class Alternating(steps: List[(AppState, Damage)]):
    private val frames    = LaptopFrameBenchmarks.PersistingFrames()
    private val remaining = Iterator.continually(steps).flatten
    private val _         = steps.headOption.foreach((state, _) => frames.render(state, Damage.Everything))

    def next(): FramePixels =
      val (state, damage) = remaining.next()
      frames.render(state, damage)
