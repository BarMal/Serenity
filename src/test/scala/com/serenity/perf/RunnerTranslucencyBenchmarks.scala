package com.serenity.perf

import java.awt.image.BufferedImage

import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.theme.Theme

/** The command runner over the laptop prose, with an opaque panel and with a translucent one (panel background alpha),
  * so the cost of blending the runner with the editor behind it is a measured difference. Each variant draws the runner
  * opening in full, then alternates the selection so only the runner's own surface is damaged.
  */
private[perf] object RunnerTranslucencyBenchmarks:

  given Balance = Balance.default

  private val runnerId         = SurfaceId("command-runner")
  private val registry         = CommandRegistry.default
  private val TranslucentAlpha = 110

  def benchmarks: List[BenchmarkRunner.Benchmark] =
    List("opaque" -> 255, "translucent" -> TranslucentAlpha).map { (label, alpha) =>
      val first  = withRunner(alpha, selection = 0)
      val second = withRunner(alpha, selection = 1)
      val frames = Alternating(
        List(
          second -> DamageProducer.forTransition(first, second),
          first  -> DamageProducer.forTransition(second, first)
        ),
        first
      )
      BenchmarkRunner.Benchmark(
        s"laptop.render.command_runner.$label.navigate_frame_2x",
        2,
        12,
        () => assert(LaptopFrameBenchmarks.hasPixels(frames.next()), s"$label runner drew nothing"),
        () => frames.next()
      )
    }

  private def withRunner(panelAlpha: Int, selection: Int): AppState =
    val base   = LaptopFrameBenchmarks.proseState
    val runner = (1 to selection).foldLeft(opened)((current, _) => current.moveSelection(1))
    val theme  = Theme.dark
    base.copy(
      persisted = base.persisted.copy(
        focus = Focus.Surface(runnerId),
        theme = theme.copy(panel = theme.panel.copy(background = theme.panel.background.withAlpha(panelAlpha)))
      ),
      runtime = base.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            runnerId,
            SurfaceContent.CommandPalette(runner),
            SurfacePresentation.Floating(Some(CursorPosition(150, 40)), SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  private def opened: CommandRunner =
    CommandRunner.empty.activate(registry, AppConfig.default).updateSearchTerm("")(using registry)

  final private class Alternating(steps: List[(AppState, Damage)], opening: AppState):
    private val frames    = LaptopFrameBenchmarks.PersistingFrames()
    private val remaining = Iterator.continually(steps).flatten
    private val _         = frames.render(opening, Damage.Everything)

    def next(): BufferedImage =
      val (state, damage) = remaining.next()
      frames.render(state, damage)
