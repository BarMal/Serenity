package com.serenity.perf

import java.util.concurrent.atomic.AtomicReference

import cats.effect.unsafe.IORuntime
import com.serenity.SkiaFrames
import com.serenity.diagnostics.{FrameCounter, FrameCounts}
import com.serenity.keystroke.events.{Event, PageDown, ScrollDown}
import com.serenity.rope.Balance
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.benchmark.ShapingTraffic
import com.serenity.ui.layout.{BundledInterFont, CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, RendererFramePlanner, ScreenIdentity}
import com.serenity.ui.tui.TuiFrameCountScenario

/** The fixed interactions whose frame counts the ratchet pins: what the planner decided for each, measured on Skia CPU
  * raster so no window or GPU is involved, plus one typing frame on the terminal surface.
  *
  * Every font is the bundled Inter subset, so the cell grid, the wraps and so the rows a frame draws do not depend on
  * which font the platform maps a logical family to.
  */
private[perf] object FrameCountScenarios:

  given Balance = Balance.default

  val LogicalWidthPx  = 1500
  val LogicalHeightPx = 1000

  private val codeFont    = BundledInterFont.plain(12f)
  private val textFont    = BundledInterFont.plain(14f)
  private val uiFont      = BundledInterFont.plain(12f)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)
  private val viewport = ViewportSize(LogicalWidthPx / cellMetrics.charWidth, LogicalHeightPx / cellMetrics.lineHeight)

  /** Reported for every surface scenario; the terminal frame also reports [[FrameCounter.TuiBytes]]. */
  val SurfaceCounters: List[FrameCounter] = List(
    FrameCounter.RowsDrawn,
    FrameCounter.RowsPreserved,
    FrameCounter.RunsDrawn,
    FrameCounter.ShapingCalls,
    FrameCounter.PresentAreaPx,
    FrameCounter.ChromePainted
  )

  val IdleScenario = "idle_frame"

  /** One scenario's name and the counters its frame reported. */
  final case class Counted(scenario: String, counters: List[FrameCounter], counts: FrameCounts):

    def entries: Vector[(String, Long)] =
      counters.toVector.map(counter => s"$scenario.${counter.label}" -> counts(counter))

  /** A window's frame loop over one persisting Skia surface, counting every frame it draws. */
  final private class CountedFrames(scale: Double):
    private val frame  = SkiaFrames.newFrame(deviceSize(LogicalWidthPx, scale), deviceSize(LogicalHeightPx, scale))
    private val caches = RenderCaches.create()
    private val screen = ScreenIdentity(this)
    caches.frameCounters.setEnabled(true)

    def render(state: AppState, damage: Damage): FrameCounts =
      val surface = SkiaFrames.surfaceOver(
        frame,
        cellMetrics,
        codeFont,
        LogicalWidthPx,
        LogicalHeightPx,
        scale,
        persists = true,
        caches,
        Some(screen)
      )
      val _ = RendererFramePlanner.renderFrame(
        state,
        cursorVisible = false,
        surface,
        viewport,
        caches.authoritativeScene.forState(state, viewport, codeFont, textFont),
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        None,
        Some(FrameOutput(screen, new AtomicReference(None))),
        damage,
        caches = caches
      )
      caches.frameCounters.lastFrame

  /** Frames drawn from scratch over a surface that keeps nothing, the way a window's first paint is drawn. */
  final private class ScratchFrames(scale: Double):
    private val frame  = SkiaFrames.newFrame(deviceSize(LogicalWidthPx, scale), deviceSize(LogicalHeightPx, scale))
    private val caches = RenderCaches.create()
    caches.frameCounters.setEnabled(true)

    def render(state: AppState): Unit =
      val surface = SkiaFrames.surfaceOver(
        frame,
        cellMetrics,
        codeFont,
        LogicalWidthPx,
        LogicalHeightPx,
        scale,
        persists = false,
        caches,
        None
      )
      val _ = RendererFramePlanner.renderFrame(
        state,
        cursorVisible = false,
        surface,
        viewport,
        caches.authoritativeScene.forState(state, viewport, codeFont, textFont),
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        None,
        None,
        Damage.Everything,
        caches = caches
      )

    /** What shaping the frame just drawn paid for, as the render benchmark's shaping traffic reads it. */
    def shapings(): Long = ShapingTraffic.Frame.of(caches.frameCounters.drain()).shapings

  private def deviceSize(logicalPx: Int, scale: Double): Int = math.ceil(logicalPx * scale).toInt

  /** The prose document's state before and after `events`, from a live `StateManager` so the damage between them is
    * what the app reports for those events.
    */
  private def transition(events: Event*)(using IORuntime): (AppState, AppState) =
    val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager(grid = viewport)
    val before            = stateManager.getCurrentState.unsafeRunSync()
    events.foreach(event => stateManager.applyEvent(event).unsafeRunSync())
    (before, stateManager.getCurrentState.unsafeRunSync())

  private def counted(scenario: String, counts: FrameCounts): Counted = Counted(scenario, SurfaceCounters, counts)

  private def damagedFrame(scenario: String, scale: Double, events: Event*)(using IORuntime): Counted =
    val (before, after) = transition(events*)
    val frames          = CountedFrames(scale)
    val _               = frames.render(before, Damage.Everything)
    counted(scenario, frames.render(after, DamageProducer.forTransition(before, after)))

  private def typingFrame(scale: Double)(using IORuntime): Counted =
    val (before, after) = TypingTransition.of(LaptopFrameBenchmarks.proseStateManager(grid = viewport)._1)
    val frames          = CountedFrames(scale)
    val _               = frames.render(before, Damage.Everything)
    counted("type_one_char", frames.render(after, DamageProducer.forTransition(before, after)))

  private def firstFrame(scale: Double)(using IORuntime): Counted =
    val (before, _) = transition()
    counted("first_frame", CountedFrames(scale).render(before, Damage.Everything))

  private def idleFrame(scale: Double)(using IORuntime): Counted =
    val (before, _) = transition()
    val frames      = CountedFrames(scale)
    val _           = frames.render(before, Damage.Everything)
    counted(IdleScenario, frames.render(before, Damage.Nothing))

  /** The same state drawn twice from scratch with one set of caches: the first pays every shaping, the second none. */
  private def coldAndWarmFrames(scale: Double)(using IORuntime): List[Counted] =
    val (state, _) = transition()
    val frames     = ScratchFrames(scale)
    frames.render(state)
    val cold = frames.shapings()
    frames.render(state)
    val warm = frames.shapings()
    List("cold_frame" -> cold, "warm_frame" -> warm).map { (scenario, shapings) =>
      Counted(scenario, List(FrameCounter.ShapingCalls), FrameCounts(Map(FrameCounter.ShapingCalls -> shapings)))
    }

  private def tuiTypingFrame()(using IORuntime): Counted =
    Counted(
      "tui_typing_frame",
      SurfaceCounters :+ FrameCounter.TuiBytes,
      TuiFrameCountScenario.typingFrame.unsafeRunSync()
    )

  /** Every scenario at one device scale, in the order the baseline lists them. */
  def measure(scale: Double)(using IORuntime): Vector[Counted] =
    Vector(
      firstFrame(scale),
      idleFrame(scale),
      typingFrame(scale),
      damagedFrame("scroll_1_line", scale, ScrollDown(1)),
      damagedFrame("scroll_3_lines", scale, ScrollDown(3)),
      damagedFrame("page_down", scale, PageDown)
    ) ++ coldAndWarmFrames(scale) :+ tuiTypingFrame()

  /** Every count of every scenario, keyed `scenario.counter`. */
  def entries(scale: Double)(using IORuntime): Vector[(String, Long)] = measure(scale).flatMap(_.entries)
