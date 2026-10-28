package com.serenity.perf

import cats.effect.unsafe.IORuntime
import com.serenity.config.{AppConfig, LayoutPreset, SpellCheckConfig, StatusLinePlacement, TabStripMode, UiLayout}
import com.serenity.rope.Balance
import com.serenity.state.models.Damage
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.FrameSurfaceFactory
import com.serenity.ui.skia.SkiaRasterSurfaceFactory

/** Where the time of a Skia raster typing frame goes, outside the present machinery the phase timers already split.
  *
  * Three views of the same typing frame: its time on the Skia surface for each chrome variant (the full-width chrome
  * band repaints on a typing frame, so removing a piece of chrome removes paint); its time on a surface that paints
  * nothing, which is what the renderer costs on its own; and the draw calls it makes. With `-Dprofile.seconds=N` it
  * instead types for N seconds and exits, which is what a sampling profiler wants to attach to.
  *
  * Settings are system properties: `profile.width`, `profile.height` (logical), `profile.scale`, and for the timed run
  * `profile.variant=skia|floor` and `profile.chrome=<variant label>`.
  */
private[perf] object TypingPaintProfile:

  final case class Variant(label: String, config: AppConfig)

  val variants: List[Variant] =
    val conventional = AppConfig.conventional
    List(
      Variant("full", conventional),
      Variant("no_spellcheck", conventional.withSpellCheck(SpellCheckConfig(enabled = false))),
      Variant("no_scrollbar", conventional.withScrollbar(false)),
      Variant("no_line_numbers", conventional.withLineNumbers(false)),
      Variant("no_pane_headers", conventional.withPaneHeaders(false)),
      Variant("no_status_line", conventional.withStatusLinePlacement(StatusLinePlacement.Off)),
      Variant("no_tab_strip", conventional.withTabStrip(TabStripMode.Off)),
      Variant("minimal", LayoutPreset.base(UiLayout.Minimal))
    )

  /** Typing frames in a window of `widthPx` x `heightPx` logical pixels, drawn with `config`'s chrome. */
  def typingCycle(factory: FrameSurfaceFactory, widthPx: Int, heightPx: Int, scale: Double, config: AppConfig)(using
    IORuntime
  ): PresentBenchmarks.Cycling =
    given Balance       = Balance.default
    val metrics         = CellMetrics.fromFont(LaptopFrameBenchmarks.codeFont)
    val grid            = ViewportSize(widthPx / metrics.charWidth, heightPx / metrics.lineHeight)
    val (prose, _)      = LaptopFrameBenchmarks.proseStateManager(config = config, grid = grid)
    val (before, after) = TypingTransition.of(prose)
    PresentBenchmarks.Cycling(PresentBenchmarks.Window(factory, widthPx, heightPx, scale, grid), Vector(before, after))

  private val WarmFrames    = 10
  private val CountedFrames = 30

  private def drawCountsPerFrame(variant: Variant, widthPx: Int, heightPx: Int, scale: Double)(using
    IORuntime
  ): Map[DrawKind, Double] =
    val counts = new DrawCounts
    val cycle = typingCycle(CountingFactory(SkiaRasterSurfaceFactory, counts), widthPx, heightPx, scale, variant.config)
    (0 until WarmFrames).foreach(_ => cycle.next())
    counts.reset()
    (0 until CountedFrames).foreach(_ => cycle.next())
    counts.snapshot.map((kind, total) => kind -> total.toDouble / CountedFrames)

  private def timedRow(
    name: String,
    factory: FrameSurfaceFactory,
    variant: Variant,
    widthPx: Int,
    heightPx: Int,
    scale: Double
  )(using IORuntime): (BenchmarkRunner.Benchmark, PresentBenchmarks.PhaseLog) =
    val cycle = typingCycle(factory, widthPx, heightPx, scale, variant.config)
    val log   = PresentBenchmarks.PhaseLog(cycle.drainPhases)
    val row = BenchmarkRunner.Benchmark(
      name,
      3,
      20,
      () => assert(cycle.forwardDamage != Damage.Everything, "typing damaged the whole frame"),
      log.timing(() => cycle.next()),
      settleJit = true
    )
    row -> log

  private def report(widthPx: Int, heightPx: Int, scale: Double)(using IORuntime): Unit =
    val label = s"${heightPx}p_${scale.toInt}x"
    val skia = variants.map { variant =>
      timedRow(
        s"render.profile.$label.typing_skia.${variant.label}",
        SkiaRasterSurfaceFactory,
        variant,
        widthPx,
        heightPx,
        scale
      )
    }
    val floor = timedRow(
      s"render.profile.$label.typing_floor.full",
      NullSurfaceFactory(),
      variants.head,
      widthPx,
      heightPx,
      scale
    )
    val java2d = variants.take(2).map { variant =>
      timedRow(
        s"render.profile.$label.typing_java2d.${variant.label}",
        FrameSurfaceFactory.Java2D,
        variant,
        widthPx,
        heightPx,
        scale
      )
    }
    val rows    = skia ++ java2d :+ floor
    val results = BenchmarkRunner.runMatching(Nil, rows.map(_._1))
    val phases  = rows.map((benchmark, log) => benchmark.name -> log).toMap
    PresentReport.print(results, name => phases.get(name).map(_.breakdown))
    def timingOf(result: BenchmarkRunner.BenchmarkResult) =
      TypingPaintReport.VariantTiming(result.name.split('.').last, result.p50Ms, result.p95Ms)
    val (skiaResults, rest)           = results.splitAt(skia.length)
    val (java2dResults, floorResults) = rest.splitAt(java2d.length)
    val timings                       = skiaResults.map(timingOf)
    println()
    println(TypingPaintReport.timingTable(timings))
    println()
    println("Java2D, same frames:")
    println(TypingPaintReport.timingTable(java2dResults.map(timingOf)))
    floorResults.headOption.foreach(floorResult =>
      println(TypingPaintReport.floorLine(skiaP50Ms = timings.head.p50Ms, floorP50Ms = floorResult.p50Ms))
    )
    variants.foreach(variant =>
      println(TypingPaintReport.drawCountsLine(variant.label, drawCountsPerFrame(variant, widthPx, heightPx, scale)))
    )

  private def typeFor(
    seconds: Double,
    factory: FrameSurfaceFactory,
    variant: Variant,
    widthPx: Int,
    heightPx: Int,
    scale: Double
  )(using
    IORuntime
  ): Unit =
    val cycle    = typingCycle(factory, widthPx, heightPx, scale, variant.config)
    val deadline = System.nanoTime() + (seconds * 1e9).toLong

    @annotation.tailrec
    def loop(frames: Long): Long =
      if System.nanoTime() >= deadline then frames
      else
        val _ = cycle.next()
        loop(frames + 1)

    println(s"typed ${loop(0L)} frames of ${variant.label}")

  def main(args: Array[String]): Unit =
    import cats.effect.unsafe.implicits.global
    val widthPx  = sys.props.getOrElse("profile.width", "1920").toInt
    val heightPx = sys.props.getOrElse("profile.height", "1080").toInt
    val scale    = sys.props.getOrElse("profile.scale", "1").toDouble
    sys.props.get("profile.seconds").map(_.toDouble) match
      case Some(seconds) =>
        val chrome = sys.props.getOrElse("profile.chrome", "full")
        val variant = variants
          .find(_.label == chrome)
          .getOrElse(
            throw IllegalArgumentException(
              s"profile.chrome must be one of ${variants.map(_.label).mkString(", ")}, not $chrome"
            )
          )
        val factory = sys.props.getOrElse("profile.variant", "skia") match
          case "skia"  => SkiaRasterSurfaceFactory
          case "floor" => NullSurfaceFactory()
          case other   => throw IllegalArgumentException(s"profile.variant must be skia or floor, not $other")
        typeFor(seconds, factory, variant, widthPx, heightPx, scale)
      case None => report(widthPx, heightPx, scale)
    sys.exit(0)
