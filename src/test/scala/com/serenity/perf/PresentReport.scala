package com.serenity.perf

/** The present benchmark's output: the plain benchmark CSV with the time of each present phase appended, and a line per
  * row saying where its frame went. The phases come last so a reader of the plain layout still parses every row.
  */
private[perf] object PresentReport:

  /** The nanoseconds one frame spent in each phase, and in all of it. */
  final case class FramePhases(
      planNanos: Long,
      surfaceMoveNanos: Long,
      imageShiftNanos: Long,
      copyOutNanos: Long,
      frameNanos: Long
  )

  /** What a row's frame costs, in milliseconds. The rest is paint and the renderer's own work, and the frame time less
    * the phases, so it is a remainder and never negative.
    */
  final case class PhaseBreakdown(
      planMs: Double,
      surfaceMoveMs: Double,
      imageShiftMs: Double,
      copyOutMs: Double,
      frameMs: Double
  ):
    def restMs: Double = (frameMs - planMs - surfaceMoveMs - imageShiftMs - copyOutMs).max(0.0)

  object PhaseBreakdown:

    /** The median of each column on its own, as the plain rows' medians are. */
    def median(frames: Vector[FramePhases]): PhaseBreakdown =
      def ms(nanos: FramePhases => Long): Double =
        if frames.isEmpty then 0.0 else frames.map(nanos).sorted.apply(frames.length / 2) / 1_000_000.0
      PhaseBreakdown(
        ms(_.planNanos),
        ms(_.surfaceMoveNanos),
        ms(_.imageShiftNanos),
        ms(_.copyOutNanos),
        ms(_.frameNanos)
      )

  /** The lookups one frame made of the shaped-text cache that were not answered by its newer generation. */
  final case class ShapeFigures(shapings: Long, promotions: Long)

  /** A row's shape traffic: for each figure the total over `frames` and the largest frame, so the mean is the total
    * over the frames and the largest says whether it comes from a few frames or from all of them.
    */
  final case class ShapeTrafficStats(frames: Int, shapings: (Long, Long), promotions: (Long, Long)):
    def meanShapings: Double   = if frames == 0 then 0.0 else shapings._1.toDouble / frames
    def meanPromotions: Double = if frames == 0 then 0.0 else promotions._1.toDouble / frames

  object ShapeTrafficStats:

    def of(frames: Vector[ShapeFigures]): ShapeTrafficStats =
      ShapeTrafficStats(
        frames.length,
        (frames.map(_.shapings).sum, frames.map(_.shapings).maxOption.getOrElse(0L)),
        (frames.map(_.promotions).sum, frames.map(_.promotions).maxOption.getOrElse(0L))
      )

  val csvHeader: String =
    BenchmarkRunner.csvHeader + ",plan_ms,surface_move_ms,image_shift_ms,copy_out_ms,rest_ms"

  def csvRow(result: BenchmarkRunner.BenchmarkResult, breakdown: Option[PhaseBreakdown]): String =
    BenchmarkRunner.csvRow(result) + breakdown.fold(",,,,,")(b =>
      f",${b.planMs}%.7f,${b.surfaceMoveMs}%.7f,${b.imageShiftMs}%.7f,${b.copyOutMs}%.7f,${b.restMs}%.7f"
    )

  def summaryLine(name: String, b: PhaseBreakdown): String =
    f"phases $name: plan ${b.planMs}%.2f ms, surface move ${b.surfaceMoveMs}%.2f ms, " +
      f"image shift ${b.imageShiftMs}%.2f ms, copy-out ${b.copyOutMs}%.2f ms, rest ${b.restMs}%.2f ms, " +
      f"frame ${b.frameMs}%.2f ms"

  def trafficLine(name: String, stats: ShapeTrafficStats): String =
    f"shapes $name: shapings avg=${stats.meanShapings}%.1f max=${stats.shapings._2}, " +
      f"promotions avg=${stats.meanPromotions}%.1f max=${stats.promotions._2} over ${stats.frames} frames"

  def print(
    results: List[BenchmarkRunner.BenchmarkResult],
    breakdownOf: String => Option[PhaseBreakdown],
    trafficOf: String => Option[ShapeTrafficStats]
  ): Unit =
    BenchmarkRunner.printContext()
    println(csvHeader)
    results.foreach(result => println(csvRow(result, breakdownOf(result.name))))
    results.foreach(result => breakdownOf(result.name).foreach(b => println(summaryLine(result.name, b))))
    results.foreach(result => trafficOf(result.name).foreach(t => println(trafficLine(result.name, t))))
