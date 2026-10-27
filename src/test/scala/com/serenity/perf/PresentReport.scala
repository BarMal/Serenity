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

  def print(results: List[BenchmarkRunner.BenchmarkResult], breakdownOf: String => Option[PhaseBreakdown]): Unit =
    BenchmarkRunner.printContext()
    println(csvHeader)
    results.foreach(result => println(csvRow(result, breakdownOf(result.name))))
    results.foreach(result => breakdownOf(result.name).foreach(b => println(summaryLine(result.name, b))))
