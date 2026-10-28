package com.serenity.perf

/** The typing profile's printed output: one row per chrome variant, the draw calls a frame made, and how much of a
  * Skia typing frame the renderer would cost with nothing to paint.
  */
private[perf] object TypingPaintReport:

  final case class VariantTiming(label: String, p50Ms: Double, p95Ms: Double)

  private val TableHeader = "variant | p50 ms | p95 ms | p50 vs first"

  def timingTable(rows: List[VariantTiming]): String =
    val firstP50 = rows.headOption.map(_.p50Ms)
    val lines = rows.map { row =>
      val ratio = firstP50.fold(1.0)(row.p50Ms / _)
      f"${row.label} | ${row.p50Ms}%.2f | ${row.p95Ms}%.2f | $ratio%.2fx"
    }
    (TableHeader :: lines).mkString("\n")

  def drawCountsLine(label: String, perFrame: Map[DrawKind, Double]): String =
    val used = perFrame.toList
      .filter((_, count) => count > 0.0)
      .sortBy((kind, count) => (-count, kind.toString))
      .map((kind, count) => f"$kind $count%.1f")
    val total = perFrame.values.sum
    f"draws $label: total $total%.1f per frame: ${if used.isEmpty then "none" else used.mkString(", ")}"

  def floorLine(skiaP50Ms: Double, floorP50Ms: Double): String =
    val share = floorP50Ms / skiaP50Ms * 100.0
    val paint = (skiaP50Ms - floorP50Ms).max(0.0)
    f"floor: renderer-only $floorP50Ms%.2f ms of the $skiaP50Ms%.2f ms Skia typing frame ($share%.1f%%); paint and present $paint%.2f ms"
