package com.serenity.perf

object EmojiClusterRunProbe:
  def main(args: Array[String]): Unit =
    val settle = args.headOption.contains("settle")
    val b      = CellGridBenchmarks.benchmarks().find(_.name == "render.cell_grid.frame_emoji_clusters").getOrElse(sys.error("none"))
    val r      = BenchmarkRunner.runBenchmark(if settle then b.copy(settleJit = true) else b)
    println(f"RUN settle=$settle batch=${r.batch} warmupInvocations=${r.warmupInvocations} samples=${r.iterations} p50=${r.p50Ms}%.3f p95=${r.p95Ms}%.3f")
