package com.serenity.perf

object EmojiClusterSeriesProbe:
  def main(args: Array[String]): Unit =
    val name  = args.headOption.getOrElse("render.cell_grid.frame_emoji_clusters")
    val calls = args.lift(1).map(_.toInt).getOrElse(4000)
    val out   = args.lift(2)
    val b     = CellGridBenchmarks.benchmarks().find(_.name == name).getOrElse(sys.error(s"no $name"))
    b.verify()
    val times = Array.ofDim[Long](calls)
    var i     = 0
    while i < calls do
      val s = System.nanoTime
      val _ = b.run()
      times(i) = System.nanoTime - s
      i += 1
    out.foreach(p => java.nio.file.Files.writeString(java.nio.file.Paths.get(p), times.mkString("\n")))
    println(s"SERIES done ${times.sum / 1e9}")
