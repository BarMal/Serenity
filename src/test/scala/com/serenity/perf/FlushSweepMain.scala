package com.serenity.perf

import com.serenity.app.SkiaVsync
import com.serenity.ui.skia.FlushSweep

/** `sbt "app/Test/runMain com.serenity.perf.FlushSweepMain [rows=1,5,15,30,50] [draws=60] [variants=same,changed]
  * [vsync=off|on]"` on a machine with a display: prints the table and the fit, to be pasted into the issue. Vsync is
  * off unless asked for, as the app runs.
  */
object FlushSweepMain:
  import FlushSweep.*

  private def setting(args: Array[String], name: String): Option[String] =
    args.collectFirst { case argument if argument.startsWith(s"$name=") => argument.drop(name.length + 1) }

  def main(args: Array[String]): Unit =
    BenchmarkMain.run:
      val rows  = setting(args, "rows").fold(List(1, 5, 15, 30, 50))(_.split(',').toList.flatMap(_.trim.toIntOption))
      val draws = setting(args, "draws").flatMap(_.toIntOption).getOrElse(60)
      val variants = setting(args, "variants").fold(Variant.values.toList)(
        _.split(',').toList.flatMap {
          case "same"    => Some(Variant.Same)
          case "changed" => Some(Variant.OneRowChanged)
          case _         => None
        }
      )
      vsyncRequested(setting(args, "vsync")) match
        case Left(message) =>
          Console.err.println(s"flush sweep failed: $message")
          sys.exit(1)
        case Right(enabled) =>
          val chosen = Option(System.getProperty(SkiaVsync.Property)).map(SkiaVsync.Property -> _).toMap
          vsyncProperty(enabled, sys.env, chosen).foreach((key, value) => System.setProperty(key, value))
          println(s"vsync: ${Option(System.getProperty(SkiaVsync.Property)).getOrElse("unset")}")
      run(rows, draws, variants) match
        case Right((renderApi, summaries)) => report(renderApi, summaries).foreach(println)
        case Left(message) =>
          Console.err.println(s"flush sweep failed: $message")
          sys.exit(1)
