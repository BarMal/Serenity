package com.serenity.ui.benchmark

import java.awt.GraphicsEnvironment
import java.io.PrintStream
import java.nio.file.{Files, Path}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import cats.effect.{ExitCode, IO}
import com.serenity.app.RenderBackendSelection.Probe
import com.serenity.ui.skia.SkiaProbe

/** `--render-benchmark`: runs the side-by-side timing, prints the report and keeps it as Markdown and JSON, without
  * starting the editor.
  */
object RenderBenchmark:

  private val Stamp = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss")

  /** `startup` is the launch's toolkit and renderer choice, which the report records. */
  def run(
    options: RenderBenchmarkOptions,
    outputDirectory: Path,
    out: PrintStream,
    startup: String = ""
  ): IO[ExitCode] =
    if GraphicsEnvironment.isHeadless then
      IO(out.println("The render benchmark opens real windows and needs a display; none is available."))
        .as(ExitCode.Error)
    else
      for
        probe  <- SkiaProbe.run
        report <- IO.blocking(RenderBenchmarkRunner.run(options, skiaProbe = skiaProbe(probe), startup = startup))
        _      <- IO.blocking(keep(report, outputDirectory, out))
      yield ExitCode.Success

  private def skiaProbe(probe: Probe): Either[String, Unit] =
    probe match
      case Probe.Loaded        => Right(())
      case Probe.Failed(cause) => Left(cause)
      case Probe.NotRun        => Left("the native library was not loaded")

  private def keep(report: BenchmarkReport, directory: Path, out: PrintStream): Unit =
    val stem = s"render-benchmark-${LocalDateTime.now().format(Stamp)}"
    val _    = Files.createDirectories(directory)
    val md   = Files.writeString(directory.resolve(s"$stem.md"), report.markdown)
    val json = Files.writeString(directory.resolve(s"$stem.json"), report.json)
    out.println(report.markdown)
    out.println(s"Saved: $md and $json")
