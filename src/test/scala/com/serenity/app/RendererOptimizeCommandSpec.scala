package com.serenity.app

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.Path
import java.time.Instant

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{ExitCode, IO}
import com.serenity.TestTemp
import com.serenity.app.RendererBenchmarkResult.Key
import com.serenity.app.RendererChoice.{Candidate, Outcome, Workload}
import com.serenity.app.RendererCoordinator.{Environment, Spawner}
import com.serenity.config.{AppConfig, ConfigManager, RenderBackendSetting, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererOptimizeCommandSpec extends AnyFlatSpec with Matchers:

  private val machine = Key("unknown", "Linux 6.18", "x11", "unknown", "25.0.5+8", "0.153.0", "ac")

  private def options(args: String*): LaunchOptions =
    LaunchOptions.parse("--optimize-renderer" :: args.toList).fold(help => fail(help.toString), identity)

  private def backendOf(candidate: Candidate): RenderBackend =
    if candidate == Candidate.SkiaRaster then RenderBackend.SkiaRaster else RenderBackend.Skia

  private def printed(candidate: Candidate, typing: Double): String =
    RendererTrial.line(
      RendererTrial.Report(
        candidate,
        backendOf(candidate),
        Some("OPENGL"),
        Outcome.Measured(Workload(typing, typing * 1.5, 2.0, 1.0, Map("page-down" -> 40.0, "full-redraw" -> 60.0)))
      )
    )

  /** Layer+image at 30 ms and the raster surface at 50: the layer wins clearly. */
  private val layerWins: Spawner = candidate =>
    IO.pure(printed(candidate, if candidate == Candidate.SkiaRaster then 50.0 else 30.0))

  private val nothingWorks: Spawner = _ => IO.pure("")

  private def run(
    opts: LaunchOptions,
    spawner: Spawner,
    config: Path = TestTemp.directory("optimize-command").resolve("config.conf"),
    display: Boolean = true
  ): (ExitCode, String, Path) =
    val bytes      = new ByteArrayOutputStream()
    val out        = new PrintStream(bytes, true, "UTF-8")
    val resultPath = config.resolveSibling("render-benchmark.json")
    val deps = RendererOptimizeCommand.Deps(
      spawner = spawner,
      environment = Environment(IO.pure(machine), IO.pure(Instant.parse("2026-11-13T19:00:00Z")), resultPath),
      configPath = config,
      hasDisplay = display,
      countdown = Duration.Zero,
      out = out
    )
    val code = RendererOptimizeCommand.run(opts, deps).unsafeRunSync()
    (code, bytes.toString("UTF-8"), config)

  /** A config that has the raster surface on: the layer winning is then a switch, not a mode already in use. */
  private def rasterConfig(): Path =
    val config = TestTemp.directory("optimize-command-raster").resolve("config.conf")
    val base   = AppConfig.default
    ConfigManager
      .saveConfigIO(
        base.withSurfaceConfig(base.surfaceConfig.copy(renderBackend = RenderBackendSetting.SkiaRaster)),
        config
      )
      .unsafeRunSync() shouldBe Right(())
    config

  "RendererOptimizeCommand.run" should "say how long it takes and that it types into windows, before it starts" in {
    val (_, text, _) = run(options(), layerWins)
    text should (include("8 minutes") and include("keyboard and mouse") and include("Ctrl+C"))
    run(options("--thorough"), layerWins)._2 should include("12 minutes")
  }

  it should "refuse, and start no trial, when there is no display to open windows on" in {
    var started           = 0
    val counting: Spawner = candidate => IO(started += 1) *> layerWins.run(candidate)
    val (code, text, _)   = run(options(), counting, display = false)
    code shouldBe ExitCode.Error
    started shouldBe 0
    text should include("display")
  }

  it should "recommend a clearly faster mode and say what to set, without touching the config" in {
    val config          = rasterConfig()
    val before          = java.nio.file.Files.readString(config)
    val (code, text, _) = run(options(), layerWins, config = config)
    code shouldBe ExitCode.Success
    text should (include("ui.render.backend = skia, ui.render.skia_frames = image") and include("--apply"))
    java.nio.file.Files.readString(config) shouldBe before
  }

  it should "write the recommended mode to the config when asked to apply it" in {
    val (code, text, config) = run(options("--apply"), layerWins, config = rasterConfig())
    code shouldBe ExitCode.Success
    text should include("written")
    val surface = ConfigManager.loadConfigIO(Some(config.toString)).unsafeRunSync().surfaceConfig
    (surface.renderBackend, surface.renderSkiaFrames) shouldBe (RenderBackendSetting.Skia, SkiaFrames.Image)
  }

  it should "change nothing when there is no clear winner, even when asked to apply" in {
    val close: Spawner =
      candidate => IO.pure(printed(candidate, if candidate == Candidate.SkiaRaster then 50.0 else 48.0))
    val (code, text, config) = run(options("--apply"), close)
    code shouldBe ExitCode.Success
    text should include("Nothing to change")
    java.nio.file.Files.exists(config) shouldBe false
  }

  it should "judge the current mode from the config, so a mode already in use is kept" in {
    val config = TestTemp.directory("optimize-command-current").resolve("config.conf")
    val base   = AppConfig.default
    ConfigManager
      .saveConfigIO(
        base.withSurfaceConfig(base.surfaceConfig.copy(renderBackend = RenderBackendSetting.SkiaRaster)),
        config
      )
      .unsafeRunSync() shouldBe Right(())
    val rasterWins: Spawner =
      candidate => IO.pure(printed(candidate, if candidate == Candidate.SkiaRaster then 20.0 else 50.0))
    val (_, text, _) = run(options(), rasterWins, config = config)
    text should (include("Nothing to change") and include("skia-raster"))
  }

  it should "exit with an error when every trial failed, and say so" in {
    val (code, text, _) = run(options(), nothingWorks)
    code shouldBe ExitCode.Error
    text should include("failed")
  }

  it should "report each trial as it starts" in {
    val (_, text, _) = run(options(), layerWins)
    text should (include("Trial 1 of 4") and include("Trial 4 of 4"))
  }
