package com.serenity.app

import java.time.Instant

import scala.concurrent.duration.*

import cats.effect.{ExitCode, IO}
import com.serenity.app.RendererCoordinator.Environment
import com.serenity.config.ConfigManager

/** `--optimize-renderer` as the real process runs it: children of this same JVM build, the user's own config and result
  * file, and the console.
  */
object RendererOptimizeProduction:

  private val Countdown = 5.seconds

  /** A trial's own ceiling, plus the time a window takes to start and stop. */
  private val ChildCeiling = RendererTrialChild.Timeout + 90.seconds

  def run(options: LaunchOptions): IO[ExitCode] =
    TrialDocument.resource.use { document =>
      val runtime = ProcessSpawner.currentRuntime
      RendererOptimizeCommand.run(
        options,
        RendererOptimizeCommand.Deps(
          spawner = ProcessSpawner(candidate => ProcessSpawner.command(runtime, candidate, document), ChildCeiling),
          environment = Environment(IO(MachineKey.current), IO(Instant.now()), RendererBenchmarkResult.defaultPath),
          configPath = ConfigManager.defaultConfigPath,
          hasDisplay = sys.env.get("DISPLAY").exists(_.nonEmpty) || sys.env.get("WAYLAND_DISPLAY").exists(_.nonEmpty),
          countdown = Countdown,
          out = System.out
        )
      )
    }
