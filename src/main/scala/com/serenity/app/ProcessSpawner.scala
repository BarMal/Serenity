package com.serenity.app

import java.lang.management.ManagementFactory
import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

import cats.effect.IO
import com.serenity.app.RendererChoice.Candidate
import com.serenity.app.RendererCoordinator.Spawner
import com.serenity.process.{ProcessOwner, ProcessService, ProcessSpec}

/** Starts one `--renderer-trial=<mode>` child as a process of its own (#2618), so each trial gets a fresh JVM and a
  * window of its own, and a native failure in one cannot take the coordinator with it.
  */
object ProcessSpawner:

  /** What the coordinator's own JVM was started with, so a child runs the same build the same way. */
  final case class Runtime(java: String, jvmArguments: List[String], classPath: String, mainClass: String)

  def currentRuntime: Runtime =
    Runtime(
      java = ProcessHandle
        .current()
        .info()
        .command()
        .orElse(Path.of(System.getProperty("java.home"), "bin", "java").toString),
      jvmArguments = ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.toList,
      classPath = System.getProperty("java.class.path"),
      mainClass = "Main"
    )

  /** The command for one trial: the same JVM flags and class path, safe mode (the trial types into the window), the
    * mode's renderer and, for the layer, its frame form named outright so a changed default cannot change what is
    * measured, on the document the trial scrolls.
    */
  def command(runtime: Runtime, candidate: Candidate, document: Path): List[String] =
    val (renderer, frames) = candidate match
      case Candidate.LayerImage   => ("skia", Some("image"))
      case Candidate.LayerPicture => ("skia", Some("picture"))
      case Candidate.SkiaRaster   => ("skia-raster", None)
    List(runtime.java) ++ runtime.jvmArguments ++ frames.map(form => s"-Dserenity.skia.layerFrames=$form") ++
      List("-cp", runtime.classPath, runtime.mainClass, "--safe-mode", s"--renderer=$renderer") ++
      List(s"--renderer-trial=${candidate.label}", "--open", document.toString)

  /** A spawner over the process service that owns every process the editor starts: whatever the child printed on its
    * standard output once it exited. A child that could not be started, or is still running at `timeout` (and so is
    * stopped), fails the call.
    */
  def apply(service: ProcessService, commandFor: Candidate => List[String], timeout: FiniteDuration): Spawner =
    candidate =>
      service
        .run(ProcessSpec(commandFor(candidate), ProcessOwner("renderer-trial")), timeout)
        .flatMap {
          case Some(result) => IO.pure(result.output)
          case None =>
            IO.raiseError(
              new IllegalStateException(
                s"the child did not finish within ${timeout.toSeconds} s, or could not be started"
              )
            )
        }
