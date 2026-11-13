package com.serenity.app

import java.io.{BufferedReader, InputStreamReader}
import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.*

import cats.effect.{IO, Ref, Resource}
import com.serenity.app.RendererChoice.Candidate
import com.serenity.app.RendererCoordinator.Spawner

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

  /** A spawner over `commandFor`: whatever the child prints on either stream, once it exits or `timeout` passes, when
    * it is stopped. A child that cannot be started fails the call.
    */
  def apply(commandFor: Candidate => List[String], timeout: FiniteDuration): Spawner =
    candidate =>
      Resource
        .make(IO.blocking(new ProcessBuilder(commandFor(candidate).asJava).redirectErrorStream(true).start()))(
          process =>
            IO.blocking(if process.isAlive then
              process.destroyForcibly(); ())
        )
        .use { process =>
          for
            printed <- Ref.of[IO, Vector[String]](Vector.empty)
            reader = new BufferedReader(new InputStreamReader(process.getInputStream, StandardCharsets.UTF_8))
            pumping <- pump(reader, printed).start
            _ <- IO
              .fromCompletableFuture(IO(process.onExit()))
              .void
              .timeoutTo(timeout, IO.blocking { process.destroyForcibly(); () })
            _    <- pumping.join
            text <- printed.get
          yield text.mkString("\n")
        }

  private def pump(reader: BufferedReader, printed: Ref[IO, Vector[String]]): IO[Unit] =
    IO.blocking(Option(reader.readLine())).flatMap {
      case Some(line) => printed.update(_ :+ line) >> pump(reader, printed)
      case None       => IO.unit
    }
