package com.serenity.perf

import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.util.concurrent.TimeUnit

import scala.jdk.CollectionConverters.*
import scala.util.Using

import com.serenity.perf.RendererBenchmarkReport.*

/** Compares Java2D, Skia on the GPU and Skia on the CPU in a real window on the desktop it is started on.
  *
  * {{{
  * sbt "Test/runMain com.serenity.perf.RendererWindowBenchmark --renderers=java2d,skia,skia-raster --rounds=3"
  * }}}
  *
  * Each renderer runs in a child JVM ([[RendererWindowBenchmarkChild]]) started with the flags that select it in the
  * editor, since the backend is fixed at startup. The rounds are interleaved (java2d, skia, skia-raster, java2d, skia,
  * ...) so that drift in the machine's load, thermals or compositor lands on every renderer alike, and the frames of
  * all rounds are pooled into one table of p50, p95 and p99 frame time and allocation per frame.
  *
  * Options: `--renderers=` (default all three), `--rounds=` (default 3), `--frames=` and `--warmup=` per scenario and
  * round, `--logical-size=WxH` for the window (default 1200x800), `--jvm-option=` (repeatable, passed to every child),
  * `--vsync` to leave the display's vertical sync on, and `--simulate-wayland-raster` to measure `skia-raster` on a
  * desktop that is not a native Wayland session, where no flag can select it.
  *
  * The scale is the desktop's own; nothing here sets it. System properties starting `sun.java2d.`, `skiko.` or `awt.`
  * that this JVM was started with are passed on to the children.
  */
object RendererWindowBenchmark:

  final private case class Options(
      renderers: List[Renderer],
      rounds: Int,
      childArguments: List[String],
      jvmOptions: List[String],
      vsync: Boolean,
      simulateRaster: Boolean
  )

  final private case class Tally(reports: Map[Renderer, List[ChildReport]], unavailable: Map[Renderer, String])

  private val ChildTimeoutMinutes = 15L
  private val ForwardedProperties = List("sun.java2d.", "skiko.", "awt.")

  private def optionsOf(args: List[String]): Either[String, Options] =
    def valuesOf(prefix: String): List[String] = args.collect {
      case arg if arg.startsWith(prefix) => arg.drop(prefix.length)
    }
    val known = List("--renderers=", "--rounds=", "--frames=", "--warmup=", "--logical-size=", "--jvm-option=")
    val unknown =
      args.filterNot(arg => known.exists(arg.startsWith) || arg == "--vsync" || arg == "--simulate-wayland-raster")
    for
      _         <- Either.cond(unknown.isEmpty, (), s"unknown option ${unknown.mkString(", ")}")
      renderers <- Renderer.parseAll(valuesOf("--renderers=").headOption.getOrElse("java2d,skia,skia-raster"))
      rounds <- valuesOf("--rounds=").headOption
        .fold(Right(3))(value => value.toIntOption.filter(_ > 0).toRight(s"--rounds=$value is not a count"))
    yield Options(
      renderers,
      rounds,
      args.filter(arg => List("--frames=", "--warmup=", "--logical-size=").exists(arg.startsWith)),
      valuesOf("--jvm-option="),
      args.contains("--vsync"),
      args.contains("--simulate-wayland-raster")
    )

  def main(args: Array[String]): Unit =
    optionsOf(args.toList) match
      case Left(problem) =>
        System.err.println(problem)
        System.err.println(
          "usage: --renderers=java2d,skia,skia-raster --rounds=3 [--frames=N --warmup=N --logical-size=WxH]"
        )
        System.err.println("       [--jvm-option=-Dsun.java2d.uiScale=2 ...] [--vsync] [--simulate-wayland-raster]")
      case Right(options) => report(options, measure(options))

  private def measure(options: Options): Tally =
    (1 to options.rounds).foldLeft(Tally(Map.empty, Map.empty)) { (afterEarlier, round) =>
      options.renderers.foldLeft(afterEarlier) { (tally, renderer) =>
        if tally.unavailable.contains(renderer) then tally
        else
          System.err.println(s"[round $round of ${options.rounds}] ${renderer.name}")
          runChild(options, renderer).flatMap(checked(renderer, _)) match
            case Right(report) =>
              tally.copy(reports = tally.reports.updated(renderer, tally.reports.getOrElse(renderer, Nil) :+ report))
            case Left(reason) => tally.copy(unavailable = tally.unavailable.updated(renderer, reason))
      }
    }

  private def runChild(options: Options, renderer: Renderer): Either[String, ChildReport] =
    val command = childCommand(options, renderer)
    val process = new ProcessBuilder(command.asJava).redirectError(Redirect.INHERIT).start()
    val output  = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8).linesIterator.toList
    if !process.waitFor(ChildTimeoutMinutes, TimeUnit.MINUTES) then
      process.destroyForcibly()
      Left(s"timed out after $ChildTimeoutMinutes minutes")
    else if process.exitValue() != 0 then Left(s"the child exited with code ${process.exitValue()}")
    else Right(parseChild(output))

  private def childCommand(options: Options, renderer: Renderer): List[String] =
    val java = Paths.get(System.getProperty("java.home"), "bin", "java").toString
    val forwarded = System.getProperties.asScala.toList.collect {
      case (name, value) if ForwardedProperties.exists(name.startsWith) => s"-D$name=$value"
    }
    val vsync = Option.when(!options.vsync && !forwarded.exists(_.startsWith("-Dskiko.vsync.enabled=")))(
      "-Dskiko.vsync.enabled=false"
    )
    val simulate = Option.when(options.simulateRaster && renderer == Renderer.SkiaRaster)("--simulate-wayland-raster")
    List(java) ++ forwarded ++ vsync ++ options.jvmOptions ++
      List("-cp", childClasspath, "com.serenity.perf.RendererWindowBenchmarkChild") ++
      options.childArguments ++ simulate ++ renderer.flags

  /** The classpath the build generated for the specs: sbt runs this in its own JVM, whose `java.class.path` is not the
    * project's.
    */
  private def childClasspath: String =
    Option(getClass.getResourceAsStream("/crash/test-classpath.txt"))
      .map(stream => Using.resource(stream)(in => new String(in.readAllBytes(), StandardCharsets.UTF_8).trim))
      .getOrElse(System.getProperty("java.class.path", ""))

  private def report(options: Options, tally: Tally): Unit =
    val measured = options.renderers.flatMap(renderer => tally.reports.get(renderer).map(renderer -> _))
    val first    = measured.flatMap(_._2).headOption
    def context(key: String): String = first.flatMap(_.environment.get(key)).getOrElse("unknown")
    println("Serenity renderer comparison in a real window")
    println(s"os: ${context("os")}; java: ${context("java")}; toolkit: ${context("toolkit")}")
    println(
      s"device scale: ${context("scale")}; canvas: ${context("canvas")}; allocation counted over: ${context("allocation")}"
    )
    println(s"jvm options: ${context("jvm")}")
    println(
      s"rounds: ${options.rounds} interleaved (${options.renderers.map(_.name).mkString(", ")}); " +
        s"frames pooled across rounds; vsync ${if options.vsync then "left on" else "off"}"
    )
    val colours = measured
      .map((renderer, reports) =>
        s"${renderer.name}=${reports.lastOption.flatMap(_.environment.get("colours")).getOrElse("?")}"
      )
      .mkString(", ")
    println(s"on-screen colours sampled after the run (a blank window shows 1): $colours")
    println()
    table(
      measured.flatMap((renderer, reports) => rows(renderer, reports)),
      options.renderers.flatMap(renderer => tally.unavailable.get(renderer).map(Unavailable(renderer.name, _)))
    ).foreach(println)
