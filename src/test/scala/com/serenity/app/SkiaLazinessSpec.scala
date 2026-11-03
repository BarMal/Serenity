package com.serenity.app

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.TimeUnit

import scala.jdk.CollectionConverters.*

import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Skiko and the Kotlin runtime it brings must stay unloaded on every launch that does not paint with Skia: the
  * terminal interface, safe mode, and a window whose setting or surroundings rule Skia out. Each case runs in a fresh
  * JVM started with `-verbose:class`, which lists every class the launch path touched.
  */
class SkiaLazinessSpec extends AnyFlatSpec with Matchers:

  private val Forbidden = List("org.jetbrains.skia", "org.jetbrains.skiko", "kotlin.")

  final private case class ChildRun(exitCode: Int, output: List[String])

  private def runChild(
    launchKind: String,
    configLines: List[String],
    environment: Map[String, String] = Map.empty,
    jvmProperties: List[String] = Nil
  ): ChildRun =
    val home = TestTemp.directory("serenity-laziness-home")
    val dir  = Files.createDirectories(home.resolve(".serenity"))
    Files.writeString(dir.resolve("config.conf"), configLines.mkString("", "\n", "\n"))
    val javaBin = Paths.get(System.getProperty("java.home"), "bin", "java").toString
    val builder = new ProcessBuilder(
      (List(javaBin, "-Djava.awt.headless=true", s"-Duser.home=$home") ++ jvmProperties ++ List(
        "-Xshare:auto",
        "-verbose:class",
        "-cp",
        testClasspath,
        "com.serenity.app.SkiaLazinessChild",
        launchKind
      )).asJava
    ).redirectErrorStream(true)
    environment.foreach((name, value) => builder.environment().put(name, value))
    val process = builder.start()
    val output  = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8).linesIterator.toList
    val exited  = process.waitFor(120, TimeUnit.SECONDS)
    if !exited then process.destroyForcibly()
    ChildRun(if exited then process.exitValue() else -1, output)

  private def loadedForbidden(run: ChildRun): List[String] =
    run.output.filter(line => Forbidden.exists(prefix => line.contains(s" $prefix") || line.contains(s"[$prefix")))

  private val cases = List(
    ("a terminal launch", "terminal", Nil),
    ("safe mode", "safe", Nil),
    ("a window with ui.render.backend = java2d", "window", List("ui.render.backend = java2d"))
  )

  private val waylandEnvironment = Map("WAYLAND_DISPLAY" -> "wayland-1", "DISPLAY" -> ":0")

  "Resolving the renderer with --wayland-render=java2d on a Wayland session" should
    "not load any Skiko or Kotlin class, even when the config asks for xwayland" in {
      assume(
        System.getProperty("os.name", "").toLowerCase.contains("linux"),
        "a Wayland session exists only on Linux; elsewhere auto loads Skia, which the test for auto covers"
      )
      val run = runChild(
        "wayland-java2d",
        List("ui.render.wayland = xwayland"),
        waylandEnvironment,
        List("-Dawt.toolkit.name=WLToolkit")
      )
      run.exitCode shouldBe 0
      run.output.exists(_.startsWith("resolved backend=java2d")) shouldBe true
      loadedForbidden(run) shouldBe Nil
    }

  cases.foreach { (description, kind, config) =>
    s"Resolving the renderer for $description" should "not load any Skiko or Kotlin class" in {
      val run = runChild(kind, config)
      run.exitCode shouldBe 0
      run.output.exists(_.startsWith("resolved backend=java2d")) shouldBe true
      loadedForbidden(run) shouldBe Nil
    }
  }

  "Resolving the renderer for a window under auto" should "load Skiko once and choose a Skia backend" in {
    val run = runChild("window", Nil)
    run.exitCode shouldBe 0
    run.output.exists(_.startsWith("resolved backend=skia")) shouldBe true
    loadedForbidden(run) should not be empty
  }

  "The class-loading check" should "recognise a loaded Skiko class line" in {
    val sample = ChildRun(0, List("[0.101s][info][class,load] org.jetbrains.skiko.Library source: jrt:/x"))
    loadedForbidden(sample) should have size 1
  }

  private lazy val testClasspath: String =
    val stream = Option(getClass.getResourceAsStream("/crash/test-classpath.txt"))
      .getOrElse(fail("build.sbt did not generate crash/test-classpath.txt"))
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8).trim
    finally stream.close()
