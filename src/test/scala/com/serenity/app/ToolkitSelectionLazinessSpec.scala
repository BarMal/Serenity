package com.serenity.app

import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.util.concurrent.TimeUnit

import scala.jdk.CollectionConverters.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Reading the JVM's own arguments to tell a user's `-Dawt.toolkit.name` from a runtime default must not load AWT: the
  * toolkit is fixed by the first `java.awt` class, so the choice has to be made before any is touched. The child JVM
  * runs with `-verbose:class`, which lists every class the choice touched.
  */
class ToolkitSelectionLazinessSpec extends AnyFlatSpec with Matchers:

  private val Forbidden = List("java.awt.", "sun.awt.", "javax.swing.")

  final private case class ChildRun(exitCode: Int, output: List[String])

  private def runChild(jvmProperties: List[String]): ChildRun =
    val javaBin = Paths.get(System.getProperty("java.home"), "bin", "java").toString
    val builder = new ProcessBuilder(
      (List(javaBin) ++ jvmProperties ++ List(
        "-Xshare:auto",
        "-verbose:class",
        "-cp",
        testClasspath,
        "com.serenity.app.ToolkitSelectionChild"
      )).asJava
    ).redirectErrorStream(true)
    val process = builder.start()
    val output  = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8).linesIterator.toList
    val exited  = process.waitFor(120, TimeUnit.SECONDS)
    if !exited then process.destroyForcibly()
    ChildRun(if exited then process.exitValue() else -1, output)

  private def loadedAwt(run: ChildRun): List[String] =
    run.output.filter(line => Forbidden.exists(prefix => line.contains(s" $prefix") || line.contains(s"[$prefix")))

  "Choosing the toolkit" should "not load any AWT or Swing class when no flag is passed" in {
    val run = runChild(Nil)
    run.exitCode shouldBe 0
    run.output.exists(_.startsWith("chose ")) shouldBe true
    loadedAwt(run) shouldBe Nil
  }

  it should "not load any AWT or Swing class when -Dawt.toolkit.name is passed, and treat it as the user's choice" in {
    val run = runChild(List("-Dawt.toolkit.name=XToolkit"))
    run.exitCode shouldBe 0
    run.output.exists(line =>
      line.startsWith("chose PlatformDefault") && line.contains("awt.toolkit.name")
    ) shouldBe true
    loadedAwt(run) shouldBe Nil
  }

  private lazy val testClasspath: String =
    val stream = Option(getClass.getResourceAsStream("/crash/test-classpath.txt"))
      .getOrElse(fail("build.sbt did not generate crash/test-classpath.txt"))
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8).trim
    finally stream.close()
