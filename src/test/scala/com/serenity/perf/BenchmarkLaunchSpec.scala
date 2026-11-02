package com.serenity.perf

import java.awt.GraphicsEnvironment
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}
import java.util.concurrent.TimeUnit

import scala.jdk.CollectionConverters.*

import com.serenity.TestTemp
import com.serenity.testkit.DisplayExclusive
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A benchmark is launched from `serenity-perf.jar` on a laptop with no source tree, and a launch that fails must end
  * the process with a non-zero code rather than leave it running. Each case runs in a fresh JVM.
  */
class BenchmarkLaunchSpec extends AnyFlatSpec with Matchers:

  final private case class ChildRun(exitCode: Int, output: String)

  private def runChild(
    main: String,
    workingDirectory: Path,
    headless: Boolean,
    arguments: List[String] = Nil
  ): ChildRun =
    val javaBin = Paths.get(System.getProperty("java.home"), "bin", "java").toString
    val builder = new ProcessBuilder(
      (List(javaBin) ++ Option.when(headless)("-Djava.awt.headless=true") ++
        List("-cp", testClasspath, main) ++ arguments).asJava
    ).directory(workingDirectory.toFile).redirectErrorStream(true)
    val process = builder.start()
    val output  = new StringBuilder
    val reader = new Thread(() =>
      val text = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      val _    = output.synchronized(output.append(text))
    )
    reader.setDaemon(true)
    reader.start()
    val exited = process.waitFor(120, TimeUnit.SECONDS)
    if !exited then process.destroyForcibly()
    reader.join(5000)
    ChildRun(if exited then process.exitValue() else -1, output.synchronized(output.toString))

  "The cell grid benchmarks" should "load their font when the working directory is not the source tree" in {
    val empty = TestTemp.directory("serenity-benchmark-launch")
    val run   = runChild("com.serenity.perf.CellGridFontChild", empty, headless = true)

    withClue(run.output)(run.exitCode shouldBe 0)
    run.output should include("loaded")
  }

  "A benchmark launch that finishes" should "end the process normally, as it did without the wrapper" in {
    val run =
      runChild("com.serenity.perf.BenchmarkMainChild", Paths.get("").toAbsolutePath, headless = true, List("success"))

    withClue(run.output)(run.exitCode shouldBe 0)
    run.output should include("finished")
  }

  "A benchmark launch that throws" should "print the failure and end the process with a non-zero exit code" in {
    val run =
      runChild("com.serenity.perf.BenchmarkMainChild", Paths.get("").toAbsolutePath, headless = true, List("failure"))

    withClue(run.output)(run.exitCode shouldBe 1)
    run.output should include("IllegalStateException: benchmark failed")
  }

  "A benchmark launch that fails with a window open" should "end the process with a non-zero exit code" in {
    assume(!GraphicsEnvironment.isHeadless, "Opening the benchmark windows needs a display")

    DisplayExclusive {
      val run = runChild("com.serenity.perf.WindowFailureChild", Paths.get("").toAbsolutePath, headless = false)

      withClue(run.output) {
        run.exitCode should not be -1
        run.exitCode should not be 0
      }
    }
  }

  private lazy val testClasspath: String =
    val stream = Option(getClass.getResourceAsStream("/crash/test-classpath.txt"))
      .getOrElse(fail("build.sbt did not generate crash/test-classpath.txt"))
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8).trim
    finally stream.close()
