package com.serenity.perf

import java.awt.GraphicsEnvironment
import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}
import java.util.concurrent.{CompletableFuture, TimeUnit}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

import com.serenity.TestTemp
import com.serenity.testkit.DisplayExclusive
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A benchmark is launched from `serenity-perf.jar` on a laptop with no source tree, and a launch that fails must end
  * the process with a non-zero code rather than leave it running. Each case runs in a fresh JVM.
  */
class BenchmarkLaunchSpec extends AnyFlatSpec with Matchers:

  final private case class ChildRun(exitCode: Int, output: String)

  /** Runs `main` in a fresh JVM. Without a `readyMarker` the child has `startBudget` to exit. With one, the child has
    * `startBudget` to print the marker (or exit), and then `exitBudget` to exit: a slow start on a loaded runner is
    * waited for, while a child that opened its windows and does not exit is still caught quickly. A child that does not
    * exit in time is stopped and reported with exit code -1.
    */
  private def runChild(
    main: String,
    workingDirectory: Path,
    headless: Boolean,
    arguments: List[String] = Nil,
    startBudget: FiniteDuration = 300.seconds,
    readyMarker: Option[String] = None,
    exitBudget: FiniteDuration = 30.seconds
  ): ChildRun =
    val javaBin = Paths.get(System.getProperty("java.home"), "bin", "java").toString
    val builder = new ProcessBuilder(
      (List(javaBin) ++ Option.when(headless)("-Djava.awt.headless=true") ++
        List("-cp", testClasspath, main) ++ arguments).asJava
    ).directory(workingDirectory.toFile).redirectErrorStream(true)
    val process = builder.start()
    val output  = new StringBuilder
    val ready   = new CompletableFuture[Unit]
    val reader = new Thread(() =>
      val lines = new BufferedReader(new InputStreamReader(process.getInputStream, StandardCharsets.UTF_8))
      Iterator.continually(Option(lines.readLine())).takeWhile(_.isDefined).flatten.foreach { line =>
        val _ = output.synchronized(output.append(line).append('\n'))
        if readyMarker.exists(line.contains) then
          val _ = ready.complete(())
      }
    )
    reader.setDaemon(true)
    reader.start()
    val exited = readyMarker match
      case None => process.waitFor(startBudget.toMillis, TimeUnit.MILLISECONDS)
      case Some(_) =>
        val progressed =
          Try(
            CompletableFuture.anyOf(ready, process.onExit()).get(startBudget.toMillis, TimeUnit.MILLISECONDS)
          ).isSuccess
        progressed && process.waitFor(exitBudget.toMillis, TimeUnit.MILLISECONDS)
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
      val run = runChild(
        "com.serenity.perf.WindowFailureChild",
        Paths.get("").toAbsolutePath,
        headless = false,
        readyMarker = Some(WindowFailureChild.WindowsOpen)
      )

      withClue(run.output) {
        run.exitCode should not be -1
        run.exitCode should not be 0
      }
    }
  }

  private val cwd = Paths.get("").toAbsolutePath

  "A child that reaches its marker later than the exit budget" should "be waited for and judged on how it exits" in {
    val run = runChild(
      "com.serenity.perf.MarkerChild",
      cwd,
      headless = true,
      List("slow", "3000"),
      startBudget = 60.seconds,
      readyMarker = Some(WindowFailureChild.WindowsOpen),
      exitBudget = 2.seconds
    )

    withClue(run.output)(run.exitCode shouldBe 1)
    run.output should include(WindowFailureChild.WindowsOpen)
  }

  "A child that reaches its marker and never exits" should "be stopped and reported as not exiting" in {
    val run = runChild(
      "com.serenity.perf.MarkerChild",
      cwd,
      headless = true,
      List("hang"),
      startBudget = 60.seconds,
      readyMarker = Some(WindowFailureChild.WindowsOpen),
      exitBudget = 3.seconds
    )

    withClue(run.output)(run.exitCode shouldBe -1)
    run.output should include(WindowFailureChild.WindowsOpen)
  }

  "A child that never reaches its marker" should "be stopped and reported as not exiting once its start budget is spent" in {
    val run = runChild(
      "com.serenity.perf.MarkerChild",
      cwd,
      headless = true,
      List("silent"),
      startBudget = 3.seconds,
      readyMarker = Some(WindowFailureChild.WindowsOpen),
      exitBudget = 30.seconds
    )

    withClue(run.output)(run.exitCode shouldBe -1)
    run.output should not include WindowFailureChild.WindowsOpen
  }

  private lazy val testClasspath: String =
    val stream = Option(getClass.getResourceAsStream("/crash/test-classpath.txt"))
      .getOrElse(fail("build.sbt did not generate crash/test-classpath.txt"))
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8).trim
    finally stream.close()
