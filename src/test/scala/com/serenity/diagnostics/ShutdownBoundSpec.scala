package com.serenity.diagnostics

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.jdk.CollectionConverters.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A JVM whose shutdown hangs must still end: after the bound, with the log saying which step was stuck and where every
  * thread was (#2507). Each case runs in a fresh JVM, since the bound is `Runtime.halt`, which would end this one.
  */
class ShutdownBoundSpec extends AnyFlatSpec with Matchers:

  final private case class ChildRun(exitCode: Option[Int], output: List[String], elapsedMillis: Long)

  private val posix = !System.getProperty("os.name", "").toLowerCase.contains("windows")

  private lazy val testClasspath: String =
    val stream = Option(getClass.getResourceAsStream("/crash/test-classpath.txt"))
      .getOrElse(fail("build.sbt did not generate crash/test-classpath.txt"))
    try new String(stream.readAllBytes(), StandardCharsets.UTF_8).trim
    finally stream.close()

  /** Starts the child, waits for its READY line, then lets `afterReady` act on the process, and waits for it to end. */
  private def runChild(mode: String, afterReady: Process => Unit): ChildRun =
    val javaBin = java.nio.file.Paths.get(System.getProperty("java.home"), "bin", "java").toString
    val process = new ProcessBuilder(
      List(
        javaBin,
        "-Djava.awt.headless=true",
        "-Dchild.hookTimeoutSeconds=3",
        "-Dchild.dumpAfterSeconds=1",
        "-cp",
        testClasspath,
        "com.serenity.diagnostics.ShutdownBoundChild",
        mode
      ).asJava
    ).redirectErrorStream(true).start()
    val lines = new java.util.concurrent.CopyOnWriteArrayList[String]()
    val ready = new CountDownLatch(1)
    val reader = new Thread(() =>
      val in = new BufferedReader(new InputStreamReader(process.getInputStream, StandardCharsets.UTF_8))
      Iterator.continually(in.readLine()).takeWhile(_ != null).foreach { line =>
        lines.add(line)
        if line == "READY" then ready.countDown()
      }
    )
    reader.setDaemon(true)
    reader.start()
    if !ready.await(60, TimeUnit.SECONDS) then
      process.destroyForcibly()
      fail(s"the child never printed READY:\n${lines.asScala.mkString("\n")}")
    val startedAt = System.nanoTime()
    afterReady(process)
    val exited  = process.waitFor(30, TimeUnit.SECONDS)
    val elapsed = (System.nanoTime() - startedAt) / 1_000_000
    if !exited then process.destroyForcibly()
    reader.join(5000)
    ChildRun(Option.when(exited)(process.exitValue()), lines.asScala.toList, elapsed)

  "A JVM told to terminate while a release hangs" should "dump the threads and exit within the hook bound" in {
    assume(posix, "Process.destroy ends a Windows process without running its shutdown hooks")
    // `Process.destroy` sends SIGTERM and then closes this side of the child's pipes, which loses the dump the child
    // prints a second later. The handle's `destroy` sends the same signal and leaves the pipes open.
    val run = runChild("signal", _.toHandle.destroy())

    withClue(run.output.mkString("\n")) {
      run.exitCode should not be empty
      run.elapsedMillis should be < 15000L
      run.output.exists(_.contains("steps that started releasing and have not finished: stuck window")) shouldBe true
      run.output.exists(_.startsWith("\"main\" (")) shouldBe true
    }
  }

  "A quit whose release hangs" should "dump the threads and halt with the watchdog's exit code" in {
    val run = runChild("quit-stuck", _ => ())

    withClue(run.output.mkString("\n")) {
      run.exitCode shouldBe Some(ShutdownWatchdog.HaltExitCode)
      run.output.exists(_.contains("steps that started releasing and have not finished: stuck window")) shouldBe true
    }
  }

  "A quit that finishes" should "end by itself, without the watchdog halting or reporting" in {
    val run = runChild("quit-clean", _ => ())

    withClue(run.output.mkString("\n")) {
      run.exitCode shouldBe Some(0)
      run.output.exists(_.contains("[SHUTDOWN]")) shouldBe false
    }
  }

  "A watchdog disarmed before its bound" should "not halt the JVM that goes on running" in {
    val run = runChild("disarmed", _ => ())

    withClue(run.output.mkString("\n")) {
      run.exitCode shouldBe Some(0)
      run.output.exists(_.contains("[SHUTDOWN]")) shouldBe false
    }
  }
