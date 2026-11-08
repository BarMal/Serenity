package com.serenity.process

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*
import scala.jdk.OptionConverters.*
import scala.util.Try

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.testkit.RecordingLogger
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The service over the JDK's own processes. POSIX only: how a tree is ended on Windows is a later step. */
class ProcessServiceRealSpec extends AnyFlatSpec with Matchers:

  private val posix = !System.getProperty("os.name", "").toLowerCase.contains("windows")

  private val owner = ProcessOwner("real-spec")

  private def withService[A](body: ProcessService => IO[A]): A =
    (for
      logger  <- RecordingLogger.create
      service <- ProcessService.create(ProcessLauncher.system)(using logger)
      result  <- body(service)
    yield result).unsafeRunSync()

  /** Gone, or only waiting to be reaped: in a container the init process may never reap, and `isAlive` stays true. */
  private def gone(pid: Long): Boolean =
    ProcessHandle.of(pid).toScala.forall { handle =>
      !handle.isAlive ||
      Try(Files.readString(Path.of(s"/proc/$pid/stat"))).toOption.exists { stat =>
        stat.substring(stat.lastIndexOf(')') + 2).startsWith("Z")
      }
    }

  private def eventually(within: FiniteDuration)(condition: => Boolean): Boolean =
    def poll(remaining: FiniteDuration): IO[Boolean] =
      IO(condition).flatMap {
        case true                                => IO.pure(true)
        case false if remaining <= Duration.Zero => IO.pure(false)
        case false                               => IO.sleep(50.millis) *> poll(remaining - 50.millis)
      }
    poll(within).unsafeRunSync()

  "A process started through the service" should "leave no descendant behind once it is released" in {
    assume(posix, "the tree kill is POSIX-only for now")
    // A shell that starts a grandchild and waits for it: the shape of a build tool or a git hook.
    val command = ProcessSpec(List("sh", "-c", "sleep 300 & echo $!; wait"), owner, StopPolicy(1.second, 1.second))

    val (rootPid, grandchildPid) = withService { service =>
      service.spawn(command).use { process =>
        IO.blocking {
          val reader = new java.io.BufferedReader(new java.io.InputStreamReader(process.stdout, StandardCharsets.UTF_8))
          (process.pid, reader.readLine().trim.toLong)
        }
      }
    }

    eventually(5.seconds)(gone(grandchildPid)) shouldBe true
    eventually(5.seconds)(gone(rootPid)) shouldBe true
  }

  "ProcessService.run" should "return what a real command printed, and its exit code" in {
    assume(posix, "needs sh")

    val result = withService(_.run(ProcessSpec(List("sh", "-c", "echo hello; exit 3"), owner), 10.seconds))

    result shouldBe Some(ProcessResult(3, "hello\n"))
  }

  it should "answer None quickly, and end the process, when a real command outlives the timeout" in {
    assume(posix, "needs sh")
    val command = ProcessSpec(List("sh", "-c", "sleep 300 & echo $!; wait"), owner, StopPolicy(1.second, 1.second))

    val started = System.nanoTime()
    val result  = withService(_.run(command, 500.millis))
    val tookMs  = (System.nanoTime() - started) / 1_000_000

    result shouldBe None
    tookMs should be < 8000L
  }

  it should "answer None for a command that does not exist" in {
    val result = withService(_.run(ProcessSpec(List("serenity-no-such-command-xyz"), owner), 10.seconds))

    result shouldBe None
  }
