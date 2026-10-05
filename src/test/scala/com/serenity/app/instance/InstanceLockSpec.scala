package com.serenity.app.instance

import java.io.{BufferedReader, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2023: the lock that keeps two Serenity processes from sharing one session. */
class InstanceLockSpec extends AnyFlatSpec with Matchers:

  private def lockFileIn(directory: Path): Path = directory.resolve("nested").resolve("instance.lock")

  "InstanceLock" should "be acquired in a fresh config directory, creating it" in {
    val lockFile = lockFileIn(Files.createTempDirectory("instance-lock-fresh"))

    InstanceLock.acquire(lockFile).use(attempt => IO(attempt)).unsafeRunSync() shouldBe LockAttempt.Acquired
    Files.exists(lockFile) shouldBe true
  }

  it should "report the lock held while another holder has it, and be acquirable once released" in {
    val lockFile = lockFileIn(Files.createTempDirectory("instance-lock-held"))

    val whileHeld  = InstanceLock.acquire(lockFile).use(_ => InstanceLock.acquire(lockFile).use(IO.pure))
    val afterwards = InstanceLock.acquire(lockFile).use(IO.pure)

    whileHeld.unsafeRunSync() shouldBe LockAttempt.HeldElsewhere
    afterwards.unsafeRunSync() shouldBe LockAttempt.Acquired
  }

  // The only place a real second process is observable: a lock held by another JVM, and that JVM dying without
  // releasing it, which is what a crash leaves behind. The lock file itself stays on disk; it must not block.
  it should "see another process's lock, and take over once that process is killed" in {
    val lockFile = lockFileIn(Files.createTempDirectory("instance-lock-process"))
    Files.createDirectories(lockFile.getParent)

    val outcome = holdingProcess(lockFile).use { child =>
      for
        heldElsewhere <- InstanceLock.acquire(lockFile).use(IO.pure)
        _             <- IO.blocking(child.destroyForcibly().waitFor(30, TimeUnit.SECONDS))
        afterCrash    <- InstanceLock.acquire(lockFile).use(IO.pure)
      yield (heldElsewhere, afterCrash)
    }

    outcome.unsafeRunSync() shouldBe (LockAttempt.HeldElsewhere, LockAttempt.Acquired)
    Files.exists(lockFile) shouldBe true
  }

  /** A child JVM, run from a single Java source file, that locks `lockFile` and holds it until it is killed. */
  private def holdingProcess(lockFile: Path): Resource[IO, Process] =
    val source =
      """import java.nio.channels.FileChannel;
        |import java.nio.file.*;
        |public class HoldLock {
        |  public static void main(String[] args) throws Exception {
        |    FileChannel channel = FileChannel.open(Path.of(args[0]), StandardOpenOption.CREATE, StandardOpenOption.WRITE);
        |    System.out.print((channel.tryLock() == null ? "busy" : "locked") + "\n");
        |    System.out.flush();
        |    Thread.sleep(Long.MAX_VALUE);
        |  }
        |}
        |""".stripMargin
    val start = IO.blocking {
      val sourceFile = Files.writeString(Files.createTempDirectory("hold-lock").resolve("HoldLock.java"), source)
      val java       = Paths.get(System.getProperty("java.home"), "bin", "java").toString
      new ProcessBuilder(java, sourceFile.toString, lockFile.toString)
        .redirectError(ProcessBuilder.Redirect.DISCARD)
        .start()
    }
    Resource.make(start)(child => IO.blocking(child.destroyForcibly()).void).evalTap { child =>
      IO.blocking(new BufferedReader(new InputStreamReader(child.getInputStream, StandardCharsets.UTF_8)).readLine())
        .flatMap(line => IO.raiseUnless(line == "locked")(new IllegalStateException(s"child said: $line")))
    }
