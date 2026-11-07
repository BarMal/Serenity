package com.serenity.process

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO}
import cats.syntax.all.*
import com.serenity.testkit.{RecordingLogger, VirtualTime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The service in virtual time, over fake processes: how long a stop may take, who it reaches, and what it lists. */
class ProcessServiceSpec extends AnyFlatSpec with Matchers:

  private val Grace     = 2.seconds
  private val ForceWait = 1.second
  private val stop      = StopPolicy(Grace, ForceWait)
  private val owner     = ProcessOwner("test")

  private def spec(command: String*): ProcessSpec = ProcessSpec(command.toList, owner, stop)

  private def withService[A](launcher: ProcessLauncher)(body: (ProcessService, RecordingLogger) => IO[A]): A =
    VirtualTime.runVirtual(
      for
        logger  <- RecordingLogger.create
        service <- ProcessService.create(launcher)(using logger)
        result  <- body(service, logger)
      yield result
    )

  private def timed[A](action: IO[A]): IO[(A, FiniteDuration)] =
    for
      before <- IO.monotonic
      result <- action
      after  <- IO.monotonic
    yield (result, after - before)

  "ProcessService.spawn" should "list a process while it is owned, with who started it, and forget it when released" in {
    val process  = new FakeProcess(4242)
    val launcher = new FakeLauncher(List(process))

    val (pid, during, after) = withService(launcher) { (service, _) =>
      val lsp = ProcessSpec(List("lsp-server", "--stdio"), ProcessOwner("lsp:scala"), stop)
      for
        seen  <- service.spawn(lsp).use(managed => service.running.map(listed => (managed.pid, listed)))
        after <- service.running
      yield (seen._1, seen._2, after)
    }

    pid shouldBe 4242L
    during.map(info => (info.owner, info.pid, info.command, info.detached)) shouldBe
      List((ProcessOwner("lsp:scala"), 4242L, List("lsp-server", "--stdio"), false))
    after shouldBe Nil
  }

  it should "only ask a process that exits on request to exit, and return at once" in {
    val process  = new FakeProcess(1)
    val launcher = new FakeLauncher(List(process))

    val (destroyed, forced, took) = withService(launcher) { (service, _) =>
      for
        result    <- timed(service.spawn(spec("x")).use(_ => IO.unit))
        destroyed <- process.destroyed
        forced    <- process.forced
      yield (destroyed, forced, result._2)
    }

    destroyed shouldBe true
    forced shouldBe false
    took should be <= 1.second
  }

  it should "force a process that ignores the request, and return within the grace plus the force wait" in {
    val process  = new FakeProcess(1, onDestroy = Response.Ignores)
    val launcher = new FakeLauncher(List(process))

    val (destroyed, forced, alive, took) = withService(launcher) { (service, _) =>
      for
        result    <- timed(service.spawn(spec("x")).use(_ => IO.unit))
        destroyed <- process.destroyed
        forced    <- process.forced
        alive     <- process.isAlive
      yield (destroyed, forced, alive, result._2)
    }

    destroyed shouldBe true
    forced shouldBe true
    alive shouldBe false
    took should be >= Grace
    took should be <= Grace + ForceWait
  }

  it should "stop the descendants it saw before the root died, since they are out of sight afterwards" in {
    val grandchild = new FakeControl(7, onDestroy = Response.Ignores)
    val process    = new FakeProcess(1, children = List(grandchild))
    val launcher   = new FakeLauncher(List(process))

    val (grandchildForced, grandchildAlive) = withService(launcher) { (service, _) =>
      for
        _      <- service.spawn(spec("sh", "-c", "build")).use(_ => IO.unit)
        forced <- grandchild.forced
        alive  <- grandchild.isAlive
      yield (forced, alive)
    }

    grandchildForced shouldBe true
    grandchildAlive shouldBe false
  }

  it should "give up after the force wait on a process that cannot be killed, and say which one" in {
    val process  = new FakeProcess(99, onDestroy = Response.Ignores, onForce = Response.Ignores)
    val launcher = new FakeLauncher(List(process))

    val (lines, took) = withService(launcher) { (service, logger) =>
      for
        result <- timed(service.spawn(ProcessSpec(List("stuck"), ProcessOwner("project-task"), stop)).use(_ => IO.unit))
        lines  <- logger.messages
      yield (lines, result._2)
    }

    took should be <= Grace + ForceWait + 1.second
    lines.exists(line => line.startsWith("warn") && line.contains("99") && line.contains("project-task")) shouldBe true
  }

  it should "stop and forget a process whose use is cancelled" in {
    val process  = new FakeProcess(1)
    val launcher = new FakeLauncher(List(process))

    val (destroyed, listed) = withService(launcher) { (service, _) =>
      for
        started   <- Deferred[IO, Unit]
        fiber     <- service.spawn(spec("x")).use(_ => started.complete(()) *> IO.never).start
        _         <- started.get
        _         <- fiber.cancel
        destroyed <- process.destroyed
        listed    <- service.running
      yield (destroyed, listed)
    }

    destroyed shouldBe true
    listed shouldBe Nil
  }

  it should "start nothing and list nothing when the launcher cannot start the process" in {
    val launcher = new FakeLauncher(Nil, failToStart = Some(new java.io.IOException("no such file")))

    val (outcome, listed) = withService(launcher) { (service, _) =>
      for
        outcome <- service.spawn(spec("missing")).use(_ => IO.unit).attempt
        listed  <- service.running
      yield (outcome, listed)
    }

    outcome.left.map(_.getMessage) shouldBe Left("no such file")
    listed shouldBe Nil
  }

  "ProcessService.run" should "return the exit code and what the command printed" in {
    val process  = new FakeProcess(1, output = IO.pure("hello\n".getBytes("UTF-8")), exit = 3)
    val launcher = new FakeLauncher(List(process))

    val result = withService(launcher)((service, _) => process.finish *> service.run(spec("echo", "hello"), 10.seconds))

    result shouldBe Some(ProcessResult(3, "hello\n"))
  }

  it should "answer None, and stop the process, when the command outlives the timeout" in {
    val process  = new FakeProcess(1)
    val launcher = new FakeLauncher(List(process))

    val (result, destroyed, listed, took) = withService(launcher) { (service, _) =>
      for
        timedRun  <- timed(service.run(spec("hang"), 3.seconds))
        destroyed <- process.destroyed
        listed    <- service.running
      yield (timedRun._1, destroyed, listed, timedRun._2)
    }

    result shouldBe None
    destroyed shouldBe true
    listed shouldBe Nil
    took should be >= 3.seconds
    took should be <= 3.seconds + Grace + ForceWait
  }

  it should "answer None when the command cannot be started" in {
    val launcher = new FakeLauncher(Nil, failToStart = Some(new java.io.IOException("no such file")))

    val result = withService(launcher)((service, _) => service.run(spec("missing"), 10.seconds))

    result shouldBe None
  }

  it should "still return within the timeout and the stop bounds when the output never closes" in {
    val process  = new FakeProcess(1, onDestroy = Response.Ignores, output = IO.never)
    val launcher = new FakeLauncher(List(process))

    val (result, took) = withService(launcher)((service, _) => timed(service.run(spec("holds-the-pipe"), 3.seconds)))

    result shouldBe None
    took should be <= 3.seconds + Grace + ForceWait + 1.second
  }

  "ProcessService.launchDetached" should "list the process as detached and leave it running when the service stops" in {
    val process  = new FakeProcess(5)
    val launcher = new FakeLauncher(List(process))

    val (listed, destroyed, alive) = withService(launcher) { (service, _) =>
      for
        _         <- service.launchDetached(spec("xdg-open", "/logs"))
        listed    <- service.running
        _         <- service.stopAll
        destroyed <- process.destroyed
        alive     <- process.isAlive
      yield (listed.map(info => (info.pid, info.detached)), destroyed, alive)
    }

    listed shouldBe List((5L, true))
    destroyed shouldBe false
    alive shouldBe true
  }

  it should "stop listing a detached process once it has exited" in {
    val process  = new FakeProcess(5)
    val launcher = new FakeLauncher(List(process))

    val (before, after) = withService(launcher) { (service, _) =>
      for
        _      <- service.launchDetached(spec("xdg-open", "/logs"))
        before <- service.running
        _      <- process.finish
        _      <- IO.sleep(1.second)
        after  <- service.running
      yield (before.size, after.size)
    }

    before shouldBe 1
    after shouldBe 0
  }

  "ProcessService.stopAll" should "stop every owned process, within the bound, and leave the detached one" in {
    val first    = new FakeProcess(1, onDestroy = Response.Ignores)
    val second   = new FakeProcess(2)
    val detached = new FakeProcess(3)
    val launcher = new FakeLauncher(List(first, second, detached))

    val (firstAlive, secondAlive, detachedAlive, took) = withService(launcher) { (service, _) =>
      for
        hold     <- Deferred[IO, Unit]
        started1 <- Deferred[IO, Unit]
        started2 <- Deferred[IO, Unit]
        fiber1   <- service.spawn(spec("a")).use(_ => started1.complete(()) *> hold.get).start
        fiber2   <- service.spawn(spec("b")).use(_ => started2.complete(()) *> hold.get).start
        _        <- started1.get *> started2.get
        _        <- service.launchDetached(spec("c"))
        stopped  <- timed(service.stopAll)
        states   <- (first.isAlive, second.isAlive, detached.isAlive).tupled
        _        <- hold.complete(())
        _        <- fiber1.join *> fiber2.join
      yield (states._1, states._2, states._3, stopped._2)
    }

    firstAlive shouldBe false
    secondAlive shouldBe false
    detachedAlive shouldBe true
    took should be <= Grace + ForceWait + 1.second
  }
