package com.serenity.project

import java.nio.file.Paths

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final private case class FakeProcessHandle(
    outputStarted: Deferred[IO, Unit],
    outputRelease: Deferred[IO, Unit],
    destroyed: Ref[IO, Boolean]
) extends ProjectTaskRunner.ProcessHandle:
  override def output: IO[String] =
    outputStarted.complete(()) >> outputRelease.get.as("done")

  override def waitFor: IO[Int] =
    IO.pure(0)

  override def destroy: IO[Unit] =
    destroyed.set(true)

final private case class FakeProcessStarter(handle: ProjectTaskRunner.ProcessHandle)
    extends ProjectTaskRunner.ProcessStarter:
  override def start(command: ProjectTaskCommand): IO[ProjectTaskRunner.ProcessHandle] =
    IO.pure(handle)

class ProjectTaskRunnerSpec extends AnyFlatSpec with Matchers:

  "ProjectTaskRunner" should "destroy a running process when canceled" in {
    val result =
      (for
        outputStarted <- Deferred[IO, Unit]
        outputRelease <- Deferred[IO, Unit]
        destroyed     <- Ref.of[IO, Boolean](false)
        command = ProjectTaskCommand(
          kind = ProjectTaskKind.Build,
          ecosystemLabel = "test",
          workingDirectory = Paths.get("."),
          executable = "test-build",
          arguments = Nil
        )
        handle  = FakeProcessHandle(outputStarted, outputRelease, destroyed)
        starter = FakeProcessStarter(handle)
        fiber        <- ProjectTaskRunner.run(command, starter).start
        _            <- outputStarted.get
        _            <- fiber.cancel
        wasDestroyed <- destroyed.get
      yield wasDestroyed).unsafeRunSync()

    result shouldBe true
  }
