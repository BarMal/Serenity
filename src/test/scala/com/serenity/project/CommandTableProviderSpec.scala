package com.serenity.project

import java.nio.file.{Files, Path}
import java.time.Instant

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.build.{BuildCandidate, Detection, OutputChannel, TaskEvent, TaskKind, TaskOutcome, TaskRequest}
import com.serenity.trust.{CommandSource, ProjectKey, ProjectTrust, TrustGrant}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandTableProviderSpec extends AnyFlatSpec with Matchers:

  private def withProject[A](marker: String)(body: Path => A): A =
    val root = Files.createTempDirectory("command-table")
    Files.writeString(root.resolve(marker), "")
    try body(root.toRealPath())
    finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(path => Files.deleteIfExists(path): Unit)

  private def grantFor(candidate: BuildCandidate): TrustGrant =
    val projectKey = ProjectKey.of(candidate.root)
    ProjectTrust.empty
      .trust(projectKey, Instant.EPOCH)
      .grant(projectKey, candidate.source, candidate.command)
      .fold(refusal => fail(s"expected a grant, got $refusal"), identity)

  /** Today's table, written out so that moving it (L9) cannot change a command unnoticed. */
  private val golden: List[(String, String, Map[TaskKind, List[String]])] = List(
    (
      "build.sbt",
      "sbt",
      Map(
        TaskKind.Build        -> List("sbt", "compile"),
        TaskKind.Test         -> List("sbt", "test"),
        TaskKind.Run          -> List("sbt", "run"),
        TaskKind.Debug        -> List("sbt", "-jvm-debug", "5005", "run"),
        TaskKind.Dependencies -> List("sbt", "update")
      )
    ),
    (
      "package.json",
      "npm",
      Map(
        TaskKind.Build        -> List("npm", "run", "build"),
        TaskKind.Test         -> List("npm", "test"),
        TaskKind.Run          -> List("npm", "run", "start"),
        TaskKind.Debug        -> List("npm", "run", "debug"),
        TaskKind.Dependencies -> List("npm", "ls", "--depth=0")
      )
    ),
    (
      "Cargo.toml",
      "cargo",
      Map(
        TaskKind.Build        -> List("cargo", "build"),
        TaskKind.Test         -> List("cargo", "test"),
        TaskKind.Run          -> List("cargo", "run"),
        TaskKind.Debug        -> List("cargo", "run"),
        TaskKind.Dependencies -> List("cargo", "tree")
      )
    ),
    (
      "pom.xml",
      "maven",
      Map(
        TaskKind.Build        -> List("mvn", "compile"),
        TaskKind.Test         -> List("mvn", "test"),
        TaskKind.Run          -> List("mvn", "exec:java"),
        TaskKind.Debug        -> List("mvnDebug", "test"),
        TaskKind.Dependencies -> List("mvn", "dependency:tree")
      )
    ),
    (
      "build.gradle",
      "gradle",
      Map(
        TaskKind.Build        -> List("gradle", "build"),
        TaskKind.Test         -> List("gradle", "test"),
        TaskKind.Run          -> List("gradle", "run"),
        TaskKind.Debug        -> List("gradle", "test", "--debug-jvm"),
        TaskKind.Dependencies -> List("gradle", "dependencies")
      )
    ),
    (
      "go.mod",
      "go",
      Map(
        TaskKind.Build        -> List("go", "build", "./..."),
        TaskKind.Test         -> List("go", "test", "./..."),
        TaskKind.Run          -> List("go", "run", "."),
        TaskKind.Debug        -> List("go", "test", "-c", "."),
        TaskKind.Dependencies -> List("go", "list", "-m", "all")
      )
    ),
    (
      "Makefile",
      "make",
      Map(
        TaskKind.Build        -> List("make"),
        TaskKind.Test         -> List("make", "test"),
        TaskKind.Run          -> List("make", "run"),
        TaskKind.Debug        -> List("make", "debug"),
        TaskKind.Dependencies -> List("make", "dependencies")
      )
    )
  )

  private def recordingProvider(exitCode: Int, output: List[String]) =
    Ref.of[IO, List[ProjectTaskCommand]](Nil).map { ran =>
      val provider = CommandTableProvider { (command, onOutput) =>
        ran.update(_ :+ command) >>
          output.traverse_(onOutput) >>
          IO.pure(ProjectTaskResult(command, exitCode, output.mkString))
      }
      (provider, ran)
    }

  "CommandTableProvider" should "run exactly today's command, in the project root, for every ecosystem and kind" in {
    for (marker, ecosystem, commands) <- golden do
      withProject(marker) { root =>
        val ran = (for
          (provider, ran) <- recordingProvider(0, Nil)
          detected        <- provider.detect(root)
          candidate = detected.collectFirst { case Detection.Found(c) => c }.getOrElse(fail(s"$marker not found"))
          _ <- provider
            .open(candidate, grantFor(candidate))
            .use(session =>
              TaskKind.values.toList.traverse_(kind => session.run(TaskRequest(kind, Nil, 1L)).compile.drain)
            )
          commands <- ran.get
        yield (candidate, commands)).unsafeRunSync()

        val (candidate, recorded) = ran
        candidate.label shouldBe ecosystem
        candidate.source shouldBe CommandSource.BuiltIn
        recorded.map(_.commandLine) shouldBe TaskKind.values.toList.map(commands)
        recorded.map(_.workingDirectory).distinct shouldBe List(root)
      }
  }

  it should "find nothing, and run nothing, where there is no marker" in {
    val root = Files.createTempDirectory("command-table-empty")
    try
      val (detected, ran) = (for
        (provider, ran) <- recordingProvider(0, Nil)
        detected        <- provider.detect(root)
        commands        <- ran.get
      yield (detected, commands)).unsafeRunSync()
      ran shouldBe empty
      detected.collect { case found: Detection.Found => found } shouldBe empty
    finally Files.deleteIfExists(root): Unit
  }

  it should "not run anything to detect a project" in withProject("Cargo.toml") { root =>
    val ran = (for
      (provider, ran) <- recordingProvider(0, Nil)
      _               <- provider.detect(root)
      commands        <- ran.get
    yield commands).unsafeRunSync()

    ran shouldBe empty
  }

  it should "report output in order, then exactly one Finished from the exit code" in withProject("Cargo.toml") {
    root =>
      def events(exitCode: Int) = (for
        (provider, _) <- recordingProvider(exitCode, List("one\n", "two\n"))
        detected      <- provider.detect(root)
        candidate = detected.collectFirst { case Detection.Found(c) => c }.getOrElse(fail("not found"))
        events <- provider
          .open(candidate, grantFor(candidate))
          .use(_.run(TaskRequest(TaskKind.Build, Nil, 1L)).compile.toList)
      yield events).unsafeRunSync()

      events(0).collect { case TaskEvent.Output(channel, text) => channel -> text } shouldBe
        List(OutputChannel.Stdout -> "one\n", OutputChannel.Stdout -> "two\n")
      events(0).lastOption shouldBe Some(TaskEvent.Finished(TaskOutcome.Succeeded))
      events(3).lastOption shouldBe Some(TaskEvent.Finished(TaskOutcome.Failed("cargo build exited with code 3")))
      events(0).count(_.isInstanceOf[TaskEvent.Finished]) shouldBe 1
  }

  it should "finish with Failed, not an error, when the command cannot start" in withProject("go.mod") { root =>
    val provider = CommandTableProvider((_, _) => IO.raiseError(new java.io.IOException("go: not found")))
    val events = (for
      detected <- provider.detect(root)
      candidate = detected.collectFirst { case Detection.Found(c) => c }.getOrElse(fail("not found"))
      events <- provider
        .open(candidate, grantFor(candidate))
        .use(_.run(TaskRequest(TaskKind.Test, Nil, 1L)).compile.toList)
    yield events).unsafeRunSync()

    events.lastOption shouldBe Some(
      TaskEvent.Finished(TaskOutcome.Failed("go test ./... could not start: go: not found"))
    )
  }
