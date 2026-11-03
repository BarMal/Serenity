package com.serenity.build

import java.nio.file.Path
import java.time.Instant

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import com.serenity.TestTemp
import com.serenity.trust.{CommandSource, ProjectKey, ProjectTrust, TrustGrant}
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BuildCoordinatorSpec extends AnyFlatSpec with Matchers:

  private val root: Path = TestTemp.directory("build-coordinator").toRealPath()

  private def trusted: ProjectTrust = ProjectTrust.empty.trust(ProjectKey.of(root), Instant.EPOCH)

  /** A provider that always finds `root` and counts how often it is opened and released. */
  final private class FakeProvider(
      name: String,
      session: BuildSession,
      opens: Ref[IO, Int],
      releases: Ref[IO, Int],
      failOpen: Boolean = false,
      source: CommandSource = CommandSource.BuiltIn
  ) extends BuildProvider:
    override val id: BuildProviderId = BuildProviderId(name)
    override def detect(start: Path): IO[List[Detection]] =
      IO.pure(List(Detection.Found(BuildCandidate(id, root, name, List(name), source))))

    override def open(candidate: BuildCandidate, grant: TrustGrant): Resource[IO, BuildSession] =
      Resource.make(
        opens.update(_ + 1) >> IO.raiseError(new RuntimeException(s"$name broke")).whenA(failOpen).as(session)
      )(_ => releases.update(_ + 1))

  private def session(events: TaskRequest => Stream[IO, TaskEvent]): BuildSession =
    new BuildSession:
      override val capabilities: ProviderCapabilities =
        ProviderCapabilities(TaskKind.values.toSet, canCancel = false, listsTargets = false, debug = false)
      override val targets: IO[List[BuildTargetRef]]                    = IO.pure(Nil)
      override def run(request: TaskRequest): Stream[IO, TaskEvent]     = events(request)
      override def buildFilesChanged(paths: List[Path]): IO[Unit]       = IO.unit
      override def debug(request: TaskRequest): Option[IO[DapEndpoint]] = None

  private val succeeding =
    session(_ => Stream(TaskEvent.Output(OutputChannel.Stdout, "ok"), TaskEvent.Finished(TaskOutcome.Succeeded)))

  private def counters = (Ref.of[IO, Int](0), Ref.of[IO, Int](0)).tupled

  "BuildCoordinator" should "open nothing for an untrusted project, and say why" in {
    val (events, opened) = (for
      (opens, releases) <- counters
      provider = FakeProvider("table", succeeding, opens, releases)
      events <- BuildCoordinator
        .resource(List(provider), IO.pure(ProjectTrust.empty))
        .use(_.run(root, TaskKind.Build).compile.toList)
      opened <- opens.get
    yield (events, opened)).unsafeRunSync()

    opened shouldBe 0
    events shouldBe List(
      TaskEvent.Finished(TaskOutcome.Unavailable("This project is not trusted, so table was not started."))
    )
  }

  it should "open a session once, reuse it, and release it when the coordinator is released" in {
    val (opened, released) = (for
      (opens, releases) <- counters
      provider = FakeProvider("table", succeeding, opens, releases)
      _ <- BuildCoordinator
        .resource(List(provider), IO.pure(trusted))
        .use(coordinator =>
          coordinator.run(root, TaskKind.Build).compile.drain >> coordinator.run(root, TaskKind.Test).compile.drain
        )
      opened   <- opens.get
      released <- releases.get
    yield (opened, released)).unsafeRunSync()

    opened shouldBe 1
    released shouldBe 1
  }

  it should "end every run with exactly one Finished, even when the session forgets or repeats it" in {
    val forgetful = session(_ => Stream.emit(TaskEvent.Output(OutputChannel.Log, "hi")))
    val repeating = session(_ =>
      Stream(
        TaskEvent.Finished(TaskOutcome.Succeeded),
        TaskEvent.Output(OutputChannel.Log, "late"),
        TaskEvent.Finished(TaskOutcome.Cancelled)
      )
    )
    def eventsOf(s: BuildSession) = (for
      (opens, releases) <- counters
      events <- BuildCoordinator
        .resource(List(FakeProvider("table", s, opens, releases)), IO.pure(trusted))
        .use(_.run(root, TaskKind.Build).compile.toList)
    yield events).unsafeRunSync()

    eventsOf(forgetful).last shouldBe TaskEvent.Finished(TaskOutcome.Failed("The build ended without a result."))
    eventsOf(repeating) shouldBe List(TaskEvent.Finished(TaskOutcome.Succeeded))
  }

  it should "turn a session's error into Failed rather than failing the stream" in {
    val broken = session(_ => Stream.raiseError[IO](new RuntimeException("socket closed")))
    val events = (for
      (opens, releases) <- counters
      events <- BuildCoordinator
        .resource(List(FakeProvider("table", broken, opens, releases)), IO.pure(trusted))
        .use(_.run(root, TaskKind.Build).compile.toList)
    yield events).unsafeRunSync()

    events shouldBe List(TaskEvent.Finished(TaskOutcome.Failed("socket closed")))
  }

  it should "drop a superseded run's later events and end it as Cancelled" in {
    val events = (for
      firstStarted <- Deferred[IO, Unit]
      never        <- Deferred[IO, Unit]
      slowThenFast = session(request =>
        if request.version == 1L then
          Stream.emit(TaskEvent.Output(OutputChannel.Stdout, "first")) ++
            Stream.eval(firstStarted.complete(())).drain ++
            Stream.eval(never.get).as(TaskEvent.Output(OutputChannel.Stdout, "stale"))
        else Stream.emit(TaskEvent.Finished(TaskOutcome.Succeeded))
      )
      (opens, releases) <- counters
      both <- BuildCoordinator
        .resource(List(FakeProvider("table", slowThenFast, opens, releases)), IO.pure(trusted))
        .use { coordinator =>
          for
            first  <- coordinator.run(root, TaskKind.Build).compile.toList.start
            _      <- firstStarted.get
            second <- coordinator.run(root, TaskKind.Build).compile.toList
            first  <- first.joinWithNever
          yield (first, second)
        }
    yield both).unsafeRunSync()

    val (first, second) = events
    first shouldBe List(TaskEvent.Output(OutputChannel.Stdout, "first"), TaskEvent.Finished(TaskOutcome.Cancelled))
    second shouldBe List(TaskEvent.Finished(TaskOutcome.Succeeded))
  }

  it should "fall back to the next provider when the preferred one cannot open" in {
    val (events, fallbackOpens) = (for
      (brokenOpens, brokenReleases) <- counters
      (tableOpens, tableReleases)   <- counters
      broken = FakeProvider("bsp", succeeding, brokenOpens, brokenReleases, failOpen = true)
      table  = FakeProvider("table", succeeding, tableOpens, tableReleases)
      events <- BuildCoordinator
        .resource(List(broken, table), IO.pure(trusted))
        .use(_.run(root, TaskKind.Build).compile.toList)
      opened <- tableOpens.get
    yield (events, opened)).unsafeRunSync()

    fallbackOpens shouldBe 1
    events.last shouldBe TaskEvent.Finished(TaskOutcome.Succeeded)
  }

  it should "skip a project-sourced command that has not been approved, and fall back past it" in {
    val bspFile = CommandSource.Project(root.resolve(".bsp/x.json"), sha256 = "abc")
    val (events, unapprovedOpens) = (for
      (bspOpens, bspReleases)     <- counters
      (tableOpens, tableReleases) <- counters
      bsp   = FakeProvider("bsp", succeeding, bspOpens, bspReleases, source = bspFile)
      table = FakeProvider("table", succeeding, tableOpens, tableReleases)
      events <- BuildCoordinator
        .resource(List(bsp, table), IO.pure(trusted))
        .use(_.run(root, TaskKind.Build).compile.toList)
      opened <- bspOpens.get
    yield (events, opened)).unsafeRunSync()

    unapprovedOpens shouldBe 0
    events.last shouldBe TaskEvent.Finished(TaskOutcome.Succeeded)
  }
