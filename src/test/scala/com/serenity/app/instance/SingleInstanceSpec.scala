package com.serenity.app.instance

import java.io.IOException
import java.nio.file.{Files, Path, Paths}

import scala.concurrent.duration.DurationInt

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref, Resource}
import com.serenity.TestTemp
import com.serenity.io.DirectoryTree
import com.serenity.testkit.VirtualTime
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

/** #2023: what a launch does given the lock and the running instance -- fakes for each case, then the real thing. */
class SingleInstanceSpec extends AnyFlatSpec with Matchers:

  private given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val logger: Logger[IO]  = LoggerFactory[IO].getLogger(using LoggerName("SingleInstanceSpec"))

  private val notes        = Paths.get("/work/notes.md")
  private val isolatedRoot = Paths.get("/tmp/serenity-isolated-session-1")

  private def coordination(
    lock: Resource[IO, LockAttempt],
    deliveries: List[Delivery] = Nil,
    forwarded: Option[Ref[IO, List[List[Path]]]] = None,
    serve: Resource[IO, Stream[IO, List[Path]]] = Resource.pure(Stream.emit(List(notes)))
  ): IO[InstanceCoordination] =
    Ref.of[IO, List[Delivery]](deliveries).map { remaining =>
      InstanceCoordination(
        acquireLock = lock,
        serve = serve,
        forward = paths =>
          forwarded.fold(IO.unit)(_.update(_ :+ paths)) >>
            remaining.modify(next => (next.drop(1), next.headOption.getOrElse(Delivery.Unreachable))),
        isolatedSessionRoot = IO.pure(isolatedRoot),
        claimRetryDelay = 1.milli
      )
    }

  private def roleOf(coordination: IO[InstanceCoordination], paths: List[Path] = List(notes)): LaunchRole =
    coordination.flatMap(SingleInstance.claim(_, paths, logger).use(IO.pure)).unsafeRunSync()

  "SingleInstance.claim" should "run as the primary, serving forwarded opens, when it takes the lock" in {
    val opens = coordination(Resource.pure(LockAttempt.Acquired)).flatMap { coordination =>
      SingleInstance.claim(coordination, Nil, logger).use {
        case LaunchRole.Primary(forwardedOpens) => forwardedOpens.compile.toList
        case other                              => IO.raiseError(new AssertionError(s"expected Primary, got $other"))
      }
    }

    opens.unsafeRunSync() shouldBe List(List(notes))
  }

  it should "forward its paths and step aside when another instance holds the lock" in {
    val program = for
      forwarded <- Ref.of[IO, List[List[Path]]](Nil)
      role <- coordination(Resource.pure(LockAttempt.HeldElsewhere), List(Delivery.Delivered), Some(forwarded))
        .flatMap(SingleInstance.claim(_, List(notes), logger).use(IO.pure))
      sent <- forwarded.get
    yield (role, sent)

    program.unsafeRunSync() shouldBe (LaunchRole.Forwarded, List(List(notes)))
  }

  it should "keep trying while the running instance is still starting its listener" in {
    val deliveries = List(Delivery.Unreachable, Delivery.Unreachable, Delivery.Delivered)

    roleOf(coordination(Resource.pure(LockAttempt.HeldElsewhere), deliveries)) shouldBe LaunchRole.Forwarded
  }

  it should "run on an isolated session, never the shared one, when forwarding fails" in {
    roleOf(coordination(Resource.pure(LockAttempt.HeldElsewhere), List(Delivery.Failed))) shouldBe
      LaunchRole.Isolated(isolatedRoot)
    roleOf(coordination(Resource.pure(LockAttempt.HeldElsewhere))) shouldBe LaunchRole.Isolated(isolatedRoot)
  }

  it should "say so when it runs isolated, and stay quiet otherwise" in {
    LaunchRole.Isolated(isolatedRoot).notice.getOrElse("") should include(isolatedRoot.toString)
    LaunchRole.Isolated(isolatedRoot).sessionRootOverride shouldBe Some(isolatedRoot)
    LaunchRole.Primary(Stream.empty).notice shouldBe None
    LaunchRole.Primary(Stream.empty).sessionRootOverride shouldBe None
  }

  it should "still run as the primary, but take no socket another instance may own, when the lock cannot be taken" in {
    val failingLock = Resource.eval(IO.raiseError[LockAttempt](new IOException("read-only config directory")))
    val noServer    = Resource.eval(IO.raiseError[Stream[IO, List[Path]]](new AssertionError("must not serve")))

    roleOf(coordination(failingLock, serve = noServer)) should matchPattern { case LaunchRole.Primary(_) => }
  }

  // Windows releases a killed process's file lock asynchronously, so a relaunch after a crash can briefly see a lock
  // that is held by nobody. Only when nothing answers the forward is that worth waiting out.
  private val retryDelay = 200.millis

  final private case class Claim(role: LaunchRole, lockAttempts: Int, elapsed: scala.concurrent.duration.FiniteDuration)

  /** A lock that reads as held until it has been tried `heldFor` times; `Int.MaxValue` never frees. */
  private def claimVirtually(
    heldFor: Int,
    deliveries: List[Delivery],
    claimAttempts: Int = 15
  ): Claim =
    VirtualTime.runVirtual {
      for
        tries <- Ref.of[IO, Int](0)
        lock = Resource.eval(
          tries.updateAndGet(_ + 1).map(n => if n > heldFor then LockAttempt.Acquired else LockAttempt.HeldElsewhere)
        )
        remaining <- Ref.of[IO, List[Delivery]](deliveries)
        coordination = InstanceCoordination(
          acquireLock = lock,
          serve = Resource.pure(Stream.empty),
          forward = _ => remaining.modify(next => (next.drop(1), next.headOption.getOrElse(Delivery.Unreachable))),
          isolatedSessionRoot = IO.pure(isolatedRoot),
          claimAttempts = claimAttempts,
          claimRetryDelay = retryDelay
        )
        started <- IO.monotonic
        role    <- SingleInstance.claim(coordination, List(notes), logger).use(IO.pure)
        elapsed <- IO.monotonic.map(_ - started)
        count   <- tries.get
      yield Claim(role, count, elapsed)
    }

  it should "take the lock once a holder that cannot be reached lets go of it" in {
    val claim = claimVirtually(heldFor = 3, deliveries = Nil)

    claim.role should matchPattern { case LaunchRole.Primary(_) => }
    claim.lockAttempts shouldBe 4
    claim.elapsed shouldBe retryDelay * 3
  }

  it should "forward at once, without retrying the lock, when the holder answers" in {
    val claim = claimVirtually(heldFor = Int.MaxValue, deliveries = List(Delivery.Delivered))

    claim.role shouldBe LaunchRole.Forwarded
    claim.lockAttempts shouldBe 1
    claim.elapsed.toMillis shouldBe 0L
  }

  it should "give up on an unreachable holder after the bounded number of attempts" in {
    val claim = claimVirtually(heldFor = Int.MaxValue, deliveries = Nil, claimAttempts = 15)

    claim.role shouldBe LaunchRole.Isolated(isolatedRoot)
    claim.lockAttempts shouldBe 15
    claim.elapsed shouldBe retryDelay * 14
  }

  it should "not wait on a holder that answered but failed to acknowledge" in {
    val claim = claimVirtually(heldFor = Int.MaxValue, deliveries = List(Delivery.Failed))

    claim.role shouldBe LaunchRole.Isolated(isolatedRoot)
    claim.lockAttempts shouldBe 1
    claim.elapsed.toMillis shouldBe 0L
  }

  it should "wait out a holder that is slow to listen and then to release, within one budget" in {
    val claim = claimVirtually(heldFor = 5, deliveries = List(Delivery.Unreachable, Delivery.Unreachable))

    claim.role should matchPattern { case LaunchRole.Primary(_) => }
    claim.lockAttempts shouldBe 6
  }

  "SingleInstance.forConfigDirectory" should "forward a second launch's file to the first through the real lock" in {
    val configDirectory = TestTemp.directory("si")
    val real            = SingleInstance.forConfigDirectory(configDirectory, logger)

    val program = SingleInstance.claim(real, Nil, logger).use {
      case LaunchRole.Primary(forwardedOpens) =>
        for
          second   <- SingleInstance.claim(real, List(notes), logger).use(IO.pure)
          received <- forwardedOpens.take(1).compile.toList.timeout(10.seconds)
        yield (second, received)
      case other => IO.raiseError(new AssertionError(s"expected Primary, got $other"))
    }

    program.unsafeRunSync() shouldBe (LaunchRole.Forwarded, List(List(notes)))
  }

  it should "remove an isolated launch's session root when the launch ends" in {
    val program = for
      created <- Ref.of[IO, Option[Path]](None)
      base    <- coordination(Resource.pure(LockAttempt.HeldElsewhere), deliveries = List(Delivery.Unreachable))
      isolated = base.copy(
        isolatedSessionRoot =
          IO.blocking(TestTemp.directory("si-isolated-root")).flatTap(dir => created.set(Some(dir))),
        claimAttempts = 1
      )
      role <- SingleInstance.claim(isolated, List(notes), logger).use { role =>
        IO.blocking(role.sessionRootOverride.foreach(root => Files.writeString(root.resolve("session.json"), "{}")))
          .as(role)
      }
      root <- created.get
    yield (role.sessionRootOverride, root)

    val (override_, root) = program.unsafeRunSync()
    override_ should not be empty
    root.map(Files.exists(_)) shouldBe Some(false)
  }

  it should "give an isolated launch a session root outside the shared config directory" in {
    val configDirectory = TestTemp.directory("si-isolated")
    val root            = SingleInstance.forConfigDirectory(configDirectory, logger).isolatedSessionRoot.unsafeRunSync()

    try
      root.startsWith(configDirectory) shouldBe false
      Files.isDirectory(root) shouldBe true
    finally DirectoryTree.deleteBlocking(root)
  }
