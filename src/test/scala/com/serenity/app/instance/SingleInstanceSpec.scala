package com.serenity.app.instance

import java.io.IOException
import java.nio.file.{Files, Path, Paths}

import scala.concurrent.duration.DurationInt

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref, Resource}
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
        forwardRetryDelay = 1.milli
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

  "SingleInstance.forConfigDirectory" should "forward a second launch's file to the first through the real lock" in {
    val configDirectory = Files.createTempDirectory("si")
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

  it should "give an isolated launch a session root outside the shared config directory" in {
    val configDirectory = Files.createTempDirectory("si-isolated")
    val root            = SingleInstance.forConfigDirectory(configDirectory, logger).isolatedSessionRoot.unsafeRunSync()

    root.startsWith(configDirectory) shouldBe false
    Files.isDirectory(root) shouldBe true
  }
