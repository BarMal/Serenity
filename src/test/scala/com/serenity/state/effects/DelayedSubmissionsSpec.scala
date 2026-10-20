package com.serenity.state.effects

import scala.concurrent.duration.*

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DelayedSubmissionsSpec extends AnyFlatSpec with Matchers:

  private val analysis: Lane.Keyed = Lane.Keyed(LaneKey.Analysis, LanePolicy.SwitchLatest)
  private val search: Lane.Keyed   = Lane.Keyed(LaneKey.Search, LanePolicy.SwitchLatest)

  private def withLanes[A](program: (EffectLanes, Ref[IO, Vector[(String, FiniteDuration)]]) => IO[A]): A =
    runVirtual(
      Ref.of[IO, Vector[(String, FiniteDuration)]](Vector.empty).flatMap { ran =>
        EffectLanes.resource((_, _) => IO.unit).use(program(_, ran))
      }
    )

  private def record(ran: Ref[IO, Vector[(String, FiniteDuration)]], label: String): IO[Unit] =
    IO.monotonic.flatMap(at => ran.update(_ :+ (label -> at)))

  "submitAfter" should "run a burst of requests once, a delay after the last" in {
    val ran = withLanes { (lanes, ran) =>
      (1 to 5000).toList.traverse_(index =>
        lanes.submitAfter(analysis, 150.millis, record(ran, s"run $index")) >> IO.sleep(1.milli)
      ) >> lanes.drain >> ran.get
    }
    ran shouldBe Vector("run 5000" -> (4999.millis + 150.millis))
  }

  it should "run a burst that never goes quiet once maxWait has passed since its first request" in {
    val ran = withLanes { (lanes, ran) =>
      (1 to 30).toList.traverse_(index =>
        lanes.submitAfter(analysis, 150.millis, record(ran, s"run $index"), Some(1.second)) >> IO.sleep(100.millis)
      ) >> lanes.drain >> ran.get
    }
    ran.map(_._2).headOption shouldBe Some(1.second)
    ran.size should be > 1
  }

  it should "not run anything while no request has been made, however long the editor idles" in {
    val ran = withLanes((_, ran) => IO.sleep(10.minutes) >> ran.get)
    ran shouldBe empty
  }

  it should "run an earlier deadline on another lane while a later one is still waiting" in {
    val ran = withLanes { (lanes, ran) =>
      lanes.submitAfter(analysis, 150.millis, record(ran, "analysis")) >>
        IO.sleep(10.millis) >> lanes.submitAfter(search, 50.millis, record(ran, "search")) >> lanes.drain >> ran.get
    }
    ran shouldBe Vector("search" -> 60.millis, "analysis" -> 150.millis)
  }

  it should "start again for a request made after an earlier one has run" in {
    val ran = withLanes { (lanes, ran) =>
      lanes.submitAfter(search, 50.millis, record(ran, "first")) >> IO.sleep(5.minutes) >>
        lanes.submitAfter(search, 50.millis, record(ran, "second")) >> lanes.drain >> ran.get
    }
    ran.map(_._1) shouldBe Vector("first", "second")
  }

  it should "keep requests for different lanes apart" in {
    val ran = withLanes { (lanes, ran) =>
      lanes.submitAfter(analysis, 50.millis, record(ran, "analysis")) >>
        lanes.submitAfter(search, 50.millis, record(ran, "search")) >> lanes.drain >> ran.get
    }
    ran.map(_._1).toSet shouldBe Set("analysis", "search")
  }

  it should "let drain wait for a request still inside its delay" in {
    val (drainedAt, ran) = withLanes { (lanes, ran) =>
      lanes.submitAfter(search, 1.second, record(ran, "search")) >> lanes.drain >> (IO.monotonic, ran.get).tupled
    }
    drainedAt shouldBe 1.second
    ran.map(_._1) shouldBe Vector("search")
  }

  it should "drop a waiting request when the lanes are released" in {
    val ran = runVirtual(
      Ref.of[IO, Vector[(String, FiniteDuration)]](Vector.empty).flatMap { ran =>
        EffectLanes
          .resource((_, _) => IO.unit)
          .use(_.submitAfter(search, 1.second, record(ran, "search")))
          .productR(IO.sleep(5.seconds) >> ran.get)
      }
    )
    ran shouldBe empty
  }

  it should "be rejected once the lanes are released" in {
    val outcome = runVirtual(
      EffectLanes.resource((_, _) => IO.unit).use(IO.pure).flatMap(_.submitAfter(search, 1.second, IO.unit).attempt)
    )
    outcome.left.map(_.getClass) shouldBe Left(classOf[EffectLanes.Released])
  }
