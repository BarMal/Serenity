package com.serenity.state.effects

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.state.models.BufferId
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class EffectLanesSpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 60)

  final private case class Mark(label: String, at: FiniteDuration)

  private class Timeline(marks: Ref[IO, Vector[Mark]]):
    def mark(label: String): IO[Unit] = IO.monotonic.flatMap(at => marks.update(_ :+ Mark(label, at)))

    def job(name: String, duration: FiniteDuration): IO[Unit] =
      (mark(s"$name start") >> IO.sleep(duration) >> mark(s"$name end")).onCancel(mark(s"$name cancelled"))

    def failing(name: String, error: Throwable): IO[Unit] = mark(s"$name start") >> IO.raiseError(error)

    def labels: IO[Vector[String]] = marks.get.map(_.map(_.label))

    def at(label: String): IO[Option[FiniteDuration]] = marks.get.map(_.find(_.label == label).map(_.at))

  private object Timeline:
    def create: IO[Timeline] = Ref.of[IO, Vector[Mark]](Vector.empty).map(Timeline(_))

  private val ignoreFailures: (Lane.Scheduled, Throwable) => IO[Unit] = (_, _) => IO.unit

  private def withLanes[A](program: (EffectLanes, Timeline) => IO[A]): A =
    runVirtual(Timeline.create.flatMap(timeline => EffectLanes.resource(ignoreFailures).use(program(_, timeline))))

  private def sequential(key: LaneKey): Lane.Keyed   = Lane.Keyed(key, LanePolicy.Sequential)
  private def switchLatest(key: LaneKey): Lane.Keyed = Lane.Keyed(key, LanePolicy.SwitchLatest)
  private def dropIfBusy(key: LaneKey): Lane.Keyed   = Lane.Keyed(key, LanePolicy.DropIfBusy)

  private val fileA = LaneKey.File(Path.of("/project/a.scala"))
  private val fileB = LaneKey.File(Path.of("/project/b.scala"))

  "A Sequential lane" should "run jobs on one key strictly FIFO without overlapping" in {
    val (labels, finished) = withLanes { (lanes, timeline) =>
      List("first", "second", "third").traverse_(name =>
        lanes.submit(sequential(fileA), timeline.job(name, 1.second))
      ) >>
        lanes.drain >> (timeline.labels, IO.monotonic).tupled
    }
    labels shouldBe Vector("first start", "first end", "second start", "second end", "third start", "third end")
    finished shouldBe 3.seconds
  }

  it should "run jobs on different keys in parallel" in {
    val (startB, finished) = withLanes { (lanes, timeline) =>
      lanes.submit(sequential(fileA), timeline.job("a", 2.seconds)) >>
        lanes.submit(sequential(fileB), timeline.job("b", 2.seconds)) >>
        lanes.drain >> (timeline.at("b start"), IO.monotonic).tupled
    }
    startB shouldBe Some(Duration.Zero)
    finished shouldBe 2.seconds
  }

  it should "return from submit without waiting for the job" in {
    val elapsed = withLanes { (lanes, timeline) =>
      IO.monotonic.flatMap(before =>
        lanes.submit(sequential(fileA), timeline.job("slow", 10.seconds)) >> IO.monotonic.map(_ - before)
      )
    }
    elapsed shouldBe Duration.Zero
  }

  "The same key under two policies" should "be two independent lanes" in {
    val starts = withLanes { (lanes, timeline) =>
      lanes.submit(sequential(LaneKey.Search), timeline.job("sequential", 2.seconds)) >>
        lanes.submit(switchLatest(LaneKey.Search), timeline.job("switching", 2.seconds)) >>
        lanes.drain >> (timeline.at("sequential start"), timeline.at("switching start")).tupled
    }
    starts shouldBe (Some(Duration.Zero), Some(Duration.Zero))
  }

  "A SwitchLatest lane" should "cancel the running job, run its finaliser, then run the new one" in {
    val (labels, startOfNew) = withLanes { (lanes, timeline) =>
      lanes.submit(switchLatest(LaneKey.Analysis), timeline.job("old", 10.seconds)) >> IO.sleep(1.second) >>
        lanes.submit(switchLatest(LaneKey.Analysis), timeline.job("new", 1.second)) >>
        lanes.drain >> (timeline.labels, timeline.at("new start")).tupled
    }
    labels shouldBe Vector("old start", "old cancelled", "new start", "new end")
    startOfNew shouldBe Some(1.second)
  }

  it should "only complete the last of several rapid submissions" in {
    val labels = withLanes { (lanes, timeline) =>
      List("a", "b", "c").traverse_(name => lanes.submit(switchLatest(LaneKey.Search), timeline.job(name, 1.second))) >>
        lanes.drain >> timeline.labels
    }
    labels.filter(_.endsWith("end")) shouldBe Vector("c end")
    labels should not contain "b start"
  }

  "A DropIfBusy lane" should "ignore a submission while busy and accept one once idle" in {
    val labels = withLanes { (lanes, timeline) =>
      lanes.submit(dropIfBusy(LaneKey.Dialog), timeline.job("open", 2.seconds)) >> IO.sleep(1.second) >>
        lanes.submit(dropIfBusy(LaneKey.Dialog), timeline.job("ignored", 1.second)) >> IO.sleep(2.seconds) >>
        lanes.submit(dropIfBusy(LaneKey.Dialog), timeline.job("later", 1.second)) >> lanes.drain >> timeline.labels
    }
    labels shouldBe Vector("open start", "open end", "later start", "later end")
  }

  "An Exclusive job" should
    "wait for Sequential work, cancel switching work, run alone, and hold later submissions" in {
      val (labels, exclusiveStart, heldStart) = withLanes { (lanes, timeline) =>
        lanes.submit(sequential(fileA), timeline.job("save 1", 2.seconds)) >>
          lanes.submit(sequential(fileA), timeline.job("save 2", 1.second)) >>
          lanes.submit(switchLatest(LaneKey.Search), timeline.job("search", 10.seconds)) >>
          lanes.submit(dropIfBusy(LaneKey.Dialog), timeline.job("dialog", 10.seconds)) >> IO.sleep(1.second) >>
          lanes.submit(Lane.Exclusive, timeline.job("quit", 1.second)) >>
          lanes.submit(sequential(fileB), timeline.job("held", 1.second)) >>
          lanes.drain >> (timeline.labels, timeline.at("quit start"), timeline.at("held start")).tupled
      }
      labels should contain allOf ("search cancelled", "dialog cancelled")
      labels should not contain "search end"
      labels.indexOf("save 2 end") should be < labels.indexOf("quit start")
      labels.indexOf("quit end") should be < labels.indexOf("held start")
      exclusiveStart shouldBe Some(3.seconds)
      heldStart shouldBe Some(4.seconds)
    }

  it should "be accepted without waiting to be admitted" in {
    val elapsed = withLanes { (lanes, timeline) =>
      lanes.submit(sequential(fileA), timeline.job("save", 5.seconds)) >>
        IO.monotonic.flatMap(before =>
          lanes.submit(Lane.Exclusive, timeline.job("quit", 1.second)) >> IO.monotonic.map(_ - before)
        )
    }
    elapsed shouldBe Duration.Zero
  }

  it should "run back-to-back Exclusive jobs one after the other" in {
    val labels = withLanes { (lanes, timeline) =>
      lanes.submit(Lane.Exclusive, timeline.job("restore", 1.second)) >>
        lanes.submit(sequential(fileA), timeline.job("between", 1.second)) >>
        lanes.submit(Lane.Exclusive, timeline.job("quit", 1.second)) >> lanes.drain >> timeline.labels
    }
    labels shouldBe Vector("restore start", "restore end", "between start", "between end", "quit start", "quit end")
  }

  "A failing job" should "be reported with its lane while the lane keeps working" in {
    val boom = new RuntimeException("boom")
    val (failures, labels) = runVirtual(
      for
        timeline <- Timeline.create
        failures <- Ref.of[IO, Vector[(Lane, String)]](Vector.empty)
        onFailure = (lane: Lane.Scheduled, error: Throwable) => failures.update(_ :+ (lane, error.getMessage))
        _ <- EffectLanes.resource(onFailure).use { lanes =>
          lanes.submit(sequential(fileA), timeline.failing("broken", boom)) >>
            lanes.submit(sequential(fileA), timeline.job("next", 1.second)) >>
            lanes.submit(Lane.Exclusive, timeline.failing("broken quit", boom)) >>
            lanes.submit(sequential(fileB), timeline.job("after quit", 1.second)) >> lanes.drain
        }
        reported <- failures.get
        labels   <- timeline.labels
      yield (reported, labels)
    )
    failures shouldBe Vector(sequential(fileA) -> "boom", Lane.Exclusive -> "boom")
    labels should contain allOf ("next end", "after quit end")
  }

  "A job that cancels itself" should "not wedge its lane" in {
    val labels = withLanes { (lanes, timeline) =>
      lanes.submit(sequential(fileA), timeline.mark("gave up") >> IO.canceled) >>
        lanes.submit(sequential(fileA), timeline.job("next", 1.second)) >> lanes.drain >> timeline.labels
    }
    labels shouldBe Vector("gave up", "next start", "next end")
  }

  "drain" should "return immediately when nothing was accepted" in {
    withLanes((lanes, _) => lanes.drain >> IO.monotonic) shouldBe Duration.Zero
  }

  it should "wait for all accepted work across lanes" in {
    val finished = withLanes { (lanes, timeline) =>
      lanes.submit(sequential(fileA), timeline.job("a1", 1.second)) >>
        lanes.submit(sequential(fileA), timeline.job("a2", 3.seconds)) >>
        lanes.submit(switchLatest(LaneKey.Analysis), timeline.job("analysis", 5.seconds)) >>
        lanes.submit(dropIfBusy(LaneKey.Dialog), timeline.job("dialog", 2.seconds)) >> lanes.drain >> IO.monotonic
    }
    finished shouldBe 5.seconds
  }

  it should "not wait for work accepted after it was called" in {
    val drainedAt = withLanes { (lanes, timeline) =>
      for
        _         <- lanes.submit(sequential(fileA), timeline.job("early", 2.seconds))
        drained   <- (lanes.drain >> IO.monotonic).start
        _         <- IO.sleep(1.second)
        _         <- lanes.submit(sequential(fileB), timeline.job("late", 10.seconds))
        drainedAt <- drained.joinWithNever
      yield drainedAt
    }
    drainedAt shouldBe 2.seconds
  }

  "Releasing the resource" should "cancel in-flight work" in {
    val (labels, releasedAt) = runVirtual(
      for
        timeline <- Timeline.create
        _ <- EffectLanes
          .resource(ignoreFailures)
          .use(lanes =>
            lanes.submit(sequential(fileA), timeline.job("running", 10.seconds)) >>
              lanes.submit(sequential(fileA), timeline.job("queued", 1.second)) >> IO.sleep(1.second)
          )
        releasedAt <- IO.monotonic
        labels     <- timeline.labels
      yield (labels, releasedAt)
    )
    labels shouldBe Vector("running start", "running cancelled")
    releasedAt shouldBe 1.second
  }

  it should "reject submissions made afterwards" in {
    val outcome = runVirtual(
      EffectLanes.resource(ignoreFailures).use(IO.pure).flatMap(_.submit(sequential(fileA), IO.unit).attempt)
    )
    outcome.left.map(_.getClass) shouldBe Left(classOf[EffectLanes.Released])
  }

  final private case class Submission(key: Int, gap: FiniteDuration, duration: FiniteDuration)

  private val submissions: Gen[List[Submission]] =
    Gen.listOf(
      for
        key      <- Gen.choose(0, 2)
        gap      <- Gen.choose(0, 20).map(_.millis)
        duration <- Gen.choose(0, 50).map(_.millis)
      yield Submission(key, gap, duration)
    )

  "Sequential lanes" should "keep per-key FIFO without overlap under random interleavings" in
    forAll(submissions) { plan =>
      val marks = withLanes { (lanes, timeline) =>
        plan.zipWithIndex.traverse_ {
          case (submission, index) =>
            IO.sleep(submission.gap) >>
              lanes.submit(
                sequential(LaneKey.Buffer(BufferId(submission.key))),
                timeline.job(s"$index", submission.duration)
              )
        } >> lanes.drain >> timeline.labels
      }
      plan.indices.groupBy(plan(_).key).values.foreach { indicesOnKey =>
        val onKey = marks.filter(label => indicesOnKey.exists(index => label.startsWith(s"$index ")))
        onKey shouldBe indicesOnKey.sorted.flatMap(index => Vector(s"$index start", s"$index end"))
      }
    }
