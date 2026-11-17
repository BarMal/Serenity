package com.serenity.app

import scala.concurrent.duration.*

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.app.PixelClock.{Region, Screen}
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PixelClockSpec extends AnyFlatSpec with Matchers:

  private val region = Region(10, 20, 4, 2)
  private val blank  = Vector.fill(8)(0xff000000)
  private val inked  = Vector.fill(8)(0xffffffff)

  /** A screen that takes `cost` to grab and shows what `ref` holds when the grab finishes. */
  private def fakeScreen(
    cost: FiniteDuration,
    ref: Ref[IO, Vector[Int]],
    grabs: Ref[IO, List[Region]],
    events: Option[Ref[IO, List[String]]] = None
  ): Screen =
    grabbed => grabs.update(grabbed :: _) *> events.traverse_(_.update("grab" :: _)) *> IO.sleep(cost) *> ref.get

  private def measure(
    cost: FiniteDuration,
    changeAfter: Option[FiniteDuration],
    timeout: FiniteDuration = 200.millis
  ): (Option[Double], List[String], Int) =
    runVirtual {
      for
        ref    <- Ref.of[IO, Vector[Int]](blank)
        grabs  <- Ref.of[IO, List[Region]](Nil)
        events <- Ref.of[IO, List[String]](Nil)
        screen = fakeScreen(cost, ref, grabs, Some(events))
        trigger = events.update("trigger" :: _) *> changeAfter.traverse_(delay =>
          (IO.sleep(delay) *> ref.set(inked)).start
        )
        result <- PixelClock.timeChange(screen, region, trigger, timeout)
        log    <- events.get
        count  <- grabs.get.map(_.size)
      yield (result, log, count)
    }

  "PixelClock.timeChange" should "answer the time of the first grab that shows the change, which is the clock's own resolution late at worst" in {
    val (elapsed, _, _) = measure(cost = 5.millis, changeAfter = Some(12.millis))
    elapsed shouldBe Some(15.0)
  }

  it should "answer within one grab of a change that happened right away" in {
    val (elapsed, _, _) = measure(cost = 4.millis, changeAfter = Some(Duration.Zero))
    elapsed shouldBe Some(4.0)
  }

  it should "answer none when the region never changes before the timeout" in {
    val (elapsed, _, grabs) = measure(cost = 5.millis, changeAfter = None, timeout = 100.millis)
    elapsed shouldBe None
    grabs should be > 5
  }

  it should "take its baseline before it runs the trigger, so the trigger's own effect is what it sees" in {
    val (_, log, _) = measure(cost = 5.millis, changeAfter = Some(1.millis))
    log.reverse.take(2) shouldBe List("grab", "trigger")
  }

  it should "grab only the region it was asked about" in {
    val seen = runVirtual {
      for
        ref   <- Ref.of[IO, Vector[Int]](blank)
        grabs <- Ref.of[IO, List[Region]](Nil)
        screen = fakeScreen(1.milli, ref, grabs)
        _   <- PixelClock.timeChange(screen, region, ref.set(inked), 50.millis)
        all <- grabs.get
      yield all.distinct
    }
    seen shouldBe List(region)
  }

  it should "fail, not answer none, when the screen cannot be read" in {
    val broken: Screen = _ => IO.raiseError(new IllegalStateException("no display"))
    val outcome        = runVirtual(PixelClock.timeChange(broken, region, IO.unit, 50.millis).attempt)
    outcome.left.map(_.getMessage) shouldBe Left("no display")
  }

  "PixelClock.resolution" should "be the quickest grab, which is the least the clock can be late by" in {
    val costs           = List(8.millis, 5.millis, 9.millis).iterator
    val varying: Screen = _ => IO.sleep(costs.next()).as(blank)
    runVirtual(PixelClock.resolution(varying, region, samples = 3)) shouldBe 5.0
  }

  it should "take the stated number of grabs" in {
    val count = runVirtual {
      for
        grabs <- Ref.of[IO, Int](0)
        screen: Screen = _ => grabs.update(_ + 1) *> IO.sleep(2.millis).as(blank)
        _ <- PixelClock.resolution(screen, region, samples = 50)
        n <- grabs.get
      yield n
    }
    count shouldBe 50
  }

  it should "answer none, without grabbing again, when the trigger itself does not finish within the timeout" in {
    val (elapsed, grabbed) = runVirtual {
      for
        ref   <- Ref.of[IO, Vector[Int]](blank)
        grabs <- Ref.of[IO, List[Region]](Nil)
        screen = fakeScreen(5.millis, ref, grabs)
        result <- PixelClock.timeChange(screen, region, IO.never, 200.millis)
        count  <- grabs.get.map(_.size)
      yield (result, count)
    }
    elapsed shouldBe None
    grabbed shouldBe 1
  }

  "PixelClock.quiet" should "answer true once the regions have stayed the same for the quiet span" in {
    val (quiet, grabs) = runVirtual {
      for
        ref   <- Ref.of[IO, Vector[Int]](blank)
        grabs <- Ref.of[IO, List[Region]](Nil)
        screen = fakeScreen(1.millis, ref, grabs)
        _      <- (IO.sleep(120.millis) *> ref.set(inked)).start
        result <- PixelClock.quiet(screen, List(region), quietFor = 300.millis, poll = 50.millis, cap = 5.seconds)
        count  <- grabs.get.map(_.size)
      yield (result, count)
    }
    quiet shouldBe true
    grabs should be > 6
  }

  it should "not call a screen quiet that keeps changing, and give up at the cap instead of waiting for ever" in {
    val (quiet, elapsed) = runVirtual {
      for
        ref   <- Ref.of[IO, Vector[Int]](blank)
        grabs <- Ref.of[IO, List[Region]](Nil)
        screen = fakeScreen(1.millis, ref, grabs)
        flicker <- (IO.sleep(20.millis) *> ref.update(pixels =>
          if pixels == blank then inked else blank
        )).foreverM.start
        started <- IO.monotonic
        result  <- PixelClock.quiet(screen, List(region), quietFor = 300.millis, poll = 50.millis, cap = 2.seconds)
        ended   <- IO.monotonic
        _       <- flicker.cancel
      yield (result, ended - started)
    }
    quiet shouldBe false
    elapsed should (be >= 2.seconds and be < 3.seconds)
  }

  it should "answer true at once, after the quiet span, for a screen that never changes" in {
    val elapsed = runVirtual {
      for
        ref     <- Ref.of[IO, Vector[Int]](blank)
        grabs   <- Ref.of[IO, List[Region]](Nil)
        started <- IO.monotonic
        result  <- PixelClock.quiet(fakeScreen(1.millis, ref, grabs), List(region), 300.millis, 50.millis, 5.seconds)
        ended   <- IO.monotonic
      yield (result, ended - started)
    }
    elapsed._1 shouldBe true
    elapsed._2 should (be >= 300.millis and be < 500.millis)
  }
