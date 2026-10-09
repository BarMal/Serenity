package com.serenity.io

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The hand-off to the AppKit thread, with a thread of the test's own standing in for the main run loop. */
class MainThreadHandoffSpec extends AnyFlatSpec with Matchers:

  private def runLoopThread(work: Runnable): Unit =
    val thread = new Thread(work, "stand-in-main-run-loop")
    thread.start()

  private def counter: Ref[IO, Int] = Ref.unsafe[IO, Int](0)

  "The hand-off" should "run the body on the thread that services the schedule, and return its result" in {
    val cancelled = counter

    val result = MainThreadHandoff.run(
      schedule = work =>
        runLoopThread(work)
        () => cancelled.update(_ + 1).unsafeRunSync()
      ,
      startTimeout = 5.seconds
    )(Thread.currentThread().getName)

    result shouldBe Right("stand-in-main-run-loop")
    cancelled.get.unsafeRunSync() shouldBe 1
  }

  it should "give back the failure of a body that raised one" in {
    val failure = new IllegalStateException("panel exploded")

    val result = MainThreadHandoff.run(
      work =>
        runLoopThread(work); () => ()
      ,
      5.seconds
    )(throw failure)

    result shouldBe Left(failure)
  }

  it should "wait for a body as long as it takes once it has started, however short the start timeout" in {
    val result = MainThreadHandoff.run(
      work =>
        runLoopThread(work); () => ()
      ,
      50.millis
    ) {
      Thread.sleep(300)
      "chosen"
    }

    result shouldBe Right("chosen")
  }

  it should "give up if the main thread never starts the body, and cancel the schedule" in {
    val ran       = counter
    val cancelled = counter
    val queued    = Ref.unsafe[IO, Option[Runnable]](None)

    val result = MainThreadHandoff.run(
      schedule = work =>
        queued.set(Some(work)).unsafeRunSync()
        () => cancelled.update(_ + 1).unsafeRunSync()
      ,
      startTimeout = 100.millis
    )(ran.update(_ + 1).unsafeRunSync())

    result.left.map(_.getMessage.contains("main thread")) shouldBe Left(true)
    cancelled.get.unsafeRunSync() shouldBe 1

    queued.get.unsafeRunSync().foreach(_.run())
    ran.get.unsafeRunSync() shouldBe 0
  }
