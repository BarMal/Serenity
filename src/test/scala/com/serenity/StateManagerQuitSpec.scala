package com.serenity

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Quitting drains Sequential lane work before the runtime tears down (#1697); with nothing pending that step must cost
  * nothing, so every runtime loop waiting on `awaitQuit` is released straight away.
  */
class StateManagerQuitSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  "Quitting" should "release awaitQuit well inside the persistence grace period when nothing is pending" in {
    // The first quit in a JVM pays class loading and cold-disk costs of the session save (2.7 s on a Windows runner,
    // 0.16 s for the identical quit once warm, #2016); a quit that really waited would take the full grace period every
    // time, so the measured quit runs warm.
    val warmUp = createStateManager("StateManagerQuitSpec-warm-up").runtimeLifecycle
    (warmUp.forceQuit >> warmUp.awaitQuit).timeout(30.seconds).unsafeRunSync()

    val stateManager = createStateManager("StateManagerQuitSpec")
    val lifecycle    = stateManager.runtimeLifecycle

    val elapsed =
      (IO.monotonic, lifecycle.forceQuit >> lifecycle.awaitQuit, IO.monotonic)
        .mapN((started, _, ended) => ended - started)
        .timeout(30.seconds)
        .unsafeRunSync()

    elapsed should be < 2.seconds
  }

  it should "let every concurrent awaitQuit waiter through once shutdown finishes" in {
    val stateManager = createStateManager("StateManagerQuitSpec-waiters")
    val lifecycle    = stateManager.runtimeLifecycle

    val released =
      (List.fill(8)(lifecycle.awaitQuit).parSequence_ &> lifecycle.forceQuit).as(true).timeout(30.seconds)

    released.unsafeRunSync() shouldBe true
  }
