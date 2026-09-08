package com.serenity

import scala.concurrent.duration.*

import cats.effect.IO
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.manager.StateManager
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class IntervalSaveStreamSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  behavior of "StateManager.intervalSaveStream"

  it should "be empty when no saveInterval is configured" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      emitted      <- stateManager.runtimeLifecycle.intervalSaveStream.take(1).compile.toList
    yield emitted shouldBe List.empty

    runVirtual(program)
  }

  it should "emit on the configured interval when saveInterval is set" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val policy = SessionManager.SessionPolicy(saveInterval = Some(20.millis))

    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger, policy)
      _            <- stateManager.runtimeLifecycle.intervalSaveStream.take(2).compile.drain
    yield succeed

    runVirtual(program)
  }
