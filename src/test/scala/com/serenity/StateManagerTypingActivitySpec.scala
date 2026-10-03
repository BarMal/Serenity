package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** A typed character opens the typing quiet window (`runtime.typingActivity`) that holds the floating status row out of
  * the caret's way. It is recorded by the keystroke's own commit, not a separate one ahead of it.
  */
class StateManagerTypingActivitySpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def stateManager: StateManager =
    StateManager(
      LoggerFactory[IO].getLogger(using LoggerName("StateManagerTypingActivitySpec")),
      initialConfig = AppConfig.default
    )
      .unsafeRunSync()

  "StateManager" should "mark typing activity when a typed character is applied" in {
    val sm = stateManager

    sm.applyEvent(InsertChar('a')).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().runtime.typingActivity.isActive shouldBe true
  }

  it should "leave typing activity idle for events that type nothing" in {
    val sm = stateManager

    sm.applyEvent(MoveRight).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().runtime.typingActivity.isActive shouldBe false
  }
