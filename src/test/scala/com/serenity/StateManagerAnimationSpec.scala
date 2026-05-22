package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.googlecode.lanterna.TextColor
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerAnimationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager.apply(logger).unsafeRunSync()

  "advanceAnimationsOnTick" should "return false immediately when no animations are active" in {
    val sm     = makeStateManager()
    val result = sm.advanceAnimationsOnTick().unsafeRunSync()
    result shouldBe false
  }

  it should "return true while animations are still in flight" in {
    val sm = makeStateManager()
    sm.updateState { state =>
      state.copy(screenAnimations =
        state.screenAnimations.addCharacterAnimation(
          'a', 0, 0, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 5
        )
      )
    }.unsafeRunSync()

    val result = sm.advanceAnimationsOnTick().unsafeRunSync()
    result shouldBe true
  }

  it should "return false when the final animation step completes" in {
    val sm = makeStateManager()
    sm.updateState { state =>
      state.copy(screenAnimations =
        state.screenAnimations.addCharacterAnimation(
          'a', 0, 0, TextColor.ANSI.BLACK, TextColor.ANSI.WHITE, 1
        )
      )
    }.unsafeRunSync()

    val result = sm.advanceAnimationsOnTick().unsafeRunSync()
    result shouldBe false
  }

  it should "not modify animation state when called with no active animations" in {
    val sm          = makeStateManager()
    val animsBefore = sm.getCurrentState.unsafeRunSync().screenAnimations.animations
    sm.advanceAnimationsOnTick().unsafeRunSync()
    val animsAfter = sm.getCurrentState.unsafeRunSync().screenAnimations.animations
    animsAfter shouldBe animsBefore
  }
