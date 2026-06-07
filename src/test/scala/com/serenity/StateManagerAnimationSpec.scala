package com.serenity

import java.awt.Color

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager

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
      val bufferWithAnimation = state.buffers.values.headOption
        .map { buffer =>
          val animations = buffer.animations.addCharacterAnimation(
            'a',
            0,
            0,
            Color.BLACK,
            Color.WHITE,
            5
          )
          buffer.copy(animations = animations)
        }
        .getOrElse {
          // Create a buffer with animation if none exists
          val buffer = com.serenity.state.models.Buffer
            .newEmpty(state.nextBufferId)
            .copy(
              animations = com.serenity.animation.AnimationState.empty.addCharacterAnimation(
                'a',
                0,
                0,
                Color.BLACK,
                Color.WHITE,
                5
              )
            )
          buffer
        }
      val (bufferId, updatedBuffer) =
        if state.buffers.nonEmpty then
          val (id, _) = state.buffers.head
          (id, bufferWithAnimation)
        else (state.nextBufferId, bufferWithAnimation)

      state.copy(buffers = state.buffers + (bufferId -> updatedBuffer))
    }.unsafeRunSync()

    val result = sm.advanceAnimationsOnTick().unsafeRunSync()
    result shouldBe true
  }

  it should "return false when the final animation step completes" in {
    val sm = makeStateManager()
    sm.updateState { state =>
      val bufferWithAnimation = state.buffers.values.headOption
        .map { buffer =>
          val animations = buffer.animations.addCharacterAnimation(
            'a',
            0,
            0,
            Color.BLACK,
            Color.WHITE,
            1
          )
          buffer.copy(animations = animations)
        }
        .getOrElse {
          // Create a buffer with animation if none exists
          val buffer = com.serenity.state.models.Buffer
            .newEmpty(state.nextBufferId)
            .copy(
              animations = com.serenity.animation.AnimationState.empty.addCharacterAnimation(
                'a',
                0,
                0,
                Color.BLACK,
                Color.WHITE,
                1
              )
            )
          buffer
        }
      val (bufferId, updatedBuffer) =
        if state.buffers.nonEmpty then
          val (id, _) = state.buffers.head
          (id, bufferWithAnimation)
        else (state.nextBufferId, bufferWithAnimation)

      state.copy(buffers = state.buffers + (bufferId -> updatedBuffer))
    }.unsafeRunSync()

    val result = sm.advanceAnimationsOnTick().unsafeRunSync()
    result shouldBe false
  }

  it should "not modify animation state when called with no active animations" in {
    val sm                = makeStateManager()
    val bufferAnimsBefore = sm.getCurrentState.unsafeRunSync().buffers.view.mapValues(_.animations.animations).toMap
    sm.advanceAnimationsOnTick().unsafeRunSync()
    val bufferAnimsAfter = sm.getCurrentState.unsafeRunSync().buffers.view.mapValues(_.animations.animations).toMap
    bufferAnimsAfter shouldBe bufferAnimsBefore
  }
