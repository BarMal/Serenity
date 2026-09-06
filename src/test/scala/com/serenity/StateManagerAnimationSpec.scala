package com.serenity

import java.awt.Color

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, MotionPreset, VisualFlairLevel}
import com.serenity.keystroke.events.NextTab
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.ViewportSize
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
    val sm    = makeStateManager()
    val state = sm.getCurrentState.unsafeRunSync()
    val bufferId =
      if state.persisted.buffers.nonEmpty then state.persisted.buffers.head._1 else state.runtime.nextBufferId
    if state.persisted.buffers.isEmpty then
      sm.updateState { s =>
        val buffer = com.serenity.state.models.Buffer.newEmpty(s.runtime.nextBufferId)
        s.copy(persisted = s.persisted.copy(buffers = s.persisted.buffers + (bufferId -> buffer)))
      }.unsafeRunSync()

    val existingAnimations =
      sm.getBufferAnimations.unsafeRunSync().getOrElse(bufferId, com.serenity.animation.AnimationState.empty)
    val animations = existingAnimations.addCharacterAnimation(
      'a',
      0,
      0,
      Color.BLACK,
      Color.WHITE,
      5
    )
    sm.updateBufferAnimations(_.updated(bufferId, animations)).unsafeRunSync()

    val result = sm.advanceAnimationsOnTick().unsafeRunSync()
    result shouldBe true
  }

  it should "return false when the final animation step completes" in {
    val sm    = makeStateManager()
    val state = sm.getCurrentState.unsafeRunSync()
    val bufferId =
      if state.persisted.buffers.nonEmpty then state.persisted.buffers.head._1 else state.runtime.nextBufferId
    if state.persisted.buffers.isEmpty then
      sm.updateState { s =>
        val buffer = com.serenity.state.models.Buffer.newEmpty(s.runtime.nextBufferId)
        s.copy(persisted = s.persisted.copy(buffers = s.persisted.buffers + (bufferId -> buffer)))
      }.unsafeRunSync()

    val existingAnimations =
      sm.getBufferAnimations.unsafeRunSync().getOrElse(bufferId, com.serenity.animation.AnimationState.empty)
    val animations = existingAnimations.addCharacterAnimation(
      'a',
      0,
      0,
      Color.BLACK,
      Color.WHITE,
      1
    )
    sm.updateBufferAnimations(_.updated(bufferId, animations)).unsafeRunSync()

    val result = sm.advanceAnimationsOnTick().unsafeRunSync()
    result shouldBe false
  }

  it should "not modify animation state when called with no active animations" in {
    val sm                = makeStateManager()
    val bufferAnimsBefore = sm.getBufferAnimations.unsafeRunSync().view.mapValues(_.animations).toMap
    sm.advanceAnimationsOnTick().unsafeRunSync()
    val bufferAnimsAfter = sm.getBufferAnimations.unsafeRunSync().view.mapValues(_.animations).toMap
    bufferAnimsAfter shouldBe bufferAnimsBefore
  }

  it should "not copy inactive buffers while advancing another buffer animation" in {
    val sm               = makeStateManager()
    val inactiveBufferId = sm.createBuffer("inactive").unsafeRunSync()
    val activeBufferId   = sm.createBuffer("active").unsafeRunSync()

    sm.updateBufferAnimations { _ =>
      Map(
        activeBufferId -> com.serenity.animation.AnimationState.empty.addCharacterAnimation(
          'a',
          0,
          0,
          Color.BLACK,
          Color.WHITE,
          5
        )
      )
    }.unsafeRunSync()

    val before           = sm.getCurrentState.unsafeRunSync()
    val inactiveBefore   = before.persisted.buffers(inactiveBufferId)
    val animationsBefore = sm.getBufferAnimations.unsafeRunSync()

    sm.advanceAnimationsOnTick().unsafeRunSync()

    val after           = sm.getCurrentState.unsafeRunSync()
    val inactiveAfter   = after.persisted.buffers(inactiveBufferId)
    val animationsAfter = sm.getBufferAnimations.unsafeRunSync()

    inactiveAfter should be theSameInstanceAs inactiveBefore
    // The inactive buffer's entry in the side table must stay absent while another
    // buffer's animation is advanced: advanceAnimationsOnTick only touches the keys
    // already present in the map, so leaving it unseeded is the real assertion here.
    animationsBefore.get(inactiveBufferId) shouldBe None
    animationsAfter.get(inactiveBufferId) shouldBe None
  }

  it should "advance the companion sprite's frame on every tick while it is enabled" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val config = AppConfig.default.withCompanionSpriteConfig(
      AppConfig.default.companionSpriteConfig.copy(enabled = true)
    )
    val sm = StateManager.apply(logger, initialConfig = config).unsafeRunSync()

    val before = sm.getCurrentState.unsafeRunSync().runtime.companionSprite

    val stillActive = sm.advanceAnimationsOnTick().unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync().runtime.companionSprite
    stillActive shouldBe true
    after.ticksInAction should not be before.ticksInAction
  }

  it should "not advance the companion sprite while it is disabled" in {
    val sm     = makeStateManager()
    val before = sm.getCurrentState.unsafeRunSync().runtime.companionSprite

    sm.advanceAnimationsOnTick().unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().runtime.companionSprite shouldBe before
  }

  it should "not advance the companion sprite when visual flair is Off, even while enabled" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val config = AppConfig.default
      .withCompanionSpriteConfig(AppConfig.default.companionSpriteConfig.copy(enabled = true))
      .withVisualFlairLevel(VisualFlairLevel.Off)
    val sm = StateManager.apply(logger, initialConfig = config).unsafeRunSync()

    val before = sm.getCurrentState.unsafeRunSync().runtime.companionSprite
    sm.advanceAnimationsOnTick().unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().runtime.companionSprite shouldBe before
  }

  it should "scale pane flow animations with the global animation speed" in {
    val sm = makeStateManager()
    sm.updateState { state =>
      state.copy(
        persisted = state.persisted.copy(
          config = AppConfig.default
            .withMotionPreset(MotionPreset.Smooth)
            .withElementTransitionSpeedScale(2.0)
        ),
        runtime = state.runtime.copy(viewportSize = Some(ViewportSize(80, 24)))
      )
    }.unsafeRunSync()

    val firstBufferId = sm.getCurrentState.unsafeRunSync().persisted.bufferOrder.head
    sm.updateBuffer(firstBufferId, "First").unsafeRunSync()
    val secondBufferId = sm.createBuffer("Second").unsafeRunSync()

    sm.applyEvent(NextTab).unsafeRunSync()

    val state            = sm.getCurrentState.unsafeRunSync()
    val bufferAnimations = sm.getBufferAnimations.unsafeRunSync()
    state.focusedBufferId shouldBe Some(secondBufferId)
    val shortestFadeLength = bufferAnimations
      .getOrElse(secondBufferId, com.serenity.animation.AnimationState.empty)
      .animations
      .values
      .flatMap(_.foregroundAnimation.map(_.steps))
      .min
    shortestFadeLength shouldBe AppConfig.default
      .withMotionPreset(MotionPreset.Smooth)
      .editorConfig
      .characterAnimation
      .get
      .steps * 2
  }
