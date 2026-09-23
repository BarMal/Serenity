package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.AppRuntimeRenderLoops
import com.serenity.config.*
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** The companion sprite panel's typing-reactivity, absorbed from the retired `com.serenity.animation.WindowSitter`
  * (issue #934 v2) -- see `CompanionSpriteTypingSpec` for the pure logic this exercises end-to-end through
  * `AppRuntime`.
  */
class AppRuntimeCompanionSpriteTypingSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val enabledConfig =
    AppConfig.default.withCompanionSpriteConfig(AppConfig.default.companionSpriteConfig.copy(enabled = true))

  "AppRuntime" should "wake the companion sprite from a real typing and tick sequence" in {
    val logger       = LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeCompanionSpriteTypingSpec"))
    val stateManager = StateManager(logger, initialConfig = enabledConfig).unsafeRunSync()

    AppRuntimeRenderLoops
      .observeCompanionSpriteTyping(InsertChar('a'), stateManager)
      .unsafeRunSync()

    val awakened = stateManager.getCurrentState.unsafeRunSync().runtime.companionSprite
    awakened.isTypingActive shouldBe true
    awakened.action shouldBe com.serenity.animation.sprite.CompanionSpriteAction.Walk

    // The companion sprite ticks continuously while enabled (its own idle-roll trick animation, unrelated to
    // typing), so unlike the retired window sitter's own finite activity window, the tick loop here never reports
    // "nothing left to animate" on its own -- advance a fixed number of ticks, comfortably past the default
    // `typingActiveTicks`, instead of driving the loop to exhaustion.
    (1 to 20).foreach(_ => stateManager.animationTicker.advanceAnimationsOnTick.unsafeRunSync())

    val settled = stateManager.getCurrentState.unsafeRunSync().runtime.companionSprite
    settled.isTypingActive shouldBe false
  }

  it should "not react to typing when the companion sprite is disabled" in {
    val logger       = LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeCompanionSpriteTypingSpec"))
    val stateManager = StateManager(logger, initialConfig = AppConfig.default).unsafeRunSync()

    AppRuntimeRenderLoops
      .observeCompanionSpriteTyping(InsertChar('a'), stateManager)
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().runtime.companionSprite.isTypingActive shouldBe false
  }
