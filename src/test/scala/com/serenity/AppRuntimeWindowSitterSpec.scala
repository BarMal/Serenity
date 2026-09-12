package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{WindowSitter, WindowSitterConfig}
import com.serenity.app.AppRuntime
import com.serenity.config.*
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.AppState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class AppRuntimeWindowSitterSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  "AppRuntime" should "keep fast rendering active while the window sitter is ticking" in {
    val state = AppState.initial.copy(runtime =
      AppState.initial.runtime.copy(windowSitter = WindowSitter.default.observeTyping(1_000_000_000L))
    )

    AppRuntime.hasActiveAnimations(state, Map.empty) shouldBe true
  }

  it should "initialize the window sitter from the configured startup frames" in {
    val config = AppConfig.default.withWindowSitterConfig(
      WindowSitterConfig(frames = Vector("rest", "active"), activeTicks = 3)
    )

    AppState.initial(config).runtime.windowSitter shouldBe WindowSitter.fromConfig(config.windowSitterConfig)
  }

  it should "wake and settle the window sitter through a real typing and tick sequence" in {
    val logger       = LoggerFactory[IO].getLogger(using LoggerName("AppRuntimeSpec"))
    val stateManager = StateManager(logger, initialConfig = AppConfig.default).unsafeRunSync()

    AppRuntime
      .observeWindowSitterTyping(InsertChar('a'), stateManager)
      .unsafeRunSync()

    val awakened = stateManager.getCurrentState.unsafeRunSync().runtime.windowSitter
    awakened.isActive shouldBe true
    awakened.glyph should not be WindowSitter.default.glyph

    Iterator
      .continually(stateManager.animationTicker.advanceAnimationsOnTick.unsafeRunSync())
      .takeWhile(identity)
      .toList

    stateManager.getCurrentState.unsafeRunSync().runtime.windowSitter.isActive shouldBe false
  }
