package com.serenity

import com.serenity.app.AppRuntimeRenderLoops
import com.serenity.config.{AppConfig, StatusLinePlacement, StatusSegment}
import com.serenity.rope.Balance
import com.serenity.state.models.AppState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Typing activity only hides the floating status row, so it should cost full frames only while that row is in use. */
class AppRuntimeTypingRenderSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def typingWith(config: AppConfig): AppState =
    val state = AppState.initial
    state.copy(
      persisted = state.persisted.copy(config = config),
      runtime = state.runtime.copy(typingActivity = state.runtime.typingActivity.observed(0L))
    )

  private val floating = AppConfig.default
    .withStatusLineSegments(List(StatusSegment.Position))
    .withStatusLinePlacement(StatusLinePlacement.Floating)

  "needsFullContentRender" should "ignore typing activity when the status line is not floating" in {
    val pinned = typingWith(AppConfig.default.withStatusLinePlacement(StatusLinePlacement.Pinned))
    val hidden = typingWith(AppConfig.default.withStatusLinePlacement(StatusLinePlacement.Off))

    AppRuntimeRenderLoops.needsFullContentRender(pinned, Map.empty) shouldBe false
    AppRuntimeRenderLoops.hasActiveAnimations(pinned, Map.empty) shouldBe false
    AppRuntimeRenderLoops.needsFullContentRender(hidden, Map.empty) shouldBe false
  }

  it should "count typing activity while it holds the floating status line hidden" in {
    val typing = typingWith(floating)

    AppRuntimeRenderLoops.needsFullContentRender(typing, Map.empty) shouldBe true
    AppRuntimeRenderLoops.hasActiveAnimations(typing, Map.empty) shouldBe true
  }

  it should "not count a floating status line with no typing burst in progress" in {
    val settled = typingWith(floating)
    val idle = settled.copy(runtime =
      settled.runtime.copy(typingActivity = settled.runtime.typingActivity.advance(Long.MaxValue))
    )

    AppRuntimeRenderLoops.needsFullContentRender(idle, Map.empty) shouldBe false
  }
