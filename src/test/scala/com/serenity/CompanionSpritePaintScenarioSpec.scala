package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, VisualFlairLevel}
import com.serenity.rope.Balance
import com.serenity.state.models.SurfaceId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** End-to-end GUI coverage: a real `StateManager` + `Java2DRenderSurface` render pass (the same `UiScenarioDriver`
  * harness `MotionUiScenarioSpec` and friends use), not just the frame-slicing/tick-state unit tests -- proving the
  * companion pane's paint step actually reaches `surface.pixels.drawImage`.
  */
class CompanionSpritePaintScenarioSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private def enabledConfig: AppConfig =
    AppConfig.default.withCompanionSpriteConfig(AppConfig.default.companionSpriteConfig.copy(enabled = true))

  "the companion sprite pane" should "draw its current frame through surface.pixels.drawImage when enabled" in {
    val driver = UiScenarioDriver.create("companion-sprite-gui", initialConfig = enabledConfig).unsafeRunSync()

    val frame = driver.renderFrame("enabled").unsafeRunSync()

    frame.evidence.surfaceRects.keySet should contain(SurfaceId.CompanionSprite)
    frame.evidence.drawnImageRects should not be empty
  }

  it should "not draw anything when disabled" in {
    val driver = UiScenarioDriver.create("companion-sprite-gui-disabled", initialConfig = AppConfig.default).unsafeRunSync()

    val frame = driver.renderFrame("disabled").unsafeRunSync()

    frame.evidence.surfaceRects.keySet should not contain SurfaceId.CompanionSprite
  }

  it should "not draw the sprite image when visual flair is Off, even while enabled" in {
    val config = enabledConfig.withVisualFlairLevel(VisualFlairLevel.Off)
    val driver = UiScenarioDriver.create("companion-sprite-gui-flair-off", initialConfig = config).unsafeRunSync()

    val frame = driver.renderFrame("flair-off").unsafeRunSync()

    // Off removes the panel surface entirely (StateManagerEffectHandlers.syncCompanionSpritePanel's rule, applied at
    // session start by AppState.initial too), so there is no pane at all to draw an image into.
    frame.evidence.surfaceRects.keySet should not contain SurfaceId.CompanionSprite
  }
