package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.sprite.CompanionCharacter
import com.serenity.command.{Command, CommandCategory, CommandIntent, PanelChromeIntent, SettingsIntent}
import com.serenity.config.{AppConfig, VisualFlairLevel}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.{SurfaceContent, SurfaceId, SurfacePresentation}
import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Covers the companion-sprite-enabled toggle and visual-flair-level settings, following the `WindowSitterConfig`
  * end-to-end wiring template exactly: `PanelChromeIntent` -> effect handler -> persisted `AppConfig` field, plus the
  * pinned-panel surface the toggle adds/removes (unlike the window sitter, which has no surface of its own).
  */
class StateManagerCompanionSpriteSettingsSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createStateManager(initialConfig: AppConfig = AppConfig.default): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerCompanionSpriteSettingsSpec"))
    StateManager(logger, initialConfig = initialConfig).unsafeRunSync()

  private def execute(stateManager: StateManager, intent: PanelChromeIntent): Unit =
    stateManager
      .executeCommand(
        Command.typed(
          "companion-sprite-settings-spec",
          "Companion sprite setting",
          CommandIntent.Settings(SettingsIntent.PanelChrome(intent)),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

  "PanelChromeIntent.SetCompanionSpriteEnabled" should "persist the toggle and add the companion pane surface when enabled" in {
    val stateManager = createStateManager()

    execute(stateManager, PanelChromeIntent.SetCompanionSpriteEnabled(true))

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.config.companionSpriteConfig.enabled shouldBe true
    val surface = state.runtime.uiSurfaces.find(_.id == SurfaceId.CompanionSprite)
    surface shouldBe defined
    surface.get.content shouldBe SurfaceContent.CompanionSprite
    surface.get.presentation shouldBe a[SurfacePresentation.Pinned]
  }

  it should "remove the companion pane surface when disabled again" in {
    val stateManager = createStateManager()
    execute(stateManager, PanelChromeIntent.SetCompanionSpriteEnabled(true))

    execute(stateManager, PanelChromeIntent.SetCompanionSpriteEnabled(false))

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.config.companionSpriteConfig.enabled shouldBe false
    state.runtime.uiSurfaces.exists(_.id == SurfaceId.CompanionSprite) shouldBe false
  }

  it should "place the panel at the configured position and size" in {
    val stateManager = createStateManager(
      AppConfig.default.withCompanionSpriteConfig(
        AppConfig.default.companionSpriteConfig.copy(position = PanelPosition.Bottom, size = 12)
      )
    )

    execute(stateManager, PanelChromeIntent.SetCompanionSpriteEnabled(true))

    val state   = stateManager.getCurrentState.unsafeRunSync()
    val surface = state.runtime.uiSurfaces.find(_.id == SurfaceId.CompanionSprite).get
    surface.presentation shouldBe SurfacePresentation.Pinned(PanelPosition.Bottom, 12)
  }

  "PanelChromeIntent.SetVisualFlairLevel" should "persist the level" in {
    val stateManager = createStateManager()

    execute(stateManager, PanelChromeIntent.SetVisualFlairLevel(VisualFlairLevel.Reduced))

    stateManager.getCurrentState.unsafeRunSync().persisted.config.visualFlairLevel shouldBe VisualFlairLevel.Reduced
  }

  it should "remove the companion pane when flair drops to Off, even while the sprite is still enabled" in {
    val stateManager = createStateManager()
    execute(stateManager, PanelChromeIntent.SetCompanionSpriteEnabled(true))
    stateManager.getCurrentState.unsafeRunSync().runtime.uiSurfaces.exists(_.id == SurfaceId.CompanionSprite) shouldBe true

    execute(stateManager, PanelChromeIntent.SetVisualFlairLevel(VisualFlairLevel.Off))

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.config.companionSpriteConfig.enabled shouldBe true
    state.runtime.uiSurfaces.exists(_.id == SurfaceId.CompanionSprite) shouldBe false
  }

  it should "restore the companion pane when flair returns from Off, without needing the toggle touched again" in {
    val stateManager = createStateManager()
    execute(stateManager, PanelChromeIntent.SetCompanionSpriteEnabled(true))
    execute(stateManager, PanelChromeIntent.SetVisualFlairLevel(VisualFlairLevel.Off))

    execute(stateManager, PanelChromeIntent.SetVisualFlairLevel(VisualFlairLevel.Full))

    stateManager.getCurrentState.unsafeRunSync().runtime.uiSurfaces.exists(_.id == SurfaceId.CompanionSprite) shouldBe true
  }

  "CompanionSpriteConfig.default" should "name the bundled placeholder character" in {
    AppConfig.default.companionSpriteConfig.character shouldBe CompanionCharacter.PixelWizard
  }
