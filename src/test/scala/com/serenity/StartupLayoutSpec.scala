package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.command.{CommandIntent, CommandRegistry, SessionIntent}
import com.serenity.config.AppConfig
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StartupLayoutSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def startedWith(name: String, config: AppConfig): AppState =
    val stateManager = createStateManager(name)
    AppStartup
      .initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        ViewportSize(80, 24),
        appConfig = config
      )
      .unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync()

  "Launching with no path in the minimal layout" should "open straight into an empty document" in {
    val state = startedWith("StartupLayoutSpec-minimal", AppConfig.default)

    state.startPageSurface shouldBe None
    state.persisted.focus shouldBe a[Focus.EditorPane]
    state.persisted.buffers should have size 1
    state.activeBuffer.map(_.document.content.toString) shouldBe Some("")
    state.persisted.config.uiLayout shouldBe com.serenity.config.UiLayout.Minimal
    state.runtime.viewportSize shouldBe Some(ViewportSize(80, 24))
  }

  it should "still tell the user about a config problem, in a prompt, since there is no start page" in {
    val stateManager = createStateManager("StartupLayoutSpec-notice")
    AppStartup
      .initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        ViewportSize(80, 24),
        appConfig = AppConfig.default,
        configNotice = Some("ui.layout is not a layout")
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().runtime.modalStack should not be empty
  }

  "Launching with no path in the conventional layout" should "show the start page" in {
    val state = startedWith("StartupLayoutSpec-conventional", AppConfig.conventional)

    state.startPageSurface shouldBe defined
    state.persisted.focus shouldBe a[Focus.Surface]
  }

  "Return to start page" should "stay a command in both layouts" in {
    val ids = CommandRegistry.withToggleUI.getAllCommands.map(_.intent)

    ids should contain(CommandIntent.Session(SessionIntent.ReturnToStartPage))
  }
