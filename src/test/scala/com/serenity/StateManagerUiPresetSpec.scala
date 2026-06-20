package com.serenity

import java.awt.Font
import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.{AppConfig, BackgroundStyle, PreferredWindowSize}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.{PanelContent, PanelPosition}
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerUiPresetSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def managerWithStore(
    store: UiPresetStore,
    windowSize: IO[Option[PreferredWindowSize]] = IO.pure(None),
    onWindowSizeChanged: PreferredWindowSize => IO[Unit] = _ => IO.unit
  ): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerUiPresetSpec"))
    StateManager
      .apply(
        logger,
        uiPresetStore = store,
        windowSizeProvider = windowSize,
        onPreferredWindowSizeChanged = onWindowSizeChanged
      )
      .unsafeRunSync()

  "StateManager UI presets" should "save the current UI preset to the preset store" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-save").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val size  = PreferredWindowSize(1500, 950)
    val sm    = managerWithStore(store, IO.pure(Some(size)))

    sm.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Bottom, 12).unsafeRunSync()
    sm.updateState(state =>
      state.copy(
        config = state.config.copy(backgroundStyle = BackgroundStyle.GlassLike),
        theme = Theme.light
      )
    ).unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "save-workbench-preset",
        "Save workbench preset",
        CommandIntent.SaveUiPreset("Workbench"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val saved = store.find("Workbench").unsafeRunSync()

    saved.map(_.themeName) shouldBe Some(Theme.light.name)
    saved.map(_.config.backgroundStyle) shouldBe Some(BackgroundStyle.GlassLike)
    saved.flatMap(_.config.preferredWindowSize) shouldBe Some(size)
    saved.map(_.pinnedPanels.map(panel => panel.position -> panel.size)) shouldBe Some(List(PanelPosition.Bottom -> 12))
  }

  it should "apply a named preset and notify runtime window size changes" in {
    val path               = Files.createTempDirectory("state-manager-ui-preset-apply").resolve("ui-presets.json")
    val store              = UiPresetStore(path)
    val observedWindowSize = Ref.of[IO, Option[PreferredWindowSize]](None).unsafeRunSync()
    val sm = managerWithStore(
      store,
      onWindowSizeChanged = size => observedWindowSize.set(Some(size))
    )
    val preset = com.serenity.ui.presets.UiPreset(
      name = "Review",
      config = AppConfig.default.copy(
        backgroundStyle = BackgroundStyle.Solid,
        preferredWindowSize = Some(PreferredWindowSize(1280, 720))
      ),
      themeName = Theme.dark.name,
      pinnedPanels = List(
        com.serenity.ui.presets.UiPreset.PinnedPanel
          .fromPanelContent(
            PanelContent.Outline(Nil),
            PanelPosition.Right,
            36
          )
          .getOrElse(fail("outline should be capturable"))
      )
    )
    store.upsert(preset).unsafeRunSync()

    sm.executeCommand(
      Command.typed(
        "apply-review-preset",
        "Apply review preset",
        CommandIntent.ApplyUiPreset("Review"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    state.config.backgroundStyle shouldBe BackgroundStyle.Solid
    state.config.preferredWindowSize shouldBe Some(PreferredWindowSize(1280, 720))
    state.theme.name shouldBe Theme.dark.name
    state.pinnedSurfaces.map(_.presentation) shouldBe List(SurfacePresentation.Pinned(PanelPosition.Right, 36))
    observedWindowSize.get.unsafeRunSync() shouldBe Some(PreferredWindowSize(1280, 720))
  }

  it should "apply a built-in writing preset when no custom preset exists" in {
    val path  = Files.createTempDirectory("state-manager-built-in-ui-preset").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.executeCommand(
      Command.typed(
        "apply-writing-preset",
        "Apply writing preset",
        CommandIntent.ApplyUiPreset("Writing"),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    state.config.fontConfig.textFontFamily shouldBe Font.SERIF
    state.config.showLineNumbers shouldBe false
    state.config.showGutter shouldBe false
    state.pinnedSurfaces.map(_.presentation) shouldBe List(SurfacePresentation.Pinned(PanelPosition.Left, 28))
    state.pinnedSurfaces.headOption.map(_.content) shouldBe Some(SurfaceContent.Outline(Nil))
  }
