package com.serenity

import java.awt.Font
import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerUiPresetManagementSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def managerWithStore(
    store: UiPresetStore,
    windowSize: IO[Option[PreferredWindowSize]] = IO.pure(None),
    onWindowSizeChanged: PreferredWindowSize => IO[Unit] = _ => IO.unit,
    sessionRoot: Option[Path] = None
  ): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerUiPresetManagementSpec"))
    StateManager
      .apply(
        logger,
        uiPresetStore = store,
        windowSizeProvider = windowSize,
        onPreferredWindowSizeChanged = onWindowSizeChanged,
        sessionRootOverride = sessionRoot
      )
      .unsafeRunSync()

  private def commandRunnerState(sm: StateManager): com.serenity.command.CommandRunner =
    sm.getCurrentState
      .map(
        _.commandRunnerSurface.flatMap {
          _.content match
            case SurfaceContent.CommandPalette(runner) => Some(runner)
            case _                                     => None
        }
      )
      .unsafeRunSync()
      .getOrElse(fail("command runner should be open"))

  "StateManager UI presets" should "duplicate, rename, and delete UI presets from commands" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-management").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.executeCommand(
      Command.typed(
        "duplicate-writing-preset",
        "Duplicate writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.DuplicateUiPreset("Writing", "Personal Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    store.find("Personal Writing").unsafeRunSync() should not be empty

    sm.executeCommand(
      Command.typed(
        "rename-writing-preset",
        "Rename writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.RenameUiPreset("Personal Writing", "Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    store.find("Personal Writing").unsafeRunSync() shouldBe None
    store.find("Drafting").unsafeRunSync() should not be empty

    sm.executeCommand(
      Command.typed(
        "delete-writing-preset",
        "Delete writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.DeleteUiPreset("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    store.find("Drafting").unsafeRunSync() shouldBe None
    com.serenity.ui.presets.UiPreset.builtIn("Writing") should not be empty
  }

  it should "reject saving or duplicating over an existing preset name" in {
    val path     = Files.createTempDirectory("state-manager-ui-preset-name-collision").resolve("ui-presets.json")
    val store    = UiPresetStore(path)
    val existing = UiPreset("Drafting", AppConfig.default.withLineNumbers(false), Theme.dark.name, Nil)
    val sm       = managerWithStore(store)
    store.upsert(existing).unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "save-as-new",
        "Save as new",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    store.find("Drafting").unsafeRunSync() shouldBe Some(existing)
    commandRunnerState(sm).statusMessage.getOrElse(fail("save failure should be visible")) should include(
      "Could not save Drafting"
    )

    sm.executeCommand(
      Command.typed(
        "duplicate",
        "Duplicate",
        CommandIntent.UiPresets(UiPresetsIntent.DuplicateUiPreset("Writing", "Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    store.find("Drafting").unsafeRunSync() shouldBe Some(existing)
    commandRunnerState(sm).statusMessage.getOrElse(fail("duplicate failure should be visible")) should include(
      "Could not duplicate Writing"
    )
  }

  it should "keep built-in presets immutable for rename commands" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-rename-built-in").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "rename-writing-preset",
        "Rename writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.RenameUiPreset("Writing", "Personal Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val runner = commandRunnerState(sm)

    store.find("Personal Writing").unsafeRunSync() shouldBe None
    runner.editingPresetName shouldBe Some("Writing")
    runner.statusMessage shouldBe Some("Built-in preset cannot be renamed. Duplicate Writing first.")
  }

  it should "overwrite a custom preset with the live workspace and refuse invalid overwrites" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-overwrite").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    store.upsert(UiPreset("Drafting", AppConfig.default.withLineNumbers(false), Theme.dark.name, Nil)).unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "set-markdown-default",
        "Set document default",
        CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown)),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "overwrite",
        "Overwrite",
        CommandIntent.UiPresets(UiPresetsIntent.OverwriteUiPreset("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val saved = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))
    saved.config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
    commandRunnerState(sm).statusMessage shouldBe Some("Preset overwritten. Configure Drafting.")

    sm.executeCommand(
      Command.typed(
        "overwrite-built-in",
        "Overwrite",
        CommandIntent.UiPresets(UiPresetsIntent.OverwriteUiPreset("Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    store.find("Writing").unsafeRunSync() shouldBe None
    commandRunnerState(sm).statusMessage shouldBe Some(
      "Built-in preset cannot be overwritten. Duplicate Writing first."
    )

    sm.executeCommand(
      Command.typed(
        "overwrite-missing",
        "Overwrite",
        CommandIntent.UiPresets(UiPresetsIntent.OverwriteUiPreset("Absent")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    store.find("Absent").unsafeRunSync() shouldBe None
    commandRunnerState(sm).statusMessage shouldBe Some(
      "Custom preset 'Absent' was not found. Use Save As New Preset."
    )
  }

  it should "reject unavailable preset resources without changing the workspace" in {
    val path    = Files.createTempDirectory("state-manager-ui-preset-unavailable-resources").resolve("ui-presets.json")
    val store   = UiPresetStore(path)
    val sm      = managerWithStore(store)
    val initial = sm.getCurrentState.unsafeRunSync()
    store
      .upsert(UiPreset("Missing Theme", AppConfig.default.withLineNumbers(false), "not-installed", Nil))
      .unsafeRunSync()
    store
      .upsert(
        UiPreset(
          "Missing Font",
          AppConfig.default.withFontConfig(
            AppConfig.default.editorConfig.fontConfig.copy(textFontFamily = "not-installed")
          ),
          Theme.dark.name,
          Nil
        )
      )
      .unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "apply",
        "Apply",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Missing Theme")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().persisted.config shouldBe initial.persisted.config
    commandRunnerState(sm).statusMessage.getOrElse(fail("missing preset error")) should include(
      "Theme 'not-installed' could not be loaded"
    )

    sm.executeCommand(
      Command.typed(
        "apply-font",
        "Apply",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Missing Font")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().persisted.config shouldBe initial.persisted.config
    commandRunnerState(sm).statusMessage.getOrElse(fail("missing preset error")) should include(
      "Preset requires unavailable text font 'not-installed'"
    )
  }

  it should "keep built-in presets immutable for delete commands" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-delete-built-in").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "delete-writing-preset",
        "Delete writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.DeleteUiPreset("Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val runner = commandRunnerState(sm)

    store.find("Writing").unsafeRunSync() shouldBe None
    runner.editingPresetName shouldBe Some("Writing")
    runner.statusMessage shouldBe Some("Built-in preset cannot be deleted. Use Reset Preset to discard overrides.")
  }

  it should "reset a custom built-in preset override to the built-in defaults" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-reset").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    sm.executeCommand(
      Command.typed(
        "reset-writing-preset",
        "Reset writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.ResetUiPreset("Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "apply-writing-preset",
        "Apply writing preset",
        CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Writing")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()

    store.find("Writing").unsafeRunSync() shouldBe None
    state.persisted.config.editorConfig.fontConfig.textFontFamily shouldBe Font.SERIF
    state.pinnedSurfaces shouldBe Nil
  }
