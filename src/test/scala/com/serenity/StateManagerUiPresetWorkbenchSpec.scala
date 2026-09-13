package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class StateManagerUiPresetWorkbenchSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def managerWithStore(
    store: UiPresetStore,
    windowSize: IO[Option[PreferredWindowSize]] = IO.pure(None),
    onWindowSizeChanged: PreferredWindowSize => IO[Unit] = _ => IO.unit,
    sessionRoot: Option[Path] = None
  ): StateManager =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerUiPresetWorkbenchSpec"))
    StateManager
      .apply(
        logger,
        uiPresetStore = store,
        windowSizeProvider = windowSize,
        onPreferredWindowSizeChanged = onWindowSizeChanged,
        sessionRootOverride = sessionRoot
      )
      .unsafeRunSync()

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

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

  "StateManager UI presets" should "list custom UI presets in the command runner when opened" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-list").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    val preset = UiPreset(
      name = "Drafting",
      config = AppConfig.default
        .withMotionPreset(com.serenity.config.MotionPreset.Reduced)
        .withMaterialPreset(com.serenity.config.MaterialPreset.Solid),
      themeName = Theme.dark.name,
      dockedPanels = List(
        SessionDockedPanel(
          "panel-1",
          SessionPinnedPanel
            .fromPanelContent(PanelContent.Outline(Nil), PanelPosition.Right, 34)
            .getOrElse(fail("outline should be capturable"))
        )
      )
    )
    store.upsert(preset).unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val runner = sm.getCurrentState
      .map(
        _.commandRunnerSurface.flatMap {
          _.content match
            case SurfaceContent.CommandPalette(runner) => Some(runner)
            case _                                     => None
        }
      )
      .unsafeRunSync()
      .getOrElse(fail("command runner should be open"))
    val presetGroup = runner.settingsGroups.find(_.id == "settings-ui-presets").getOrElse(fail("missing presets group"))
    val presetPicker = descendants(presetGroup)
      .collectFirst {
        case item: CommandSurfaceItem.OptionItem if item.id == "ui-preset-select" => item
      }
      .getOrElse(fail("missing preset picker"))

    presetPicker.options.map(_.label) should contain("Drafting")
    presetPicker.options.find(_.label == "Drafting").map(_.intent) shouldBe Some(
      CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Drafting"))
    )
    presetPicker.options.find(_.label == "Drafting").flatMap(_.hint) shouldBe Some(
      "plain text default; dark; reduced motion; fade text reveal; solid material; solid background; comfortable density; SansSerif 12pt prose; Right outline 34"
    )
  }

  it should "refresh custom UI presets in an open command runner after saving" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-refresh").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "save-drafting-preset",
        "Save drafting preset",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val runner = sm.getCurrentState
      .map(
        _.commandRunnerSurface.flatMap {
          _.content match
            case SurfaceContent.CommandPalette(runner) => Some(runner)
            case _                                     => None
        }
      )
      .unsafeRunSync()
      .getOrElse(fail("command runner should stay open"))
    val presetGroup = runner.settingsGroups.find(_.id == "settings-ui-presets").getOrElse(fail("missing presets group"))
    val presetPicker = descendants(presetGroup)
      .collectFirst {
        case item: CommandSurfaceItem.OptionItem if item.id == "ui-preset-select" => item
      }
      .getOrElse(fail("missing preset picker"))

    presetPicker.options.map(_.label) should contain("Drafting")
    presetPicker.options.find(_.label == "Drafting").map(_.intent) shouldBe Some(
      CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Drafting"))
    )
  }

  it should "keep command runner preset context current after preset management actions" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-action-status").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)
    val preset = UiPreset(
      name = "Drafting",
      config = AppConfig.default,
      themeName = Theme.dark.name,
      dockedPanels = Nil
    )
    store.upsert(preset).unsafeRunSync()

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "duplicate-drafting-preset",
        "Duplicate drafting preset",
        CommandIntent.UiPresets(UiPresetsIntent.DuplicateUiPreset("Drafting", "Drafting Copy")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    store.find("Drafting Copy").unsafeRunSync() should not be empty
    commandRunnerState(sm).editingPresetName shouldBe Some("Drafting Copy")
    commandRunnerState(sm).statusMessage shouldBe Some("Preset duplicated. Configure Drafting Copy.")

    sm.executeCommand(
      Command.typed(
        "rename-drafting-copy-preset",
        "Rename drafting copy preset",
        CommandIntent.UiPresets(UiPresetsIntent.RenameUiPreset("Drafting Copy", "Final Draft")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    commandRunnerState(sm).editingPresetName shouldBe Some("Final Draft")
    commandRunnerState(sm).statusMessage shouldBe Some("Preset renamed. Configure Final Draft.")

    sm.executeCommand(
      Command.typed(
        "delete-final-draft-preset",
        "Delete final draft preset",
        CommandIntent.UiPresets(UiPresetsIntent.DeleteUiPreset("Final Draft")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    commandRunnerState(sm).editingPresetName shouldBe None
    commandRunnerState(sm).statusMessage shouldBe Some("Preset deleted.")
  }

  it should "open preset options after saving a new UI preset from the command runner" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-create-options").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-save-as-new",
        "Save preset as new",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val runner = state.commandRunnerSurface
      .flatMap {
        _.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None
      }
      .getOrElse(fail("command runner should stay open"))
    runner.activeSettingsSurface.map(_.current.groupId) shouldBe Some("settings-preset-edit")
    runner.activeSettingsSurface.flatMap(_.ancestors.headOption.map(_.groupId)) shouldBe Some("settings-ui-presets")
    runner.editingPresetName shouldBe Some("Drafting")
    runner.statusMessage shouldBe Some("Preset saved. Configure Drafting.")
    state.runtime.uiSurfaces should have size 1
    state.persisted.focus shouldBe Focus.Surface(state.commandRunnerSurface.get.id)
  }

  it should "leave a saved preset untouched while later settings change the live workspace" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-snapshot").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-save-as-new",
        "Save preset as new",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    List(
      CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown)),
      CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetMotionPreset(MotionPreset.Subtle))),
      CommandIntent.Settings(
        SettingsIntent.General(GeneralSettingsIntent.SetBackgroundStyle(BackgroundStyle.GlassLike))
      ),
      CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextFontSize(18.0f))),
      CommandIntent.View(ViewIntent.PinOutlinePanel)
    ).zipWithIndex.foreach { (intent, index) =>
      sm.executeCommand(Command.typed(s"change-$index", "Change setting", intent, CommandCategory.Settings))
        .unsafeRunSync()
    }

    val state = sm.getCurrentState.unsafeRunSync()

    state.persisted.config.defaultDocumentMode shouldBe DefaultDocumentMode.Markdown
    state.persisted.config.surfaceConfig.motionPreset shouldBe MotionPreset.Subtle
    state.persisted.config.surfaceConfig.backgroundStyle shouldBe BackgroundStyle.GlassLike
    state.persisted.config.editorConfig.fontConfig.textFontSize shouldBe 18.0f
    store.find("Drafting").unsafeRunSync() shouldBe Some(savedBefore)
  }

  it should "capture live config, theme, and panel changes when overwriting a preset" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-overwrite-capture").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-save-as-new",
        "Save preset as new",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "change-document-default",
        "Change document default",
        CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.Markdown)),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "pin-outline",
        "Pin outline",
        CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Right))),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.updateState(state => state.copy(persisted = state.persisted.copy(theme = Theme.light))).unsafeRunSync()
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
    saved.themeName shouldBe Theme.light.name
    saved.pinnedPanels.map(_.position) shouldBe List(PanelPosition.Right)
  }

  it should "expose panel reorder commands in the preset active panels group" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-edit-panel-order-menu").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-save-as-new",
        "Save preset as new",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "pin-drafting-outline-right",
        "Pin drafting outline right",
        CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Right))),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "pin-drafting-diagnostics-right",
        "Pin drafting diagnostics right",
        CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Right))),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val runner      = commandRunnerState(sm)
    val presetGroup = runner.settingsGroups.find(_.id == "settings-ui-presets").getOrElse(fail("missing presets group"))
    val activePanelsGroup = descendants(presetGroup)
      .collectFirst {
        case item: CommandSurfaceItem.GroupItem if item.id == "settings-preset-workspace-layout" => item
      }
      .getOrElse(fail("missing active panels group"))
    val commands = descendants(activePanelsGroup).collect {
      case item: CommandSurfaceItem.CommandItem =>
        item.command.label -> item.command.intent
    }

    commands should contain(
      "Move Outline Earlier" -> CommandIntent.View(ViewIntent.MovePanelEarlier(PanelKind.Outline))
    )
    commands should contain("Move Outline Later" -> CommandIntent.View(ViewIntent.MovePanelLater(PanelKind.Outline)))
    commands should contain(
      "Move Diagnostics Earlier" -> CommandIntent.View(ViewIntent.MovePanelEarlier(PanelKind.Diagnostics))
    )
    commands should contain(
      "Move Diagnostics Later" -> CommandIntent.View(ViewIntent.MovePanelLater(PanelKind.Diagnostics))
    )
  }

  it should "save the live workspace under a second name without touching the first preset" in {
    val path  = Files.createTempDirectory("state-manager-ui-preset-second-name").resolve("ui-presets.json")
    val store = UiPresetStore(path)
    val sm    = managerWithStore(store)

    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "ui-preset-save-as-new",
        "Save preset as new",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    val savedBefore = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))

    sm.executeCommand(
      Command.typed(
        "set-drafting-rich-text-default",
        "Set drafting document default",
        CommandIntent.View(ViewIntent.SetDefaultDocumentMode(DefaultDocumentMode.RichText)),
        CommandCategory.Settings
      )
    ).unsafeRunSync()
    sm.executeCommand(
      Command.typed(
        "save-drafting-copy",
        "Save drafting copy",
        CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Drafting Edited")),
        CommandCategory.Settings
      )
    ).unsafeRunSync()

    val original = store.find("Drafting").unsafeRunSync().getOrElse(fail("Drafting preset should exist"))
    val copy     = store.find("Drafting Edited").unsafeRunSync().getOrElse(fail("Drafting Edited preset should exist"))
    val runner   = commandRunnerState(sm)

    original shouldBe savedBefore
    copy.config.defaultDocumentMode shouldBe DefaultDocumentMode.RichText
    runner.editingPresetName shouldBe Some("Drafting Edited")
    runner.statusMessage shouldBe Some("Preset saved. Configure Drafting Edited.")
  }
