package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerUiPresetsSettingsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  private def groupByIdRecursive(
    groups: List[CommandSurfaceItem.GroupItem],
    id: String
  ): CommandSurfaceItem.GroupItem =
    (groups ++ groups.flatMap(group =>
      descendants(group).collect { case child: CommandSurfaceItem.GroupItem => child }
    ))
      .find(_.id == id)
      .getOrElse(fail(s"missing group $id"))

  "CommandRunner state" should "surface UI preset save and apply inputs in settings" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withUiPresetNames(List("Drafting", "Research Notes"))
    val presetGroup = runner.settingsGroups.find(_.id == "settings-ui-presets").getOrElse(fail("missing presets group"))

    presetGroup.children.map(_.id) shouldBe List(
      "settings-preset-select",
      "settings-preset-create",
      "settings-preset-edit"
    )

    val selectPreset = presetGroup.children
      .collectFirst {
        case group: CommandSurfaceItem.GroupItem if group.id == "settings-preset-select" => group
      }
      .getOrElse(fail("missing select preset group"))
    val presetPicker = selectPreset.children
      .collectFirst {
        case item: CommandSurfaceItem.OptionItem if item.id == "ui-preset-select" => item
      }
      .getOrElse(fail("missing combined preset picker"))

    presetPicker.options
      .map(_.label) shouldBe List("Writing", "Documentation", "Code", "Compact", "Review", "Drafting", "Research Notes")
    presetPicker.options.map(_.intent) should contain(CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Writing")))
    presetPicker.options.map(_.intent) should contain(
      CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Research Notes"))
    )
    presetPicker.options.headOption.flatMap(_.hint) shouldBe Some(
      "rich text default; dark; subtle motion; typed text reveal; frosted material; frosted background; spacious density; Serif 18pt prose; 1 editor pane"
    )
    presetPicker.options.takeRight(2).map(_.hint) shouldBe List(
      Some("Saved workspace setup"),
      Some("Saved workspace setup")
    )

    val createPreset = presetGroup.children
      .collectFirst {
        case item: CommandSurfaceItem.GroupItem if item.id == "settings-preset-create" => item
      }
      .getOrElse(fail("missing create preset group"))
    val editPreset = presetGroup.children
      .collectFirst {
        case item: CommandSurfaceItem.GroupItem if item.id == "settings-preset-edit" => item
      }
      .getOrElse(fail("missing edit preset group"))

    createPreset.label shouldBe "Create New Preset"
    createPreset.children.map(_.id) shouldBe List(
      "settings-preset-create-name",
      "settings-preset-active-panels",
      "settings-preset-theme",
      "settings-preset-animations",
      "settings-preset-fonts",
      "settings-preset-document-defaults"
    )
    editPreset.label shouldBe "Edit Preset: Writing"
    editPreset.hint shouldBe Some("Editing Writing")
    editPreset.children.map(_.id) shouldBe List(
      "settings-preset-name",
      "settings-preset-actions",
      "settings-preset-active-panels",
      "settings-preset-theme",
      "settings-preset-animations",
      "settings-preset-fonts",
      "settings-preset-document-defaults"
    )
    val createName = createPreset.children
      .collectFirst {
        case group: CommandSurfaceItem.GroupItem if group.id == "settings-preset-create-name" => group
      }
      .getOrElse(fail("missing create preset name group"))
    createName.children.map(_.id) shouldBe List("ui-preset-save-as-new")

    val presetName = editPreset.children
      .collectFirst {
        case group: CommandSurfaceItem.GroupItem if group.id == "settings-preset-name" => group
      }
      .getOrElse(fail("missing preset name group"))
    presetName.label shouldBe "Name"
    presetName.children.map(_.id) shouldBe List("ui-preset-rename")
    val presetActions = editPreset.children
      .collectFirst {
        case group: CommandSurfaceItem.GroupItem if group.id == "settings-preset-actions" => group
      }
      .getOrElse(fail("missing preset actions group"))
    presetActions.label shouldBe "Preset Actions"
    presetActions.children.map(_.id) shouldBe List(
      "ui-preset-apply",
      "ui-preset-overwrite",
      "ui-preset-duplicate",
      "ui-preset-delete",
      "ui-preset-reset"
    )
    val activePanels = editPreset.children
      .collectFirst {
        case group: CommandSurfaceItem.GroupItem if group.id == "settings-preset-active-panels" => group
      }
      .getOrElse(fail("missing active panels group"))
    activePanels.label shouldBe "Active Panels"
    val workspaceItems = descendants(activePanels)
    workspaceItems.collect {
      case option: CommandSurfaceItem.OptionItem => option.options.map(_.intent)
    }.flatten should contain allOf (
      CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Outline, Some(PanelPosition.Right))),
      CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.MarkdownPreview, Some(PanelPosition.Right))),
      CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Explorer, Some(PanelPosition.Left))),
      CommandIntent.View(ViewIntent.SetPanelPin(PanelKind.Diagnostics, Some(PanelPosition.Bottom)))
    )
    workspaceItems.collect { case CommandSurfaceItem.CommandItem(command) => command.intent } shouldBe Nil
    val animations = groupByIdRecursive(List(editPreset), "settings-preset-animations")
    animations.children.map(_.id) shouldBe List(
      "settings-preset-cursor-motion",
      "settings-preset-text-entry-motion",
      "settings-preset-ui-surface-motion"
    )
    val fonts = groupByIdRecursive(List(editPreset), "settings-preset-fonts")
    fonts.children.map(_.id) shouldBe List(
      "settings-preset-editor-typography",
      "settings-preset-code-typography",
      "settings-preset-ui-typography"
    )
    val documentDefaults = groupByIdRecursive(List(editPreset), "settings-preset-document-defaults")
    documentDefaults.children.map(_.id) shouldBe List(
      "settings-preset-new-documents",
      "settings-preset-markdown-preview",
      "settings-preset-spelling"
    )
    descendants(documentDefaults).map(_.id) should contain allOf (
      "default-document-mode",
      "markdown-view",
      "spellcheck-enabled",
      "spellcheck-languages",
      "spellcheck-dictionaries",
      "spellcheck-words"
    )
    descendants(documentDefaults).map(_.id) should not contain "lang-plain-text"
    // issue #1057: this used to also carry a "settings-preset-theme-selection" child (Theme Chooser/Creator/Toggle/
    // Reload) -- those are one-shot actions with no preset-scoped value of their own, now ordinary CommandRegistry
    // commands (CommandRunnerOneShotActionsSpec), not part of this settings subtree.
    val theme = groupByIdRecursive(List(editPreset), "settings-preset-theme")
    theme.label shouldBe "Theme & Surface"
    theme.children.map(_.id) shouldBe List("settings-preset-surface-material")
    descendants(theme).map(_.id) should contain allOf ("background-style", "material-preset", "blur-radius")

    val inputs = descendants(presetGroup).collect {
      case item: CommandSurfaceItem.InputItem if item.id.startsWith("ui-preset-") => item
    }
    inputs.map(_.id) should contain allOf (
      "ui-preset-save-as-new",
      "ui-preset-apply",
      "ui-preset-overwrite",
      "ui-preset-duplicate",
      "ui-preset-rename",
      "ui-preset-delete",
      "ui-preset-reset"
    )
    val saveAsNewInput = inputs.find(_.id == "ui-preset-save-as-new").getOrElse(fail("missing save-as-new input"))
    val overwriteInput = inputs.find(_.id == "ui-preset-overwrite").getOrElse(fail("missing overwrite input"))
    val applyInput     = inputs.find(_.id == "ui-preset-apply").getOrElse(fail("missing apply input"))
    val dupeInput      = inputs.find(_.id == "ui-preset-duplicate").getOrElse(fail("missing duplicate input"))
    val renameInput    = inputs.find(_.id == "ui-preset-rename").getOrElse(fail("missing rename input"))
    val deleteInput    = inputs.find(_.id == "ui-preset-delete").getOrElse(fail("missing delete input"))
    val resetInput     = inputs.find(_.id == "ui-preset-reset").getOrElse(fail("missing reset input"))

    saveAsNewInput.label shouldBe "Save As New Preset"
    saveAsNewInput.hint shouldBe "New preset name"
    overwriteInput.currentValue shouldBe "Writing"
    applyInput.currentValue shouldBe "Writing"
    dupeInput.currentValue shouldBe "Writing -> "
    renameInput.currentValue shouldBe "Writing -> "
    deleteInput.currentValue shouldBe "Writing"
    resetInput.currentValue shouldBe "Writing"
    saveAsNewInput.parse("Longform Writing") shouldBe Some(
      CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Longform Writing"))
    )
    overwriteInput.parse("Writing") shouldBe Some(CommandIntent.UiPresets(UiPresetsIntent.OverwriteUiPreset("Writing")))
    dupeInput.parse("Writing -> My Writing") shouldBe Some(
      CommandIntent.UiPresets(UiPresetsIntent.DuplicateUiPreset("Writing", "My Writing"))
    )
    renameInput.parse("Draft -> Final") shouldBe Some(
      CommandIntent.UiPresets(UiPresetsIntent.RenameUiPreset("Draft", "Final"))
    )
    deleteInput.parse("Old Preset") shouldBe Some(CommandIntent.UiPresets(UiPresetsIntent.DeleteUiPreset("Old Preset")))
    resetInput.parse("Writing") shouldBe Some(CommandIntent.UiPresets(UiPresetsIntent.ResetUiPreset("Writing")))
    inputs.foreach { item =>
      item.accepts("", 'W') shouldBe true
      item.accepts("Work", ' ') shouldBe true
    }
  }

  it should "preserve selected built-in and custom UI presets in the settings submenu" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withUiPresetNames(List("Drafting", "Research Notes"))
      .copy(optionSelections = Map("ui-preset-built-in" -> 2, "ui-preset-custom" -> 1))

    val presetGroup = runner.settingsGroups.find(_.id == "settings-ui-presets").getOrElse(fail("missing presets group"))
    val presetPicker = descendants(presetGroup)
      .collectFirst {
        case item: CommandSurfaceItem.OptionItem if item.id == "ui-preset-select" => item
      }
      .getOrElse(fail("missing preset picker"))

    presetPicker.options.map(_.label) shouldBe List(
      "Writing",
      "Documentation",
      "Code",
      "Compact",
      "Review",
      "Drafting",
      "Research Notes"
    )
    presetPicker.selectedOption shouldBe "Research Notes"

    val inputs = descendants(presetGroup).collect {
      case item: CommandSurfaceItem.InputItem if item.id.startsWith("ui-preset-") => item
    }
    inputs.find(_.id == "ui-preset-overwrite").map(_.currentValue) shouldBe Some("Research Notes")
    inputs.find(_.id == "ui-preset-apply").map(_.currentValue) shouldBe Some("Research Notes")
    inputs.find(_.id == "ui-preset-duplicate").map(_.currentValue) shouldBe Some("Research Notes -> ")
    inputs.find(_.id == "ui-preset-rename").map(_.currentValue) shouldBe Some("Research Notes -> ")
    inputs.find(_.id == "ui-preset-delete").map(_.currentValue) shouldBe Some("Research Notes")
    inputs.find(_.id == "ui-preset-reset").map(_.currentValue) shouldBe Some("Research Notes")
  }
