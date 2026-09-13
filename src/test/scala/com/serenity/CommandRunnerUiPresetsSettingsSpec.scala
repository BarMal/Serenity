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

    // issue #1058: editing/creating a preset used to walk a hand-maintained parallel clone tree (Active Panels,
    // Theme & Surface > Surface Material, Animations > Cursor/Text Entry/UI Surface Motion, Fonts > Editor/Code/UI
    // Typography, Document Defaults > New Documents/Markdown Preview/Spelling) that re-sliced the same canonical
    // items several levels deeper than their top-level counterparts. It now reuses the same canonical settings
    // groups directly (see `presetScopedGroups` in `CommandRunnerSettingsGroups`), just re-tagged with
    // `settings-preset-*` ids so they remain addressable as distinct pages.
    val presetScopedGroupIds = List(
      "settings-preset-workspace-layout",
      "settings-preset-surface-appearance",
      "settings-preset-cursor",
      "settings-preset-animation",
      "settings-preset-prose-font",
      "settings-preset-code-font",
      "settings-preset-ui-font",
      "settings-preset-document-defaults",
      "settings-preset-spellcheck"
    )
    createPreset.label shouldBe "Create New Preset"
    createPreset.children.map(_.id) shouldBe "settings-preset-create-name" :: presetScopedGroupIds
    editPreset.label shouldBe "Edit Preset: Writing"
    editPreset.hint shouldBe Some("Editing Writing")
    editPreset.children.map(_.id) shouldBe
      List("settings-preset-name", "settings-preset-actions") ++ presetScopedGroupIds
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
        case group: CommandSurfaceItem.GroupItem if group.id == "settings-preset-workspace-layout" => group
      }
      .getOrElse(fail("missing workspace layout group"))
    activePanels.label shouldBe "Panels & Workspace"
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
    val animation = groupByIdRecursive(List(editPreset), "settings-preset-animation")
    animation.label shouldBe "Motion & Animation"
    animation.children.map(_.id) should contain allOf ("motion-preset", "cursor-speed-scale", "editor-text-transition")
    val proseFont = groupByIdRecursive(List(editPreset), "settings-preset-prose-font")
    proseFont.label shouldBe "Prose Font"
    proseFont.children.map(_.id) should contain allOf ("text-font", "text-ligatures", "text-font-size")
    val codeFont = groupByIdRecursive(List(editPreset), "settings-preset-code-font")
    codeFont.children.map(_.id) should contain allOf ("code-font", "code-ligatures", "code-font-size")
    val uiFont = groupByIdRecursive(List(editPreset), "settings-preset-ui-font")
    uiFont.children.map(_.id) should contain allOf ("ui-font", "ui-ligatures", "ui-font-size")
    val documentDefaults = groupByIdRecursive(List(editPreset), "settings-preset-document-defaults")
    documentDefaults.label shouldBe "Document Defaults"
    documentDefaults.children.map(_.id) shouldBe List("default-document-mode", "markdown-view")
    val spellcheck = groupByIdRecursive(List(editPreset), "settings-preset-spellcheck")
    spellcheck.label shouldBe "Spell Check"
    spellcheck.children.map(_.id) should contain allOf (
      "spellcheck-enabled",
      "spellcheck-languages",
      "spellcheck-dictionaries",
      "spellcheck-words"
    )
    descendants(documentDefaults).map(_.id) should not contain "lang-plain-text"
    // issue #1057: this used to also carry a "Theme Selection" child (Theme Chooser/Creator/Toggle/Reload) -- those
    // are one-shot actions with no preset-scoped value of their own, now ordinary CommandRegistry commands
    // (CommandRunnerOneShotActionsSpec), not part of this settings subtree.
    val surfaceAppearance = groupByIdRecursive(List(editPreset), "settings-preset-surface-appearance")
    surfaceAppearance.label shouldBe "Surface Appearance"
    surfaceAppearance.children.map(_.id) should contain allOf ("background-style", "material-preset", "blur-radius")

    // issue #1060: Apply/Overwrite/Delete/Reset now pick from the existing-preset catalog instead of requiring a
    // typed exact name -- Duplicate/Rename/Save-As-New still need typed input since each needs a *new* name.
    val inputs = descendants(presetGroup).collect {
      case item: CommandSurfaceItem.InputItem if item.id.startsWith("ui-preset-") => item
    }
    inputs.map(_.id) should contain allOf ("ui-preset-save-as-new", "ui-preset-duplicate", "ui-preset-rename")
    inputs.map(_.id) should not contain "ui-preset-apply"
    inputs.map(_.id) should not contain "ui-preset-overwrite"
    inputs.map(_.id) should not contain "ui-preset-delete"
    inputs.map(_.id) should not contain "ui-preset-reset"

    val saveAsNewInput = inputs.find(_.id == "ui-preset-save-as-new").getOrElse(fail("missing save-as-new input"))
    val dupeInput      = inputs.find(_.id == "ui-preset-duplicate").getOrElse(fail("missing duplicate input"))
    val renameInput    = inputs.find(_.id == "ui-preset-rename").getOrElse(fail("missing rename input"))

    saveAsNewInput.label shouldBe "Save As New Preset"
    saveAsNewInput.hint shouldBe "New preset name"
    dupeInput.currentValue shouldBe "Writing -> "
    renameInput.currentValue shouldBe "Writing -> "
    saveAsNewInput.parse("Longform Writing") shouldBe Some(
      CommandIntent.UiPresets(UiPresetsIntent.SaveUiPresetAsNew("Longform Writing"))
    )
    dupeInput.parse("Writing -> My Writing") shouldBe Some(
      CommandIntent.UiPresets(UiPresetsIntent.DuplicateUiPreset("Writing", "My Writing"))
    )
    renameInput.parse("Draft -> Final") shouldBe Some(
      CommandIntent.UiPresets(UiPresetsIntent.RenameUiPreset("Draft", "Final"))
    )
    inputs.foreach { item =>
      item.accepts("", 'W') shouldBe true
      item.accepts("Work", ' ') shouldBe true
    }

    val presetOptions = descendants(presetGroup).collect {
      case item: CommandSurfaceItem.OptionItem
          if List("ui-preset-apply", "ui-preset-overwrite", "ui-preset-delete", "ui-preset-reset").contains(item.id) =>
        item
    }
    val applyOption     = presetOptions.find(_.id == "ui-preset-apply").getOrElse(fail("missing apply picker"))
    val overwriteOption = presetOptions.find(_.id == "ui-preset-overwrite").getOrElse(fail("missing overwrite picker"))
    val deleteOption    = presetOptions.find(_.id == "ui-preset-delete").getOrElse(fail("missing delete picker"))
    val resetOption     = presetOptions.find(_.id == "ui-preset-reset").getOrElse(fail("missing reset picker"))

    // Each defaults to the preset currently being edited ("Writing").
    List(applyOption, overwriteOption, deleteOption, resetOption).foreach(_.selectedOption shouldBe "Writing")
    applyOption.selectedIntent shouldBe Some(CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Writing")))
    overwriteOption.selectedIntent shouldBe Some(CommandIntent.UiPresets(UiPresetsIntent.OverwriteUiPreset("Writing")))
    deleteOption.selectedIntent shouldBe Some(CommandIntent.UiPresets(UiPresetsIntent.DeleteUiPreset("Writing")))
    resetOption.selectedIntent shouldBe Some(CommandIntent.UiPresets(UiPresetsIntent.ResetUiPreset("Writing")))
    applyOption.options.map(_.label) shouldBe
      List("Writing", "Documentation", "Code", "Compact", "Review", "Drafting", "Research Notes")
    overwriteOption.options.map(_.intent) should contain(
      CommandIntent.UiPresets(UiPresetsIntent.OverwriteUiPreset("Documentation"))
    )
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
    inputs.find(_.id == "ui-preset-duplicate").map(_.currentValue) shouldBe Some("Research Notes -> ")
    inputs.find(_.id == "ui-preset-rename").map(_.currentValue) shouldBe Some("Research Notes -> ")

    val options = descendants(presetGroup).collect {
      case item: CommandSurfaceItem.OptionItem
          if List("ui-preset-apply", "ui-preset-overwrite", "ui-preset-delete", "ui-preset-reset").contains(item.id) =>
        item
    }
    options.find(_.id == "ui-preset-overwrite").map(_.selectedOption) shouldBe Some("Research Notes")
    options.find(_.id == "ui-preset-apply").map(_.selectedOption) shouldBe Some("Research Notes")
    options.find(_.id == "ui-preset-delete").map(_.selectedOption) shouldBe Some("Research Notes")
    options.find(_.id == "ui-preset-reset").map(_.selectedOption) shouldBe Some("Research Notes")
  }
