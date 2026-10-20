package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerUiPresetsSettingsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  "CommandRunner state" should "surface UI preset save and apply inputs in settings" in {
    val registry          = CommandRegistry.default
    given CommandRegistry = registry
    val runner = CommandRunner.empty
      .activate(registry, AppConfig.default)
      .withUiPresetNames(List("Drafting", "Research Notes"))
    val presetGroup = runner.settingsGroups
      .flatMap(group => group :: descendants(group))
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == "settings-ui-presets" => group }
      .getOrElse(fail("missing presets group"))

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
      "rich text default; spacious density; Serif 18pt prose; 1 editor pane"
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

    // issue #1682: the edit page is flat -- every setting a preset can hold is a row on it, not a page below it -- and
    // only those settings are offered. Create is just Save As New, and the panel layout is captured whole by
    // Overwrite Preset.
    val own = "preset:Writing:"
    createPreset.label shouldBe "Save As New Preset"
    createPreset.children.map(_.id) shouldBe List("ui-preset-save-as-new")
    editPreset.label shouldBe "Edit Preset: Writing"
    editPreset.hint shouldBe Some("Editing Writing. Panel layout: use Overwrite Preset")
    editPreset.children.collect { case group: CommandSurfaceItem.GroupItem => group.id } shouldBe Nil
    editPreset.children.map(_.id) should contain inOrderOnly (
      "ui-preset-rename",
      "ui-preset-apply",
      "ui-preset-overwrite",
      "ui-preset-duplicate",
      "ui-preset-set-theme",
      "ui-preset-use-current-theme",
      "ui-preset-clear-theme",
      "ui-preset-delete",
      "ui-preset-reset",
      s"${own}cursor-mode",
      s"${own}text-font",
      s"${own}text-ligatures",
      s"${own}text-font-size",
      s"${own}code-font",
      s"${own}code-ligatures",
      s"${own}code-font-size",
      s"${own}ui-font",
      s"${own}ui-ligatures",
      s"${own}ui-font-size",
      s"${own}default-document-mode",
      s"${own}markdown-view",
      s"${own}drop-caps-enabled",
      s"${own}spellcheck-enabled",
      s"${own}spellcheck-languages",
      s"${own}spellcheck-dictionaries",
      s"${own}spellcheck-words"
    )
    descendants(editPreset).map(_.id) should not contain "arrange-panels"
    descendants(editPreset).map(_.id) should not contain "lang-plain-text"
    // issue #1057: this used to also carry a "Theme Selection" child (Theme Chooser/Creator/Toggle/Reload) -- those
    // are one-shot actions with no preset-scoped value of their own, now ordinary CommandRegistry commands
    // (CommandRunnerOneShotActionsSpec), not part of this settings subtree.
    // The font families cycle in place like every other carousel row.
    List("text-font", "code-font", "ui-font").foreach { family =>
      editPreset.children.collectFirst {
        case item: CommandSurfaceItem.OptionItem if item.id == s"$own$family" => item
      } should not be empty
    }

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
    applyOption.selectedIntent shouldBe Some(CommandIntent.UiPresets(UiPresetsIntent.ReviewUiPreset("Writing")))
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

    val presetGroup = runner.settingsGroups
      .flatMap(group => group :: descendants(group))
      .collectFirst { case group: CommandSurfaceItem.GroupItem if group.id == "settings-ui-presets" => group }
      .getOrElse(fail("missing presets group"))
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
