package com.serenity

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.ui.presets.UiPreset
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerSettingsGroupsSpec extends AnyFlatSpec with Matchers:

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  private def groupById(
    groups: List[CommandSurfaceItem.GroupItem],
    id: String
  ): CommandSurfaceItem.GroupItem =
    (groups ++ groups.flatMap(group =>
      descendants(group).collect { case child: CommandSurfaceItem.GroupItem => child }
    ))
      .find(_.id == id)
      .getOrElse(fail(s"missing group $id"))

  "CommandRunnerSettingsGroups" should "build the settings taxonomy independently of runner navigation state" in {
    val config = AppConfig.default
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )

    groups.map(_.id) shouldBe List(
      "settings-workspace-layout",
      "settings-document-writing",
      "settings-editor-view",
      "settings-typography",
      "settings-appearance-motion",
      "settings-ui-presets",
      "settings-keymap"
    )
    groupById(groups, "settings-workspace-layout").children.map(_.id) shouldBe List(
      "settings-panel-pins"
    )
    groupById(groups, "settings-document-writing").children.map(_.id) shouldBe List(
      "settings-navigation",
      "settings-document-defaults",
      "settings-language",
      "settings-rich-text",
      "settings-spellcheck"
    )
    groupById(groups, "settings-document-defaults").children.map(_.id) shouldBe List(
      "default-document-mode",
      "markdown-view"
    )
    groupById(groups, "settings-language").label shouldBe "Current Buffer Language"
    groupById(groups, "settings-language").children.map(_.id) should contain("lang-plain-text")
    groupById(groups, "settings-language").children.map(_.id) should not contain "default-document-mode"
    groupById(groups, "settings-spellcheck").children.map(_.id) should contain allOf (
      "spellcheck-enabled",
      "spellcheck-languages",
      "spellcheck-dictionaries"
    )
    groupById(groups, "settings-text-display").children.map(_.id) shouldBe List(
      "line-numbers",
      "gutter",
      "line-wrap",
      "focused-text-body",
      "contextual-toolbar",
      "contextual-toolbar-display"
    )
    groupById(groups, "settings-interface-layout").children.map(_.id) shouldBe List(
      "interface-density",
      "window-chrome",
      "ui-element-gap",
      "ui-corner-radius",
      "ui-outline-thickness",
      "command-runner-visible-rows",
      "command-runner-item-gap-rows",
      "command-runner-cursor-gap-rows"
    )
    groupById(groups, "settings-surface-appearance").children.map(_.id) shouldBe List(
      "background-style",
      "material-preset",
      "blur-radius"
    )
    groupById(groups, "settings-animation").children.map(_.id) should contain allOf (
      "motion-preset",
      "animation-mode",
      "editor-text-transition",
      "panel-open-transition",
      "panel-close-transition",
      "command-runner-transition",
      "command-runner-fade",
      "ui-animation",
      "render-fps",
      "element-transition-speed-scale"
    )
    groupById(groups, "settings-cursor").children.map(_.id) shouldBe List(
      "cursor-mode",
      "cursor-info-bar",
      "cursor-info-bar-placement",
      "cursor-speed-scale"
    )
    groupById(groups, "settings-text-area").children.map(_.id) shouldBe List(
      "text-area-left",
      "text-area-right",
      "text-area-top",
      "text-area-bottom"
    )
    groupById(groups, "settings-keymap").children.map(_.id) should contain allOf (
      "keymap-global-command_palette",
      "keymap-command-runner-submit"
    )
  }

  it should "apply preset edit context while deriving preset groups" in {
    val config = AppConfig.default
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config) + ("ui-preset-custom" -> 1),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = List(UiPreset.Preview("Drafting", "Saved workspace setup"), UiPreset.Preview("Review", "")),
      editingPresetName = None
    )

    val editGroup = groupById(groups, "settings-preset-edit")
    editGroup.children.map(_.id).take(2) shouldBe List("settings-preset-name", "settings-preset-actions")
    groupById(groups, "settings-preset-name").children.map(_.id) shouldBe List("ui-preset-rename")
    groupById(groups, "settings-preset-actions").children.map(_.id) shouldBe List(
      "ui-preset-save",
      "ui-preset-apply",
      "ui-preset-duplicate",
      "ui-preset-delete",
      "ui-preset-reset"
    )
    val presetInputs = descendants(editGroup).collect {
      case item: CommandSurfaceItem.InputItem if item.id.startsWith("ui-preset-") => item.id -> item.currentValue
    }.toMap

    editGroup.label shouldBe "Edit Preset: Review"
    presetInputs("ui-preset-save") shouldBe "Review"
    presetInputs("ui-preset-apply") shouldBe "Review"
    presetInputs("ui-preset-duplicate") shouldBe "Review -> "
    presetInputs("ui-preset-rename") shouldBe "Review -> "
    presetInputs("ui-preset-delete") shouldBe "Review"
    presetInputs("ui-preset-reset") shouldBe "Review"
  }

  it should "group preset animation controls by element surface" in {
    val config = AppConfig.default
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )

    groupById(groups, "settings-preset-animations").children.map(_.id) shouldBe List(
      "settings-preset-cursor-motion",
      "settings-preset-text-entry-motion",
      "settings-preset-ui-surface-motion"
    )
    groupById(groups, "settings-preset-cursor-motion").children.map(_.id) shouldBe List(
      "cursor-mode",
      "cursor-speed-scale",
      "cursor-info-bar",
      "cursor-info-bar-placement"
    )
    groupById(groups, "settings-preset-text-entry-motion").children.map(_.id) should contain allOf (
      "editor-text-transition",
      "editor-text-speed-scale",
      "element-transition-speed-scale"
    )
    groupById(groups, "settings-preset-ui-surface-motion").children.map(_.id) should contain allOf (
      "motion-preset",
      "animation-mode",
      "panel-open-transition",
      "panel-close-transition",
      "command-runner-transition",
      "command-runner-fade",
      "command-runner-speed-scale",
      "ui-animation",
      "ui-speed-scale",
      "render-fps"
    )
  }

  it should "group preset font controls by editor and interface surface" in {
    val config = AppConfig.default
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )

    groupById(groups, "settings-preset-fonts").children.map(_.id) shouldBe List(
      "settings-preset-editor-typography",
      "settings-preset-code-typography",
      "settings-preset-ui-typography"
    )
    groupById(groups, "settings-preset-editor-typography").children.map(_.id) shouldBe List(
      "text-font",
      "text-ligatures",
      "text-font-size"
    )
    groupById(groups, "settings-preset-code-typography").children.map(_.id) shouldBe List(
      "code-font",
      "code-ligatures",
      "code-font-size"
    )
    groupById(groups, "settings-preset-ui-typography").children.map(_.id) shouldBe List(
      "ui-font",
      "ui-ligatures",
      "ui-font-size"
    )
  }

  it should "group preset theme controls by theme and surface material" in {
    val config = AppConfig.default
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )

    groupById(groups, "settings-preset-theme").children.map(_.id) shouldBe List(
      "settings-preset-theme-selection",
      "settings-preset-surface-material"
    )
    groupById(groups, "settings-preset-theme-selection").children.map(_.id) shouldBe List(
      "theme-chooser",
      "theme-creator",
      "toggle-theme",
      "reload-theme"
    )
    groupById(groups, "settings-preset-surface-material").children.map(_.id) shouldBe List(
      "background-style",
      "material-preset",
      "blur-radius"
    )
  }

  it should "group preset document defaults by document, preview, and spelling" in {
    val config = AppConfig.default
    val groups = CommandRunnerSettingsGroups.build(
      optionSelections = CommandRunnerOptionSelections.default(config),
      inputItems = CommandRunnerSettingsInputItems.build(config),
      uiPresetPreviews = Nil,
      editingPresetName = None
    )

    groupById(groups, "settings-preset-document-defaults").children.map(_.id) shouldBe List(
      "settings-preset-new-documents",
      "settings-preset-markdown-preview",
      "settings-preset-spelling"
    )
    groupById(groups, "settings-preset-new-documents").children.map(_.id) shouldBe List("default-document-mode")
    groupById(groups, "settings-preset-markdown-preview").children.map(_.id) shouldBe List("markdown-view")
    groupById(groups, "settings-preset-spelling").children.map(_.id) shouldBe List(
      "spellcheck-enabled",
      "spellcheck-languages",
      "spellcheck-dictionaries",
      "spellcheck-words"
    )
  }
