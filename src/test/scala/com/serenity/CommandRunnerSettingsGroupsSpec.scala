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
      "settings-panel-pins",
      "settings-panel-actions"
    )
    groupById(groups, "settings-document-writing").children.map(_.id) shouldBe List(
      "settings-navigation",
      "settings-language",
      "settings-markdown",
      "settings-rich-text",
      "settings-spellcheck"
    )
    groupById(groups, "settings-interface-layout").children.map(_.id) shouldBe List(
      "interface-density",
      "ui-element-gap",
      "ui-corner-radius",
      "command-runner-visible-rows"
    )
    groupById(groups, "settings-cursor").children.map(_.id) shouldBe List(
      "cursor-mode",
      "cursor-info-bar",
      "cursor-info-bar-placement"
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
