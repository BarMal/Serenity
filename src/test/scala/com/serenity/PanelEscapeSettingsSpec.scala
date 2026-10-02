package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.rope.Balance
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Where Escape from a focused panel returns focus is set per app mode, so the Panels settings group shows the current
  * mode's row -- both rows once Show All Settings is on.
  */
class PanelEscapeSettingsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val escapeRowIds = List("panel-escape-code", "panel-escape-prose")

  private def panelsGroup(config: AppConfig): CommandSurfaceItem.GroupItem =
    given registry: CommandRegistry = CommandRegistry.default
    val runner                      = CommandRunner.empty.activate(registry, config).openSettings
    def walk(items: List[CommandSurfaceItem]): List[CommandSurfaceItem.GroupItem] =
      items.collect { case group: CommandSurfaceItem.GroupItem => group :: walk(group.children) }.flatten
    walk(runner.settingsGroups)
      .find(_.id == "settings-workspace-layout")
      .getOrElse(fail("missing the Panels settings group"))

  private def escapeRows(config: AppConfig): List[CommandSurfaceItem.OptionItem] =
    panelsGroup(config).children.collect {
      case item: CommandSurfaceItem.OptionItem if escapeRowIds.contains(item.id) => item
    }

  private def escapeRow(config: AppConfig, id: String): CommandSurfaceItem.OptionItem =
    escapeRows(config).find(_.id == id).getOrElse(fail(s"missing row $id"))

  private def setTarget(mode: AppMode, target: PanelEscapeTarget): CommandIntent =
    CommandIntent.Settings(SettingsIntent.InterfaceChrome(InterfaceChromeIntent.SetPanelEscapeTarget(mode, target)))

  "The Panels settings group" should "show only the code mode's Escape target in a code workspace" in {
    escapeRows(AppConfig.default.withAppMode(AppMode.Code)).map(_.id) shouldBe List("panel-escape-code")
  }

  it should "show only the prose mode's Escape target in a prose workspace" in {
    escapeRows(AppConfig.default.withAppMode(AppMode.Prose)).map(_.id) shouldBe List("panel-escape-prose")
  }

  it should "show both modes' Escape targets when showing all settings" in {
    escapeRows(AppConfig.default.withShowAllSettingsRegardlessOfMode(true)).map(_.id) shouldBe escapeRowIds
  }

  it should "select each mode's own configured target" in {
    val config = AppConfig.default
      .withShowAllSettingsRegardlessOfMode(true)
      .withPanelEscapeTarget(AppMode.Prose, PanelEscapeTarget.Previous)

    escapeRow(config, "panel-escape-code").selectedOption shouldBe "Editor"
    escapeRow(config, "panel-escape-prose").selectedOption shouldBe "Previous Focus"
  }

  it should "change only the row's own mode" in {
    val config = AppConfig.default.withShowAllSettingsRegardlessOfMode(true)

    escapeRow(config, "panel-escape-code").options.map(_.intent) shouldBe List(
      setTarget(AppMode.Code, PanelEscapeTarget.Editor),
      setTarget(AppMode.Code, PanelEscapeTarget.Previous)
    )
    escapeRow(config, "panel-escape-prose").options.map(_.intent) shouldBe List(
      setTarget(AppMode.Prose, PanelEscapeTarget.Editor),
      setTarget(AppMode.Prose, PanelEscapeTarget.Previous)
    )
  }

  it should "keep the preset editor's Panels group to arranging panels" in {
    given registry: CommandRegistry = CommandRegistry.default
    val runner = CommandRunner.empty.activate(registry, AppConfig.default.withShowAllSettingsRegardlessOfMode(true))
    def walk(items: List[CommandSurfaceItem]): List[CommandSurfaceItem] =
      items.flatMap {
        case group: CommandSurfaceItem.GroupItem => group :: walk(group.children)
        case item                                => List(item)
      }
    val presetPanels = walk(runner.settingsGroups)
      .collectFirst {
        case group: CommandSurfaceItem.GroupItem if group.id == "settings-preset-workspace-layout" => group
      }
      .getOrElse(fail("missing the preset Panels group"))

    presetPanels.children.map(_.id) shouldBe List("arrange-panels")
  }
