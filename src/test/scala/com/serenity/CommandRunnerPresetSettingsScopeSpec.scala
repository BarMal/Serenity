package com.serenity

import com.serenity.command.*
import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.ui.presets.UiPreset
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #1682 part 1: what the preset pages show, and that no page or row appears twice in the settings tree. */
class CommandRunnerPresetSettingsScopeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry       = CommandRegistry.default
  given CommandRegistry      = registry
  private val PresetFontSize = 19.0f

  private val EditableGroupIds = List(
    "settings-preset-cursor",
    "settings-preset-prose-font",
    "settings-preset-code-font",
    "settings-preset-ui-font",
    "settings-preset-document-defaults",
    "settings-preset-spellcheck"
  )

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  private def allGroups(groups: List[CommandSurfaceItem.GroupItem]): List[CommandSurfaceItem.GroupItem] =
    groups.flatMap(group => group :: descendants(group).collect { case child: CommandSurfaceItem.GroupItem => child })

  private def groupNamed(groups: List[CommandSurfaceItem.GroupItem], id: String): CommandSurfaceItem.GroupItem =
    allGroups(groups).find(_.id == id).getOrElse(fail(s"missing group $id"))

  private def drafting: UiPreset =
    val base = AppConfig.default
    UiPreset(
      name = "Drafting",
      config = base.withFontConfig(base.editorConfig.fontConfig.copy(fontSize = PresetFontSize))
    )

  private def runnerEditing(
    config: AppConfig,
    editing: Option[String],
    presets: List[UiPreset]
  ): CommandRunner =
    CommandRunner.empty
      .activate(registry, config)
      .withUiPresetPreviews(presets.map(UiPreset.Preview.fromPreset))
      .copy(editingPresetName = editing)

  private val everythingShown = AppConfig.default.withShowAllSettingsRegardlessOfMode(true)

  "The settings tree" should "give every group a unique id, including the preset pages" in {
    val tree = runnerEditing(everythingShown, Some("Drafting"), List(drafting)).settingsGroups
    val ids  = allGroups(tree).map(_.id)

    ids.diff(ids.distinct) shouldBe Nil
  }

  it should "give every group a unique id with no preset selected for editing" in {
    val ids = allGroups(runnerEditing(everythingShown, None, Nil).settingsGroups).map(_.id)

    ids.diff(ids.distinct) shouldBe Nil
  }

  it should "give every group a unique id in prose mode" in {
    val prose = everythingShown.withAppMode(AppMode.Prose)
    val ids   = allGroups(runnerEditing(prose, Some("Drafting"), List(drafting)).settingsGroups).map(_.id)

    ids.diff(ids.distinct) shouldBe Nil
  }

  it should "give every row outside the preset pages a unique id" in {
    val tree = runnerEditing(everythingShown, Some("Drafting"), List(drafting)).settingsGroups
    val rows = tree.filterNot(_.id == "settings-workspace") ++
      groupNamed(tree, "settings-workspace").children.collect {
        case group: CommandSurfaceItem.GroupItem if group.id != "settings-ui-presets" => group
      }
    val ids =
      rows.flatMap(group => descendants(group).filterNot(_.isInstanceOf[CommandSurfaceItem.GroupItem])).map(_.id)

    ids.diff(ids.distinct) shouldBe Nil
  }

  "Save As New" should "be the whole of the create page, with no copy of the editable settings" in {
    val tree   = runnerEditing(everythingShown, Some("Drafting"), List(drafting)).settingsGroups
    val create = groupNamed(tree, "settings-preset-create")

    create.label shouldBe "Save As New Preset"
    create.children.map(_.id) shouldBe List("ui-preset-save-as-new")
  }

  "The edit page" should "offer only the settings a preset can edit, after its name and actions" in {
    val tree = runnerEditing(everythingShown, Some("Drafting"), List(drafting)).settingsGroups
    val edit = groupNamed(tree, "settings-preset-edit")

    edit.children.map(_.id) shouldBe List("settings-preset-name", "settings-preset-actions") ++ EditableGroupIds
    allGroups(tree).map(_.id) should not contain "settings-preset-workspace-layout"
  }

  it should "point to Overwrite Preset for the panel layout" in {
    val tree = runnerEditing(everythingShown, Some("Drafting"), List(drafting)).settingsGroups
    val edit = groupNamed(tree, "settings-preset-edit")

    edit.hint.getOrElse("") should include("Overwrite Preset")
  }

  it should "show the edited preset's values, not the live settings" in {
    val tree = runnerEditing(everythingShown, Some("Drafting"), List(drafting)).settingsGroups
    def sizeOn(id: String): String =
      descendants(groupNamed(tree, id))
        .collectFirst {
          case item: CommandSurfaceItem.InputItem if item.id.endsWith("code-font-size") => item.currentValue
        }
        .getOrElse(fail(s"missing code font size on $id"))

    sizeOn("settings-code-font") shouldBe AppConfig.default.editorConfig.fontConfig.codeFontSize.toString
    sizeOn("settings-preset-code-font") shouldBe drafting.config.editorConfig.fontConfig.codeFontSize.toString
  }

  it should "show a built-in preset's own values" in {
    val writing = UiPreset.builtIn("Writing").getOrElse(fail("Writing is a built-in preset"))
    val tree    = runnerEditing(everythingShown, Some("Writing"), Nil).settingsGroups
    val shown = descendants(groupNamed(tree, "settings-preset-document-defaults")).collectFirst {
      case item: CommandSurfaceItem.OptionItem if item.id.endsWith("default-document-mode") => item.selectedOption
    }

    shown shouldBe Some(
      Map(
        DefaultDocumentMode.PlainText -> "Plain Text",
        DefaultDocumentMode.Markdown  -> "Markdown",
        DefaultDocumentMode.RichText  -> "Rich Text"
      )(writing.config.defaultDocumentMode)
    )
  }

  it should "keep a row's choice separate from the same row on the global page" in {
    val runner = runnerEditing(everythingShown, Some("Drafting"), List(drafting))
    val ids = allGroups(runner.settingsGroups)
      .filter(group => group.id == "settings-code-font" || group.id == "settings-preset-code-font")
      .flatMap(group => descendants(group).filterNot(_.isInstanceOf[CommandSurfaceItem.GroupItem]))
      .collect { case item: CommandSurfaceItem.InputItem => item.id }

    ids.distinct should have size 2
  }

  "Settings search" should "never offer a preset page's copy of a setting" in {
    val runner = runnerEditing(everythingShown, Some("Drafting"), List(drafting)).updateSearchTerm("code font size")
    val targets = runner.visibleItems.collect {
      case result: CommandSurfaceItem.SettingSearchItem => result.targetItemId
    }

    targets.filter(_.endsWith("code-font-size")) shouldBe List("code-font-size")
  }
