package com.serenity

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendCapabilities
import com.serenity.ui.fonts.FontLoader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #1682 part 2: UI Presets sits beside Workspace, and its Edit page is flat with carousel font pickers. */
class CommandRunnerUiPresetsMenuShapeSpec extends AnyFlatSpec with Matchers:

  private val catalog = FontLoader.FontFamilyCatalog(
    monospace = List("Mono", "Fira"),
    text = List("Serif", "Sans"),
    ui = List("Inter", "Roboto")
  )

  private val tree: List[CommandSurfaceItem.GroupItem] =
    CommandRunnerSettingsGroups.build(
      Map("settings-show-all" -> 0),
      CommandRunnerSettingsInputItems.build(AppConfig.default, FrontendCapabilities.gui),
      Nil,
      None,
      FrontendCapabilities.gui,
      catalog
    )

  private def group(id: String): CommandSurfaceItem.GroupItem =
    tree.find(_.id == id).getOrElse(fail(s"missing top-level group $id"))

  private def everyItem(items: List[CommandSurfaceItem]): List[CommandSurfaceItem] =
    items.flatMap {
      case parent: CommandSurfaceItem.GroupItem => parent :: everyItem(parent.children)
      case leaf                                 => List(leaf)
    }

  private def depth(item: CommandSurfaceItem): Int =
    item match
      case parent: CommandSurfaceItem.GroupItem => 1 + parent.children.map(depth).maxOption.getOrElse(0)
      case _                                    => 1

  private def familyCarousel(id: String): CommandSurfaceItem.OptionItem =
    group("settings-ui-presets").children
      .collect { case parent: CommandSurfaceItem.GroupItem if parent.id == "settings-preset-edit" => parent }
      .flatMap(_.children)
      .collectFirst { case option: CommandSurfaceItem.OptionItem if option.id == id => option }
      .getOrElse(fail(s"missing carousel $id"))

  "The settings menu" should "list UI Presets as a top-level group right after Workspace" in {
    tree.map(_.id).take(3) shouldBe List("settings-workspace", "settings-ui-presets", "settings-editor")
    group("settings-ui-presets").label shouldBe "UI Presets"
    everyItem(group("settings-workspace").children).map(_.id) should not contain "settings-ui-presets"
  }

  it should "keep UI Presets at most three levels deep, counting the group itself" in {
    depth(group("settings-ui-presets")) should be <= 3
  }

  it should "give every group and row in the whole tree a unique id" in {
    val ids = everyItem(tree).map(_.id)
    ids.diff(ids.distinct) shouldBe Nil
  }

  "The Edit Preset page" should "hold no nested groups" in {
    val edit = everyItem(group("settings-ui-presets").children).collectFirst {
      case parent: CommandSurfaceItem.GroupItem if parent.id == "settings-preset-edit" => parent
    }
    edit.map(_.children.collect { case nested: CommandSurfaceItem.GroupItem => nested.id }) shouldBe Some(Nil)
  }

  it should "show each font family as an inline carousel that edits that preset" in {
    val textFamilies = familyCarousel("preset:Writing:text-font")
    textFamilies.options.map(_.label) shouldBe List("Serif", "Sans")
    textFamilies.options.map(_.intent) shouldBe List("Serif", "Sans").map(family =>
      CommandIntent.Scoped(
        SettingsTarget.Preset("Writing"),
        CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetTextFontFamily(family)))
      )
    )
    familyCarousel("preset:Writing:code-font").options.map(_.label) shouldBe List("Mono", "Fira")
    familyCarousel("preset:Writing:ui-font").options.map(_.label) shouldBe List("Inter", "Roboto")
  }
