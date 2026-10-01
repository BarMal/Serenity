package com.serenity

import com.serenity.command.*
import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendCapabilities
import com.serenity.ui.fonts.FontLoader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Which settings rows each frontend shows: rows with no visible effect in a terminal are hidden there, and come back
  * led by an inert note when Show All Settings is on.
  */
class CommandRunnerSettingsFrontendSpec extends AnyFlatSpec with Matchers:

  private def descendants(group: CommandSurfaceItem.GroupItem): List[CommandSurfaceItem] =
    group.children.flatMap {
      case child: CommandSurfaceItem.GroupItem => child :: descendants(child)
      case child                               => List(child)
    }

  private val guiOnlyRowIds = List(
    "window-chrome",
    "background-style",
    "post-processing",
    "ui-shadows",
    "blur-radius",
    "ui-corner-radius",
    "ui-outline-thickness",
    "settings-prose-font",
    "settings-code-font",
    "settings-ui-font",
    "settings-text-scale",
    "rich-text-font-family",
    "rich-text-font-size",
    "panel-open-transition",
    "panel-close-transition",
    "panel-markdown-preview-pin"
  )

  private val proseMode   = "app-mode"          -> 1
  private val showAllOn   = "settings-show-all" -> 0
  private val testCatalog = FontLoader.FontFamilyCatalog(monospace = List("Mono"), text = List("Serif"), ui = Nil)

  private def treeOn(capabilities: FrontendCapabilities, selections: (String, Int)*) =
    CommandRunnerSettingsGroups.build(
      selections.toMap,
      CommandRunnerSettingsInputItems.build(AppConfig.default, capabilities),
      Nil,
      None,
      capabilities,
      testCatalog
    )

  private def allRows(groups: List[CommandSurfaceItem.GroupItem]): List[CommandSurfaceItem] =
    groups.flatMap(group => group :: descendants(group))

  private def rowHint(groups: List[CommandSurfaceItem.GroupItem], id: String): Option[String] =
    allRows(groups).find(_.id == id).getOrElse(fail(s"missing row $id")) match
      case item: CommandSurfaceItem.OptionItem => item.hint
      case item: CommandSurfaceItem.InputItem  => Some(item.hint)
      case item: CommandSurfaceItem.GroupItem  => item.hint
      case other                               => fail(s"unexpected row kind $other")

  "CommandRunnerSettingsGroups.build" should "hide every GUI-only row on a TUI frontend" in {
    val rowIds = allRows(treeOn(FrontendCapabilities.tui(), proseMode)).map(_.id).toSet

    guiOnlyRowIds.filter(rowIds.contains) shouldBe empty
    rowIds should contain allOf ("rich-text-color", "material-preset", "interface-density", "ui-element-gap")
  }

  it should "drop the Typography group in a code workspace on a TUI frontend, where none of its rows apply" in {
    allRows(treeOn(FrontendCapabilities.tui())).map(_.id) should not contain "settings-typography"
  }

  it should "show GUI-only rows on a TUI frontend when Show All Settings is on, each led by an inert note" in {
    val tree = treeOn(FrontendCapabilities.tui(), proseMode, showAllOn)

    guiOnlyRowIds.foreach(id => rowHint(tree, id).getOrElse("") should startWith("Inert in TUI mode -- "))
    rowHint(tree, "settings-prose-font") shouldBe Some("Inert in TUI mode -- Family, size, ligatures")
    rowHint(tree, "material-preset").getOrElse("") should not include "Inert in TUI mode"
    rowHint(tree, "settings-typography") shouldBe Some("Typefaces for prose, code, and interface")
  }

  it should "show every row unannotated on a GUI frontend" in {
    val tree = treeOn(FrontendCapabilities.gui, showAllOn)

    guiOnlyRowIds.foreach(id => rowHint(tree, id).getOrElse("") should not include "Inert in TUI mode")
  }

  it should "hide the GUI-only font groups from a preset being edited on a TUI frontend" in {
    val presetIds = allRows(treeOn(FrontendCapabilities.tui(), proseMode)).map(_.id).toSet

    presetIds should contain("settings-preset-cursor")
    presetIds should not contain "settings-preset-prose-font"
  }
