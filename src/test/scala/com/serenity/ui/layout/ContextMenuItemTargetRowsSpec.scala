package com.serenity.ui.layout

import com.serenity.command.{Command, CommandIntent, EditIntent}
import com.serenity.config.{InterfaceDensity, InterfaceDensityMetrics}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389 (owner decision, 2026-10-31): a context-menu item spends one row at Comfortable, as it does at Compact,
  * so the pitch equals the hit region and no dead row sits between items. Spacious keeps its larger pitch. The other
  * surfaces that share `itemTargetRowsFor` keep the contract's minimum target rows.
  */
class ContextMenuItemTargetRowsSpec extends AnyFlatSpec with Matchers:

  private def command(label: String): ContextMenuItem =
    val c = Command.typed(label, label, CommandIntent.Edit(EditIntent.Copy), label = label)
    ContextMenuItem(c.name, label, c)

  private val menu = ContextMenu(
    title = "editor",
    targetFocus = Focus.EditorPane(PaneId(0)),
    items = List(command("Save"), command("Cut"), command("Copy")),
    selectedIndex = 0
  )

  private val menuContent = SurfaceContent.ContextMenu(menu)

  private def pitch(density: InterfaceDensity): Double =
    SurfaceFrameLayout.itemTargetRowsFor(menuContent, density) + InterfaceDensityMetrics.forDensity(density).itemGapRows

  "SurfaceFrameLayout.itemTargetRowsFor" should "give a context menu item one row at Compact and at Comfortable" in {
    SurfaceFrameLayout.itemTargetRowsFor(menuContent, InterfaceDensity.Compact) shouldBe 1
    SurfaceFrameLayout.itemTargetRowsFor(menuContent, InterfaceDensity.Comfortable) shouldBe 1
  }

  it should "leave a context menu's Spacious pitch as it is: two rows and a one-row gap" in {
    SurfaceFrameLayout.itemTargetRowsFor(menuContent, InterfaceDensity.Spacious) shouldBe 2
    pitch(InterfaceDensity.Spacious) shouldBe 3.0
  }

  it should "make a context menu's Comfortable pitch exactly one row" in {
    pitch(InterfaceDensity.Comfortable) shouldBe 1.0
  }

  it should "keep the minimum target rows for the surfaces that are not context menus" in {
    val palette = SurfaceContent.CommandPalette(com.serenity.command.CommandRunner.empty)
    List(InterfaceDensity.Compact, InterfaceDensity.Comfortable, InterfaceDensity.Spacious).foreach { density =>
      withClue(density.toString) {
        SurfaceFrameLayout.itemTargetRowsFor(palette, density) shouldBe SurfaceFrameLayout.minimumTargetRows(density)
      }
    }
    SurfaceFrameLayout.itemTargetRowsFor(palette, InterfaceDensity.Comfortable) shouldBe 2
  }

  "ContextMenuSurfaceComposition at the Comfortable pitch" should "stack the items on adjacent rows, every row hittable" in {
    val resolved = ContextMenuSurfaceComposition.forMenu(
      menu,
      LayoutRect(0, 0, 28, 10),
      itemGapRows = InterfaceDensityMetrics.forDensity(InterfaceDensity.Comfortable).itemGapRows,
      itemTargetRows = SurfaceFrameLayout.itemTargetRowsFor(menuContent, InterfaceDensity.Comfortable)
    )
    val items = resolved.paintBoxes.filter(_.kind == SurfacePaintKind.ActionItem)

    items.map(_.rect.y).sliding(2).collect { case Seq(a, b) => b - a }.toList shouldBe List(1.0, 1.0)
    items.foreach(item => resolved.hitAt(item.rect.x, item.rect.y).flatMap(_.actionId) shouldBe item.actionId)
  }
