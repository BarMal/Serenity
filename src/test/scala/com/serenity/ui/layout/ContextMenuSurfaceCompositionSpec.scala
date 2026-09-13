package com.serenity.ui.layout

import com.serenity.command.{Command, CommandIntent, FileIntent}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `ContextMenuSurfaceComposition` (issue #819, slice 2): the editor's right-click context menu resolved
  * into one paint/hit-test/focus plan, the same way `ModalSurfaceComposition` already does for blocking workflows. Both
  * painting and hit-testing are derived from the same `SurfaceFrameLayout.contentRowSlotsFor` row positions the
  * pre-migration renderer/hit-tester separately called, so the two can never disagree about where a row sits.
  */
class ContextMenuSurfaceCompositionSpec extends AnyFlatSpec with Matchers:

  private val save = Command.typed("save", "Save file", CommandIntent.File(FileIntent.SaveCurrentFile), label = "Save")
  private val cut  = Command.typed("cut", "Cut", CommandIntent.Edit(com.serenity.command.EditIntent.Cut), label = "Cut")
  private val copyIt =
    Command.typed("copy", "Copy", CommandIntent.Edit(com.serenity.command.EditIntent.Copy), label = "Copy")

  private def menu(items: List[Command], selectedIndex: Int = 0): ContextMenu =
    ContextMenu(
      title = "editor",
      targetFocus = Focus.EditorPane(PaneId(0)),
      items = items.map(c => ContextMenuItem(c.name, c.label, c)),
      selectedIndex = selectedIndex
    )

  "forMenu" should "paint the menu title as a non-interactive header row" in {
    val resolved = ContextMenuSurfaceComposition.forMenu(menu(List(save)), LayoutRect(0, 0, 28, 8))

    val header = resolved.paintBoxes.find(_.kind == SurfacePaintKind.Text)
    header.flatMap(_.text) shouldBe Some("editor")
    header.flatMap(_.focusId) shouldBe None
    header.flatMap(_.actionId) shouldBe None
  }

  it should "paint one selectable ActionItem box per menu item, addressed and labelled by item index" in {
    val resolved =
      ContextMenuSurfaceComposition.forMenu(menu(List(save, cut, copyIt), selectedIndex = 1), LayoutRect(0, 0, 28, 8))

    val itemBoxes = resolved.paintBoxes.filter(_.kind == SurfacePaintKind.ActionItem)
    itemBoxes.map(_.text) shouldBe List(Some("Save"), Some("Cut"), Some("Copy"))
    itemBoxes.map(_.focusId) shouldBe List(
      Some(SurfaceFocusId("context-menu-item-0")),
      Some(SurfaceFocusId("context-menu-item-1")),
      Some(SurfaceFocusId("context-menu-item-2"))
    )
    itemBoxes.map(_.actionId) shouldBe List(
      Some(SurfaceActionId("context-menu-item-0")),
      Some(SurfaceActionId("context-menu-item-1")),
      Some(SurfaceActionId("context-menu-item-2"))
    )
    itemBoxes.map(_.selected) shouldBe List(false, true, false)
  }

  it should "render a selection-count footer only when the menu has items" in {
    val withItems = ContextMenuSurfaceComposition.forMenu(menu(List(save, cut)), LayoutRect(0, 0, 28, 8))
    val empty = ContextMenuSurfaceComposition.forMenu(
      ContextMenu(title = "empty", targetFocus = Focus.EditorPane(PaneId(0)), items = Nil),
      LayoutRect(0, 0, 28, 8)
    )

    withItems.paintBoxes.find(_.text.contains("1/2")) shouldBe defined
    empty.paintBoxes.filter(_.kind == SurfacePaintKind.ActionItem) shouldBe Nil
    empty.paintBoxes.exists(_.text.exists(_.contains("/"))) shouldBe false
  }

  it should "resolve hitAt to the exact item clicked, matching what was painted" in {
    val resolved = ContextMenuSurfaceComposition.forMenu(menu(List(save, cut, copyIt)), LayoutRect(0, 0, 28, 8))

    val itemBox = resolved.paintBoxes.find(_.text.contains("Cut")).get
    val hit     = resolved.hitAt(itemBox.rect.x, itemBox.rect.y)

    hit.map(_.focusId) shouldBe Some(SurfaceFocusId("context-menu-item-1"))
    hit.flatMap(_.actionId) shouldBe Some(SurfaceActionId("context-menu-item-1"))
    hit.map(_.semanticLabel) shouldBe Some("Cut")
  }

  it should "expose a focus order matching on-screen item order" in {
    val resolved = ContextMenuSurfaceComposition.forMenu(menu(List(save, cut, copyIt)), LayoutRect(0, 0, 28, 8))

    resolved.focusOrder shouldBe List(
      SurfaceFocusId("context-menu-item-0"),
      SurfaceFocusId("context-menu-item-1"),
      SurfaceFocusId("context-menu-item-2")
    )
  }

  it should "windows a menu taller than its frame, keeping the selected item visible and its index absolute" in {
    val items = (0 until 10)
      .map(i =>
        Command
          .typed(s"cmd-$i", s"Item $i", CommandIntent.Edit(com.serenity.command.EditIntent.Copy), label = s"Item $i")
      )
      .toList
    val resolved = ContextMenuSurfaceComposition.forMenu(menu(items, selectedIndex = 9), LayoutRect(0, 0, 28, 8))

    val itemBoxes = resolved.paintBoxes.filter(_.kind == SurfacePaintKind.ActionItem)
    itemBoxes.map(_.text) should contain(Some("Item 9"))
    itemBoxes.find(_.selected).flatMap(_.text) shouldBe Some("Item 9")
    itemBoxes.find(_.selected).flatMap(_.focusId) shouldBe Some(SurfaceFocusId("context-menu-item-9"))
  }

  it should "leave a genuine, un-hittable gap row between items at non-default itemTargetRows/itemGapRows, matching what the pre-migration hit-tester expected" in {
    val resolved = ContextMenuSurfaceComposition.forMenu(
      menu(List(save, cut, copyIt)),
      LayoutRect(0, 0, 28, 10),
      itemGapRows = 1.0,
      itemTargetRows = 2
    )

    val itemBoxes  = resolved.paintBoxes.filter(_.kind == SurfacePaintKind.ActionItem)
    val firstRowY  = itemBoxes.headOption.map(_.rect.y).getOrElse(fail("expected a first item row"))
    val secondRowY = itemBoxes.lift(1).map(_.rect.y).getOrElse(fail("expected a second item row"))

    // itemHeight = itemTargetRows(2) + itemGapRows(1) = 3: the row directly between two item rows is a real gap,
    // not a third item -- hitAt there must resolve to nothing, matching `isContextMenuItemGap`'s contract.
    secondRowY - firstRowY shouldBe 3.0
    resolved.hitAt(itemBoxes.head.rect.x, firstRowY + 1) shouldBe None
  }
