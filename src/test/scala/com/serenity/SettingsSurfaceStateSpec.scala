package com.serenity

import com.serenity.command.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** TDD contract for the new page-stack types introduced for issue #1059, in isolation from the existing
  * `CommandRunner`/`CommandRunnerSubmenuState` state machine they will eventually replace. Nothing here is wired up to
  * `CommandRunnerReducer` yet -- that migration is a later stage.
  */
class SettingsSurfaceStateSpec extends AnyFlatSpec with Matchers:

  private val root: SettingsPage.Group       = SettingsPage.Group(groupId = "settings-root", selectedIndex = 2)
  private val child: SettingsPage.Group      = SettingsPage.Group(groupId = "settings-child", selectedIndex = 0)
  private val grandchild: SettingsPage.Group = SettingsPage.Group(groupId = "settings-grandchild", selectedIndex = 1)

  "SettingsSurfaceState construction" should "reject an empty stack" in {
    an[IllegalArgumentException] should be thrownBy SettingsSurfaceState(Nil)
  }

  it should "start as a single-page stack rooted at the given page" in {
    val state = SettingsSurfaceState(root)
    state.stack shouldBe List(root)
    state.current shouldBe root
    state.depth shouldBe 1
  }

  "push" should "add a page above the current one without disturbing ancestors" in {
    val state = SettingsSurfaceState(root).push(child).push(grandchild)
    state.stack shouldBe List(grandchild, child, root)
    state.current shouldBe grandchild
    state.depth shouldBe 3
  }

  "pop" should "reveal the parent page and drop the popped one" in {
    val state  = SettingsSurfaceState(root).push(child)
    val popped = state.pop
    popped shouldBe Some(SettingsSurfaceState(List(root)))
  }

  it should "return None at the root, where there is nothing left to pop" in {
    SettingsSurfaceState(root).pop shouldBe None
  }

  it should "be a true inverse of push at any depth" in {
    val state = SettingsSurfaceState(root).push(child).push(grandchild)
    state.pop.flatMap(_.pop) shouldBe Some(SettingsSurfaceState(List(root)))
  }

  "SettingsSurfaceState.escape" should "pop one page when depth > 1" in {
    val state = SettingsSurfaceState(root).push(child)
    SettingsSurfaceState.escape(state) shouldBe SettingsSurfaceState.EscapeOutcome.Popped(
      SettingsSurfaceState(List(root))
    )
  }

  it should "signal CloseSurface at the root instead of popping" in {
    val state = SettingsSurfaceState(root)
    SettingsSurfaceState.escape(state) shouldBe SettingsSurfaceState.EscapeOutcome.CloseSurface
  }

  it should "never touch a page's text -- only navigate" in {
    val withSearch = SettingsSurfaceState(root.copy(searchTerm = "font"))
    val popped     = SettingsSurfaceState.escape(withSearch.push(child))
    popped shouldBe SettingsSurfaceState.EscapeOutcome.Popped(withSearch)
  }

  "SettingsSurfaceState.deleteBackward" should "drop the last character of a Group page's search term" in {
    val state    = SettingsSurfaceState(root.copy(searchTerm = "font"))
    val expected = state.copy(current = root.copy(searchTerm = "fon"))
    SettingsSurfaceState.deleteBackward(state) shouldBe expected
  }

  it should "drop the last character of an Editing page's draft text" in {
    val editing: SettingsPage.Editing =
      SettingsPage.Editing(groupId = "settings-child", itemId = "blur-radius", draftText = "12")
    val state  = SettingsSurfaceState(editing)
    val result = SettingsSurfaceState.deleteBackward(state)
    result.current shouldBe editing.copy(draftText = "1")
  }

  it should "be a no-op on a Group page with no search term" in {
    val state = SettingsSurfaceState(root)
    SettingsSurfaceState.deleteBackward(state) shouldBe state
  }

  it should "be a no-op on an Editing page with empty draft text" in {
    val editing = SettingsPage.Editing(groupId = "settings-child", itemId = "blur-radius", draftText = "")
    val state   = SettingsSurfaceState(editing)
    SettingsSurfaceState.deleteBackward(state) shouldBe state
  }

  it should "never pop the stack, even at the root with an empty page" in {
    val state = SettingsSurfaceState(root).push(child)
    SettingsSurfaceState.deleteBackward(state).depth shouldBe 2
  }

  it should "only ever affect the top page, leaving ancestors untouched" in {
    val state  = SettingsSurfaceState(root.copy(searchTerm = "ab")).push(child.copy(searchTerm = "xy"))
    val result = SettingsSurfaceState.deleteBackward(state)
    result.stack shouldBe List(child.copy(searchTerm = "x"), root.copy(searchTerm = "ab"))
  }

  "SettingsSurfaceState.previewRows" should "be empty when the selected item is not a group" in {
    val items = List(
      CommandSurfaceItem.OptionItem(
        id = "opt",
        label = "Option",
        options = List(CommandOption("A", CommandIntent.View(ViewIntent.NextTab))),
        selectedIndex = 0,
        category = CommandCategory.Settings
      )
    )
    SettingsSurfaceState.previewRows(items, 0) shouldBe SettingsSurfaceState.PreviewRows(Nil, 0)
  }

  it should "be empty when selectedIndex is out of bounds" in {
    SettingsSurfaceState.previewRows(Nil, 0) shouldBe SettingsSurfaceState.PreviewRows(Nil, 0)
  }

  it should "list every child's label when there are 4 or fewer" in {
    val group = CommandSurfaceItem.GroupItem(
      id = "grp",
      label = "Group",
      category = CommandCategory.Settings,
      children = List(optionNamed("One"), optionNamed("Two"), optionNamed("Three"))
    )
    val result = SettingsSurfaceState.previewRows(List(group), 0)
    result.rows shouldBe List("One", "Two", "Three")
    result.overflowCount shouldBe 0
    result.isEmpty shouldBe false
  }

  it should "cap at 4 rows and report the overflow count beyond that" in {
    val labels = List("One", "Two", "Three", "Four", "Five", "Six")
    val group = CommandSurfaceItem.GroupItem(
      id = "grp",
      label = "Group",
      category = CommandCategory.Settings,
      children = labels.map(optionNamed)
    )
    val result = SettingsSurfaceState.previewRows(List(group), 0)
    result.rows shouldBe List("One", "Two", "Three", "Four")
    result.overflowCount shouldBe 2
  }

  it should "derive purely from items.lift(selectedIndex), carrying no state of its own" in {
    val group = CommandSurfaceItem.GroupItem(
      id = "grp",
      label = "Group",
      category = CommandCategory.Settings,
      children = List(optionNamed("Solo"))
    )
    val items = List(optionNamed("Before"), group, optionNamed("After"))
    SettingsSurfaceState.previewRows(items, 1) shouldBe SettingsSurfaceState.PreviewRows(List("Solo"), 0)
    SettingsSurfaceState.previewRows(items, 0) shouldBe SettingsSurfaceState.PreviewRows(Nil, 0)
  }

  private def optionNamed(label: String): CommandSurfaceItem.OptionItem =
    CommandSurfaceItem.OptionItem(
      id = label,
      label = label,
      options = List(CommandOption(label, CommandIntent.View(ViewIntent.NextTab))),
      selectedIndex = 0,
      category = CommandCategory.Settings
    )
