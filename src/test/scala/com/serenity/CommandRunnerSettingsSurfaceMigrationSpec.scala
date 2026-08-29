package com.serenity

import com.serenity.command.*
import com.serenity.config.AppConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** TDD contract for the settings-relevant `CommandRunner` methods re-platformed onto `activeSettingsSurface` (issue
  * #1059's page stack), migrated alongside their pre-existing `activeSubmenu` behavior. Every case below asserts the
  * new field's shape and, where useful, cross-checks it against the old field it mirrors -- both are still written by
  * these methods (see the field's doc on `CommandRunner`), so this is a faithfulness check on the re-platforming, not a
  * behavior change. `CommandRunnerReducer`/`SurfaceContentResolver` are not touched or exercised here -- they still
  * read only `activeSubmenu`, unchanged.
  *
  * Setup chains below use only the nine migrated methods (`moveSubmenuSelection` in particular, rather than
  * `withSelectedFocusedSubmenuIndex`) whenever a scenario needs `activeSettingsSurface` to be trustworthy --
  * `withSelectedFocusedSubmenuIndex` is not migrated this turn, so it would silently leave the new field stale.
  */
class CommandRunnerSettingsSurfaceMigrationSpec extends AnyFlatSpec with Matchers:

  private val registry = CommandRegistry.default

  private def opened: CommandRunner =
    CommandRunner.empty.activate(registry, AppConfig.default).openSettings

  "openSettings" should "leave no active settings-surface stack" in {
    opened.activeSettingsSurface shouldBe None
  }

  "enterSelectedGroup" should "push a single-page stack, with no ancestors, for a top-level group" in {
    val entered = opened.withSelectedItem("settings-document-writing").enterSelectedGroup

    entered.activeSettingsSurface shouldBe Some(SettingsSurfaceState(SettingsPage.Group("settings-document-writing")))
    entered.activeSettingsSurface.map(_.current.groupId) shouldBe entered.activeSubmenu.map(_.groupId)
  }

  it should "build ancestor pages matching the preferred ancestor path when jumping to a search leaf" in {
    given CommandRegistry = registry
    val searched          = opened.updateSettingsSearch("default document")
    val leafIndex         = searched.visibleItems.indexWhere(_.isInstanceOf[CommandSurfaceItem.SettingSearchItem])
    leafIndex should be >= 0
    val entered = searched.withSelectedVisibleIndex(leafIndex).enterSelectedGroup

    entered.activeSettingsSurface.map(_.current.groupId) shouldBe Some("settings-document-defaults")
    entered.activeSettingsSurface.map(_.ancestors.map(_.groupId)) shouldBe Some(List("settings-document-writing"))
    entered.activeSubmenu.map(_.ancestorGroupIds) shouldBe Some(List("settings-document-writing"))
  }

  "enterSelectedSubmenuGroup" should "push, making the left group (at its current index) the nearest ancestor" in {
    val level1 = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup.moveSubmenuSelection(1)

    val level2 = level1.enterSelectedSubmenuGroup

    level2.activeSettingsSurface.map(_.current.groupId) shouldBe Some("settings-surface-appearance")
    level2.activeSettingsSurface.map(_.ancestors) shouldBe Some(
      List(SettingsPage.Group("settings-appearance-motion", 1))
    )
    level2.activeSubmenu.map(s => s.groupId -> s.ancestorGroupIds) shouldBe
      Some("settings-surface-appearance" -> List("settings-appearance-motion"))
  }

  "exitSubmenuToPreview" should "pop and re-point the parent page at the child just left" in {
    val level2 = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup

    val back = level2.exitSubmenuToPreview

    back.activeSettingsSurface shouldBe Some(SettingsSurfaceState(SettingsPage.Group("settings-appearance-motion", 1)))
    back.activeSubmenu.map(s => s.groupId -> s.selectedIndex) shouldBe Some("settings-appearance-motion" -> 1)
  }

  it should "close the whole stack when there is no parent to reveal" in {
    val level1 = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup

    val back = level1.exitSubmenuToPreview

    back.activeSettingsSurface shouldBe None
    back.activeSubmenu shouldBe None
  }

  "moveSubmenuSelection" should "advance the current page's index" in {
    val level1 = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup

    val moved = level1.moveSubmenuSelection(1)

    moved.activeSettingsSurface shouldBe Some(SettingsSurfaceState(SettingsPage.Group("settings-appearance-motion", 1)))
    moved.activeSubmenu.map(_.selectedIndex) shouldBe Some(1)
  }

  it should "drop an Editing page back to Group, mirroring the old model always exiting edit mode" in {
    val editing = editingBlurRadius // selectedIndex 4 of 5 items

    val moved = editing.moveSubmenuSelection(1) // wraps: (4 + 1) % 5 == 0

    moved.activeSettingsSurface.map(_.current) shouldBe Some(SettingsPage.Group("settings-surface-appearance", 0))
  }

  "beginSubmenuEditMode" should "replace the current page with an Editing page carrying the item's value" in {
    val level2 = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup
      .moveSubmenuSelection(4)
    val currentValue = level2
      .submenuItems("settings-surface-appearance")
      .collectFirst { case item: CommandSurfaceItem.InputItem if item.id == "blur-radius" => item.currentValue }
      .getOrElse(fail("Expected a blur-radius InputItem"))

    val editing = level2.beginSubmenuEditMode

    editing.activeSettingsSurface.map(_.current) shouldBe Some(
      SettingsPage.Editing(groupId = "settings-surface-appearance", itemId = "blur-radius", draftText = currentValue)
    )
    editing.activeSubmenu.map(s => s.editingItemId -> s.editingText) shouldBe Some(Some("blur-radius") -> currentValue)
  }

  "normalizeSubmenuEditMode" should "leave an Editing page alone while its item is still selected" in {
    val editing = editingBlurRadius

    editing.normalizeSubmenuEditMode shouldBe editing
  }

  it should "revert to a Group page once the edited item is no longer selected" in {
    val editing = editingBlurRadius
    // Simulate the selection having moved without going through moveSubmenuSelection (which already clears edit
    // state itself) -- this isolates normalizeSubmenuEditMode's own reconciliation.
    val stale = editing.copy(activeSubmenu = editing.activeSubmenu.map(_.copy(selectedIndex = 0)))

    val normalized = stale.normalizeSubmenuEditMode

    normalized.activeSettingsSurface shouldBe Some(
      SettingsSurfaceState(
        SettingsPage.Group("settings-surface-appearance", 0),
        List(SettingsPage.Group("settings-appearance-motion", 1))
      )
    )
  }

  "updateSubmenuSearch" should "reset to a filtered Group page at index 0, dropping any Editing page" in {
    val editing = editingBlurRadius

    val searched = editing.updateSubmenuSearch("blur")

    searched.activeSettingsSurface shouldBe Some(
      SettingsSurfaceState(
        SettingsPage.Group("settings-surface-appearance", 0, "blur"),
        List(SettingsPage.Group("settings-appearance-motion", 1))
      )
    )
    searched.activeSubmenu.map(s => (s.groupId, s.selectedIndex, s.searchTerm)) shouldBe
      Some(("settings-surface-appearance", 0, "blur"))
  }

  "adjustSelectedSubmenuOption" should "leave activeSettingsSurface untouched" in {
    val level2 = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup

    val adjusted = level2.adjustSelectedSubmenuOption(1)

    adjusted.activeSettingsSurface shouldBe level2.activeSettingsSurface
    adjusted.optionSelections should not be level2.optionSelections
  }

  /** Navigates to Appearance & Motion > Surface Appearance > blur-radius (index 4 of 5) and begins editing it -- the
    * same target `SettingsSurfaceSpec` exercises for its footer/breadcrumb assertions.
    */
  private def editingBlurRadius: CommandRunner =
    opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup
      .moveSubmenuSelection(4)
      .beginSubmenuEditMode
