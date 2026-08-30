package com.serenity

import com.serenity.command.*
import com.serenity.config.AppConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Unit-level contract for the settings-navigation methods on `CommandRunner` built on the `activeSettingsSurface` page
  * stack (issue #1059). These were originally written as a faithfulness check cross-comparing the page stack against
  * the flat `CommandRunnerSubmenuState`/`activeSubmenu` field it replaced; now that `activeSubmenu` and its mirroring
  * machinery are fully removed (production and specs read/write `activeSettingsSurface` exclusively), this spec
  * exercises each mutator's `activeSettingsSurface` outcome directly.
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
  }

  it should "build ancestor pages matching the preferred ancestor path when jumping to a search leaf" in {
    given CommandRegistry = registry
    val searched          = opened.updateSettingsSearch("default document")
    val leafIndex         = searched.visibleItems.indexWhere(_.isInstanceOf[CommandSurfaceItem.SettingSearchItem])
    leafIndex should be >= 0
    val entered = searched.withSelectedVisibleIndex(leafIndex).enterSelectedGroup

    entered.activeSettingsSurface.map(_.current.groupId) shouldBe Some("settings-document-defaults")
    entered.activeSettingsSurface.map(_.ancestors.map(_.groupId)) shouldBe Some(List("settings-document-writing"))
  }

  "enterSelectedSubmenuGroup" should "push, making the left group (at its current index) the nearest ancestor" in {
    val level1 = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup.moveSubmenuSelection(1)

    val level2 = level1.enterSelectedSubmenuGroup

    level2.activeSettingsSurface.map(_.current.groupId) shouldBe Some("settings-surface-appearance")
    level2.activeSettingsSurface.map(_.ancestors) shouldBe Some(
      List(SettingsPage.Group("settings-appearance-motion", 1))
    )
  }

  "exitSubmenuToPreview" should "pop and re-point the parent page at the child just left" in {
    val level2 = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup

    val back = level2.exitSubmenuToPreview

    back.activeSettingsSurface shouldBe Some(SettingsSurfaceState(SettingsPage.Group("settings-appearance-motion", 1)))
  }

  it should "close the whole stack when there is no parent to reveal" in {
    val level1 = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup

    val back = level1.exitSubmenuToPreview

    back.activeSettingsSurface shouldBe None
  }

  "moveSubmenuSelection" should "advance the current page's index" in {
    val level1 = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup

    val moved = level1.moveSubmenuSelection(1)

    moved.activeSettingsSurface shouldBe Some(SettingsSurfaceState(SettingsPage.Group("settings-appearance-motion", 1)))
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
  }

  "normalizeSubmenuEditMode" should "leave an Editing page alone while its item is still selected" in {
    val editing = editingBlurRadius

    editing.normalizeSubmenuEditMode shouldBe editing
  }

  it should "revert to a Group page once the edited item is no longer selected" in {
    val editing = editingBlurRadius
    // Simulate the underlying item list changing so the edited item ("blur-radius") is no longer present -- this
    // isolates normalizeSubmenuEditMode's own reconciliation from moveSubmenuSelection (which already clears edit
    // state itself and so can't be used to set this up).
    val stale = editing.activeSettingsSurface match
      case Some(surface) =>
        editing.withDrilledSettingsSurface(
          surface.copy(current =
            SettingsPage.Editing(groupId = "settings-surface-appearance", itemId = "no-such-item", draftText = "")
          )
        )
      case None => fail("expected a drilled settings surface")

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

  "withSelectedFocusedSubmenuIndex" should "set the current page's index directly, mirroring moveSubmenuSelection" in {
    val level1 = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup

    val moved = level1.withSelectedFocusedSubmenuIndex(3)

    moved.activeSettingsSurface shouldBe Some(SettingsSurfaceState(SettingsPage.Group("settings-appearance-motion", 3)))
  }

  it should "drop an Editing page back to Group" in {
    val editing = editingBlurRadius

    val moved = editing.withSelectedFocusedSubmenuIndex(2)

    moved.activeSettingsSurface.map(_.current) shouldBe Some(SettingsPage.Group("settings-surface-appearance", 2))
  }

  "deactivate" should "clear the settings-surface stack along with everything else" in {
    val active = editingBlurRadius

    val deactivated = active.deactivate

    deactivated.activeSettingsSurface shouldBe None
  }

  // issue #931: `withActiveCategory` is retired along with category tabs -- there is no successor operation, so
  // this test (switching categories closes the settings-surface stack) has nothing left to exercise.

  "updateSearchTerm" should "close the settings-surface stack" in {
    given CommandRegistry = registry
    val level1            = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup

    val searched = level1.updateSearchTerm("foo")

    searched.activeSettingsSurface shouldBe None
  }

  "focusedSubmenuItems" should "read through activeSettingsSurface, unfiltered" in {
    val level1 = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup

    level1.focusedSubmenuItems.map(_.id) shouldBe level1.submenuItems("settings-appearance-motion").map(_.id)
  }

  it should "read through activeSettingsSurface, applying its search filter" in {
    val searched = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .updateSubmenuSearch("appearance")

    searched.focusedSubmenuItems.map(_.id) should contain("settings-surface-appearance")
    searched.focusedSubmenuItems.size should be < searched.submenuItems("settings-appearance-motion").size
  }

  "settingsSurfaceItems" should "read through activeSettingsSurface once inside a group" in {
    val level1 = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup

    level1.settingsSurfaceItems.map(_.id) shouldBe level1.submenuItems("settings-appearance-motion").map(_.id)
  }

  "settingsSurfaceSelectedIndex" should "read through activeSettingsSurface, recovering an Editing page's index by item id" in {
    val editing = editingBlurRadius

    editing.settingsSurfaceSelectedIndex shouldBe 4
  }

  "settingsSurfaceBreadcrumbLabels" should "read through activeSettingsSurface" in {
    val level2 = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup

    level2.settingsSurfaceBreadcrumbLabels shouldBe List("Settings", "Appearance & Motion", "Surface Appearance")
  }

  "submenuBreadcrumbLabels" should "read through activeSettingsSurface.ancestors, reversed back to root-first" in {
    val level2 = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup

    level2.submenuBreadcrumbLabels("settings-surface-appearance") shouldBe List(
      "Appearance & Motion",
      "Surface Appearance"
    )
  }

  "withSubmenuEditingItem" should "begin editing a specific item with the given text" in {
    val level2 = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup

    val editing = level2.withSubmenuEditingItem("blur-radius", "1")

    editing.activeSettingsSurface.map(_.current) shouldBe Some(
      SettingsPage.Editing(groupId = "settings-surface-appearance", itemId = "blur-radius", draftText = "1")
    )
  }

  it should "continue (replace) the edit when called again for the same item" in {
    val started = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup
      .withSubmenuEditingItem("blur-radius", "1")

    val continued = started.withSubmenuEditingItem("blur-radius", "12")

    continued.activeSettingsSurface.map(_.current) shouldBe Some(
      SettingsPage.Editing(groupId = "settings-surface-appearance", itemId = "blur-radius", draftText = "12")
    )
  }

  it should "be a no-op with no active submenu" in {
    val runner = opened
    runner.withSubmenuEditingItem("blur-radius", "1") shouldBe runner
  }

  "withSubmenuEditingText" should "replace the currently-edited item's draft text" in {
    val editing = editingBlurRadius

    val updated = editing.withSubmenuEditingText("12")

    updated.activeSettingsSurface.map(_.current) shouldBe Some(
      SettingsPage.Editing(groupId = "settings-surface-appearance", itemId = "blur-radius", draftText = "12")
    )
  }

  it should "be a no-op when nothing is being edited" in {
    val level2 = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup

    level2.withSubmenuEditingText("x") shouldBe level2
  }

  "cancelSubmenuEditingText" should "cancel an in-progress edit, reverting to a Group page" in {
    val editing = editingBlurRadius

    val cancelled = editing.cancelSubmenuEditingText

    cancelled.activeSettingsSurface.map(_.current) shouldBe Some(SettingsPage.Group("settings-surface-appearance", 4))
  }

  "beginSubmenuRecording" should "begin an Editing page tagged with a fresh RecordingState" in {
    val level2 = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup

    val recording = level2.beginSubmenuRecording("blur-radius")

    recording.activeSettingsSurface.map(_.current) shouldBe Some(
      SettingsPage.Editing(
        groupId = "settings-surface-appearance",
        itemId = "blur-radius",
        draftText = "",
        recording = Some(RecordingState("blur-radius"))
      )
    )
  }

  "withPendingRecordedBinding" should "stash a pending keystroke on the current Editing page's RecordingState" in {
    val level2 = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup
    val recording = level2.beginSubmenuRecording("blur-radius")
    val info      = com.serenity.keystroke.KeyStrokeInfo(com.serenity.keystroke.InputKey.Ctrl, None, Set.empty)

    val pending = recording.withPendingRecordedBinding(info, 100L)

    pending.activeSettingsSurface.map(_.current) shouldBe Some(
      SettingsPage.Editing(
        groupId = "settings-surface-appearance",
        itemId = "blur-radius",
        draftText = "",
        recording = Some(RecordingState("blur-radius", pendingRecordedBinding = Some(info -> 100L)))
      )
    )
  }

  "clearSubmenuEditingAndRecording" should "clear all editing/recording sub-state, reverting to a Group page" in {
    val level2 = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup
    val info      = com.serenity.keystroke.KeyStrokeInfo(com.serenity.keystroke.InputKey.Ctrl, None, Set.empty)
    val recording = level2.beginSubmenuRecording("blur-radius").withPendingRecordedBinding(info, 100L)

    val cleared = recording.clearSubmenuEditingAndRecording

    // Recovers the Group page's index by looking up "blur-radius" itself (pageSelectedIndex's id-lookup for an
    // Editing page), landing on its own position (4) rather than whatever index was selected before
    // beginSubmenuRecording was called directly on an item -- the page stack has no separate "index before editing"
    // field to fall back to (issue #1059's `SettingsPage.Editing` names its item by id, not position).
    cleared.activeSettingsSurface.map(_.current) shouldBe Some(SettingsPage.Group("settings-surface-appearance", 4))
  }

  "deleteSubmenuTextBackward" should "delete one character of an Editing page's draft text" in {
    val editing = editingBlurRadius.withSubmenuEditingItem("blur-radius", "12")

    val deleted = editing.deleteSubmenuTextBackward

    deleted.activeSettingsSurface.map(_.current) shouldBe Some(
      SettingsPage.Editing(groupId = "settings-surface-appearance", itemId = "blur-radius", draftText = "1")
    )
  }

  it should "delete one character of a Group page's search term" in {
    val searched = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup
      .updateSubmenuSearch("blur")

    val deleted = searched.deleteSubmenuTextBackward

    deleted.activeSettingsSurface.map(_.current) shouldBe Some(
      SettingsPage.Group("settings-surface-appearance", 0, "blu")
    )
  }

  it should "be a no-op with no text to delete -- never navigating the stack (issue #1059)" in {
    val level2 = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .moveSubmenuSelection(1)
      .enterSelectedSubmenuGroup

    level2.deleteSubmenuTextBackward shouldBe level2
  }

  it should "be a no-op with no activeSettingsSurface" in {
    val runner = opened
    runner.deleteSubmenuTextBackward shouldBe runner
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
