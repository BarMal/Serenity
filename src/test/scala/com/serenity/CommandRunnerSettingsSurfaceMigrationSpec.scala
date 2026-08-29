package com.serenity

import com.serenity.command.*
import com.serenity.config.AppConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** TDD contract for the settings-relevant `CommandRunner` methods re-platformed onto `activeSettingsSurface` (issue
  * #1059's page stack), migrated alongside their pre-existing `activeSubmenu` behavior. Every case below asserts the
  * new field's shape and, where useful, cross-checks it against the old field it mirrors -- both are still written by
  * these methods (see the field's doc on `CommandRunner`), so this is a faithfulness check on the re-platforming, not a
  * behavior change.
  *
  * Every *writer* of `activeSubmenu` now keeps `activeSettingsSurface` in sync as a faithful mirror (see the final
  * "always be defined exactly when activeSubmenu is" case for the full-surface consistency check), so setup chains
  * below are free to use whichever method reads most naturally for the scenario.
  *
  * The five *reader* methods (`settingsSurfaceItems`/`settingsSurfaceSelectedIndex`/`settingsSurfaceBreadcrumbLabels`/
  * `focusedSubmenuItems`/`submenuBreadcrumbLabels`) now read `activeSettingsSurface` too: `SurfaceContentResolver`
  * routes every settings render through the single-list resolver and `CommandRunnerMouseHitTesting` hit-tests it the
  * same way, so a state built only by setting `activeSubmenu` directly (bypassing the migrated mutators) no longer
  * renders or hit-tests correctly -- see the "no longer reflect a directly-constructed activeSubmenu" case below, and
  * the matching `activeSettingsSurface` now added wherever `SettingsSurfaceSpec`/`SurfaceContentResolverSpec` build
  * such states by hand.
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

  "withSelectedFocusedSubmenuIndex" should "set the current page's index directly, mirroring moveSubmenuSelection" in {
    val level1 = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup

    val moved = level1.withSelectedFocusedSubmenuIndex(3)

    moved.activeSettingsSurface shouldBe Some(SettingsSurfaceState(SettingsPage.Group("settings-appearance-motion", 3)))
    moved.activeSubmenu.map(_.selectedIndex) shouldBe Some(3)
  }

  it should "drop an Editing page back to Group" in {
    val editing = editingBlurRadius

    val moved = editing.withSelectedFocusedSubmenuIndex(2)

    moved.activeSettingsSurface.map(_.current) shouldBe Some(SettingsPage.Group("settings-surface-appearance", 2))
  }

  "clearGroupPreview" should "close the whole settings-surface stack" in {
    val level1 = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup

    val cleared = level1.clearGroupPreview

    cleared.activeSettingsSurface shouldBe None
    cleared.previewedGroupId shouldBe None
  }

  "deactivate" should "clear the settings-surface stack along with everything else" in {
    val active = editingBlurRadius

    val deactivated = active.deactivate

    deactivated.activeSettingsSurface shouldBe None
    deactivated.activeSubmenu shouldBe None
  }

  "withActiveCategory" should "close the settings-surface stack when switching categories" in {
    given CommandRegistry = registry
    val level1            = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup

    val switched = level1.withActiveCategory(CommandCategory.File)

    switched.activeSettingsSurface shouldBe None
    switched.activeSubmenu shouldBe None
  }

  "updateSearchTerm" should "close the settings-surface stack" in {
    given CommandRegistry = registry
    val level1            = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup

    val searched = level1.updateSearchTerm("foo")

    searched.activeSettingsSurface shouldBe None
    searched.activeSubmenu shouldBe None
  }

  // The five reader methods below (focusedSubmenuItems, settingsSurfaceItems, settingsSurfaceSelectedIndex,
  // settingsSurfaceBreadcrumbLabels, submenuBreadcrumbLabels) now read activeSettingsSurface, flipped this turn
  // (issue #1059) once SurfaceContentResolver/CommandRunnerMouseHitTesting routed every settings render/hit-test
  // through it instead of constructing a second submenu surface. States built via the migrated mutators (as every
  // case below except the last one is) keep both fields in sync, so these mostly cross-check against equivalent
  // activeSubmenu-derived expectations to confirm the flip didn't change observable behavior for real navigation.

  "focusedSubmenuItems" should "read through activeSettingsSurface, unfiltered" in {
    val level1 = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup

    level1.focusedSubmenuItems.map(_.id) shouldBe level1.submenuItems("settings-appearance-motion").map(_.id)
  }

  it should "read through activeSettingsSurface, applying its search filter" in {
    val searched = opened
      .withSelectedItem("settings-appearance-motion")
      .enterSelectedGroup
      .updateSubmenuSearch("appearance")

    searched.focusedSubmenuItems shouldBe searched.activeSubmenu
      .map(_.filteredItems(searched.submenuItems("settings-appearance-motion")))
      .getOrElse(Nil)
    searched.focusedSubmenuItems.map(_.id) should contain("settings-surface-appearance")
  }

  "settingsSurfaceItems" should "read through activeSettingsSurface once inside a group" in {
    val level1 = opened.withSelectedItem("settings-appearance-motion").enterSelectedGroup

    level1.settingsSurfaceItems.map(_.id) shouldBe level1.submenuItems("settings-appearance-motion").map(_.id)
  }

  "settingsSurfaceSelectedIndex" should "read through activeSettingsSurface, recovering an Editing page's index by item id" in {
    val editing = editingBlurRadius

    editing.settingsSurfaceSelectedIndex shouldBe 4
    editing.settingsSurfaceSelectedIndex shouldBe editing.activeSubmenu.map(_.selectedIndex).getOrElse(-1)
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

  it should "no longer reflect a directly-constructed activeSubmenu missing a matching activeSettingsSurface" in {
    // Mirrors how SettingsSurfaceSpec/SurfaceContentResolverSpec used to build states before this turn's flip: setting
    // activeSubmenu via a raw copy, bypassing every migrated mutator, leaving activeSettingsSurface at its default
    // None. Those specs now build a matching activeSettingsSurface alongside activeSubmenu; this pins down why --
    // without it, these readers fall back to their root-level (no active group) behavior.
    val runner = opened.copy(activeSubmenu = Some(CommandRunnerSubmenuState("settings-surface-appearance")))

    runner.activeSettingsSurface shouldBe None
    runner.settingsSurfaceItems shouldBe runner.settingsGroups
    runner.settingsSurfaceSelectedIndex shouldBe runner.selectedIndex
    runner.settingsSurfaceBreadcrumbLabels shouldBe List("Settings")
  }

  // The mutators below are new this turn, added to let CommandRunnerReducer eliminate every direct
  // CommandRunnerSubmenuState/activeSubmenu construction of its own (issue #1059) -- see the migration report for
  // where each replaces reducer code that used to hand-build activeSubmenu without touching activeSettingsSurface.

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
    editing.activeSubmenu.map(s => s.editingItemId -> s.editingText) shouldBe Some(Some("blur-radius") -> "1")
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
    updated.activeSubmenu.map(_.editingText) shouldBe Some("12")
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
    cancelled.activeSubmenu.map(s => s.editingItemId -> s.editingText) shouldBe Some(None -> "")
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
    recording.activeSubmenu.map(s => (s.editingItemId, s.editingText, s.recordingItemId)) shouldBe
      Some((Some("blur-radius"), "", Some("blur-radius")))
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
    pending.activeSubmenu.flatMap(_.pendingRecordedBinding) shouldBe Some(info -> 100L)
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

    cleared.activeSettingsSurface.map(_.current) shouldBe Some(SettingsPage.Group("settings-surface-appearance", 0))
    cleared.activeSubmenu.map(s =>
      (s.editingItemId, s.editingText, s.recordingItemId, s.pendingRecordedBinding)
    ) shouldBe Some((None, "", None, None))
  }

  "deleteSubmenuTextBackward" should "delete one character of an Editing page's draft text" in {
    val editing = editingBlurRadius.withSubmenuEditingItem("blur-radius", "12")

    val deleted = editing.deleteSubmenuTextBackward

    deleted.activeSettingsSurface.map(_.current) shouldBe Some(
      SettingsPage.Editing(groupId = "settings-surface-appearance", itemId = "blur-radius", draftText = "1")
    )
    deleted.activeSubmenu.map(_.editingText) shouldBe Some("1")
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
    deleted.activeSubmenu.map(_.searchTerm) shouldBe Some("blu")
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

  "selectPreviewSubmenuItem" should "start a fresh, ancestor-less single-page stack at the given index" in {
    val selected = opened.selectPreviewSubmenuItem("settings-surface-appearance", 2)

    selected.activeSettingsSurface shouldBe Some(
      SettingsSurfaceState(SettingsPage.Group("settings-surface-appearance", 2))
    )
    selected.activeSubmenu shouldBe Some(CommandRunnerSubmenuState("settings-surface-appearance", selectedIndex = 2))
    selected.previewedGroupId shouldBe Some("settings-surface-appearance")
  }

  "activeSettingsSurface" should "always be defined exactly when activeSubmenu is, across the full migrated surface" in {
    given CommandRegistry = registry
    val steps: List[CommandRunner => CommandRunner] = List(
      _.withSelectedItem("settings-appearance-motion"),
      _.enterSelectedGroup,
      _.withSelectedFocusedSubmenuIndex(1),
      _.enterSelectedSubmenuGroup,
      _.moveSubmenuSelection(4),
      _.beginSubmenuEditMode,
      _.normalizeSubmenuEditMode,
      _.updateSubmenuSearch("blur"),
      _.exitSubmenuToPreview,
      _.clearGroupPreview,
      _.openSettings,
      _.withSelectedItem("settings-appearance-motion"),
      _.enterSelectedGroup,
      _.updateSearchTerm("q"),
      _.withActiveCategory(CommandCategory.Settings),
      _.deactivate
    )

    steps.foldLeft(opened) { (runner, step) =>
      val next = step(runner)
      next.activeSettingsSurface.isDefined shouldBe next.activeSubmenu.isDefined
      next
    }
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
