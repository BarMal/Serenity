package com.serenity

import com.serenity.command.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Search/navigate/select/run semantics for `CommandPaletteState` (Stage 2 of the command-runner interaction-model
  * rework, issue #931) -- the palette-only state split out of `CommandRunner`'s shared mode-flag grab-bag. Exercises
  * the type standalone; it is not yet wired into `CommandRunner`/`UiSurface`.
  */
class CommandPaletteStateSpec extends AnyFlatSpec with Matchers:

  given CommandRegistry = CommandRegistry.default

  "empty" should "carry no search term, no selection offset, and no results" in {
    val state = CommandPaletteState.empty
    state.searchTerm shouldBe ""
    state.selectedIndex shouldBe 0
    state.filteredCommands shouldBe empty
  }

  "activate" should "populate every registered command with no category to default to" in {
    val state = CommandPaletteState.activate(CommandRegistry.default)
    state.filteredCommands should contain theSameElementsAs CommandRegistry.default.getAllCommands
    state.searchTerm shouldBe ""
    state.selectedIndex shouldBe 0
  }

  "updateSearchTerm" should "fall back to every command when the term is cleared" in {
    val activated = CommandPaletteState.activate(CommandRegistry.default)
    val searched  = activated.updateSearchTerm("save")
    val cleared   = searched.updateSearchTerm("")
    cleared.filteredCommands should contain theSameElementsAs CommandRegistry.default.getAllCommands
  }

  it should "delegate to the registry's fuzzy search, resetting selection to the top" in {
    val state = CommandPaletteState.activate(CommandRegistry.default).copy(selectedIndex = 3)
    val result = state.updateSearchTerm("save")
    result.searchTerm shouldBe "save"
    result.selectedIndex shouldBe 0
    result.filteredCommands.map(_.name) should contain("save")
  }

  it should "still find a Settings-only command by name even though category tabs are gone" in {
    // "toggle-theme" lived only under the old Settings category tab; with no tab to switch to, search is now the
    // only way to find it (issue #931's "retire category tabs, fold into text search").
    val result = CommandPaletteState.activate(CommandRegistry.default).updateSearchTerm("toggle theme")
    result.filteredCommands.map(_.name) should contain("toggle-theme")
  }

  "selectedCommand" should "read the command at selectedIndex" in {
    val state = CommandPaletteState(filteredCommands = List(sampleCommand("a"), sampleCommand("b")), selectedIndex = 1)
    state.selectedCommand.map(_.name) shouldBe Some("b")
  }

  it should "be empty when there are no results" in {
    CommandPaletteState.empty.selectedCommand shouldBe None
  }

  "moveSelection" should "wrap forward past the last item back to the first" in {
    val state = CommandPaletteState(filteredCommands = List(sampleCommand("a"), sampleCommand("b")), selectedIndex = 1)
    state.moveSelection(1).selectedIndex shouldBe 0
  }

  it should "wrap backward past the first item to the last" in {
    val state = CommandPaletteState(filteredCommands = List(sampleCommand("a"), sampleCommand("b")), selectedIndex = 0)
    state.moveSelection(-1).selectedIndex shouldBe 1
  }

  it should "be a no-op on an empty result list" in {
    CommandPaletteState.empty.moveSelection(1).selectedIndex shouldBe 0
  }

  "withSelectedIndex" should "jump to any in-bounds index (mouse hover/click)" in {
    val state = CommandPaletteState(filteredCommands = List(sampleCommand("a"), sampleCommand("b"), sampleCommand("c")))
    state.withSelectedIndex(2).selectedIndex shouldBe 2
  }

  it should "ignore an out-of-bounds index" in {
    val state = CommandPaletteState(filteredCommands = List(sampleCommand("a")), selectedIndex = 0)
    state.withSelectedIndex(5).selectedIndex shouldBe 0
  }

  private def sampleCommand(name: String): Command =
    Command.typed(
      name,
      s"Sample command $name",
      CommandIntent.Edit(EditIntent.Copy),
      CommandCategory.Edit,
      label = name
    )
