package com.serenity.command

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.Paste
import com.serenity.state.models.*
import com.serenity.state.reducers.CommandRunnerReducer
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1854: the settings index is built when the runner's settings inputs change, and the results when the query does --
  * never again for a selection move, a re-render, or each character of a paste.
  */
class CommandRunnerSearchStateSpec extends AnyFlatSpec with Matchers:

  private val fontFamilies =
    FontLoader.FontFamilyCatalog(monospace = List("Mono A", "Mono B"), text = List("Serif A"), ui = List("Sans A"))

  private def activated(registry: CommandRegistry): CommandRunner =
    CommandRunner.empty.copy(fontFamilies = fontFamilies).activate(registry, AppConfig.default)

  private def settingsResults(runner: CommandRunner): List[CommandSurfaceItem] =
    runner.visibleItems.filter {
      case _: CommandSurfaceItem.SettingSearchItem | _: CommandSurfaceItem.GroupItem => true
      case _                                                                         => false
    }

  "Moving the selection" should "reuse the settings tree rather than rebuilding it" in {
    val runner = activated(CommandRegistry.default).openSettings

    val moved = runner.moveSelection(1)

    moved.selectedIndex shouldBe 1
    moved.settingsGroups should be theSameInstanceAs runner.settingsGroups
  }

  it should "reuse the settings search results for an unchanged query" in {
    given CommandRegistry = CommandRegistry.default
    val searched          = activated(summon[CommandRegistry]).openSettings.updateSearchTerm("font")
    val results           = searched.settingsSurfaceItems
    results.size should be > 1

    val moved = searched.moveSelection(1).moveSelection(1)

    moved.settingsSurfaceItems should be theSameInstanceAs results
  }

  it should "reuse the palette's settings matches for an unchanged query" in {
    given CommandRegistry = CommandRegistry.default
    val searched          = activated(summon[CommandRegistry]).updateSearchTerm("font size")
    val matches           = settingsResults(searched)
    matches should not be empty

    val moved = searched.moveSelection(1)

    settingsResults(moved).corresponds(matches)(_ eq _) shouldBe true
  }

  "Settings search results" should "be the same whether typed a character at a time or found from a fresh index" in {
    given CommandRegistry = CommandRegistry.default
    val typed = "font".foldLeft(activated(summon[CommandRegistry]).openSettings)((runner, char) =>
      runner.updateSearchTerm(runner.searchTerm + char)
    )
    val fresh = activated(summon[CommandRegistry]).openSettings.updateSearchTerm("font")

    // Compared by id: separately built trees hold distinct (but equivalent) `InputItem.parse` functions.
    typed.moveSelection(1).settingsSurfaceItems.map(_.id) shouldBe fresh.settingsSurfaceItems.map(_.id)
  }

  "Pasting into the search box" should "search once for the whole pasted text" in {
    val registry = CountingRegistry(CommandRegistry.default.getAllCommands)
    val state    = withClipboard(paletteState(activated(registry)), "font size")

    val pasted = CommandRunnerReducer.reduce(Paste, state, registry)

    runnerIn(pasted.state).searchTerm shouldBe "font size"
    registry.searches.get shouldBe 1
  }

  final private class CountingRegistry(commands: List[Command]) extends CommandRegistry(commands):
    val searches = new AtomicInteger(0)

    override def searchCommands(term: String, maxResults: Int): List[Command] =
      searches.incrementAndGet()
      super.searchCommands(term, maxResults)

  private def paletteState(runner: CommandRunner): AppState =
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    AppState(
      persisted = Persisted(layout = Layout.empty, buffers = Map.empty, focus = Focus.Surface(surface.id)),
      runtime = Runtime(uiSurfaces = List(surface), focusHistory = List(Focus.EditorPane(PaneId(2))))
    )

  private def withClipboard(state: AppState, text: String): AppState =
    state.copy(runtime = state.runtime.copy(clipboard = Some(text)))

  private def runnerIn(state: AppState): CommandRunner =
    state.commandRunnerSurface
      .flatMap(_.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None)
      .getOrElse(fail("expected the command palette to be open"))

end CommandRunnerSearchStateSpec
