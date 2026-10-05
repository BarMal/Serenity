package com.serenity.perf

import com.serenity.command.{CommandRegistry, CommandRunner, CommandSurfaceItem}
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{Direction, RunnerInsertChar, RunnerNavigate}
import com.serenity.state.models.{
  AppState,
  Focus,
  SurfaceContent,
  SurfaceId,
  SurfacePlacement,
  SurfacePresentation,
  UiSurface
}
import com.serenity.state.reducers.CommandRunnerReducer
import com.serenity.ui.fonts.FontLoader

/** #1854's per-keystroke and per-arrow-press command runner update, measured as the reducer step plus the read the
  * renderer makes of the result -- that read is where a rebuilt settings index or re-run settings search would show.
  */
private[perf] object CommandRunnerBenchmarks:

  /** Sized like a laptop with a few hundred installed families: each family becomes one picker row per font role, so
    * the catalogue dominates the settings index the runner searches.
    */
  val fontCatalog: FontLoader.FontFamilyCatalog =
    def families(kind: String, count: Int): List[String] = (1 to count).toList.map(i => f"$kind Family $i%03d")
    FontLoader.FontFamilyCatalog(
      monospace = families("Mono", 80),
      text = families("Text", 320),
      ui = families("UI", 320)
    )

  private val registry = CommandRegistry.default

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    val palette       = paletteState(activated.updateSearchTerm("fon")(using registry))
    val paletteFound  = paletteState(activated.updateSearchTerm("font")(using registry))
    val settings      = paletteState(activated.openSettings.updateSearchTerm("fon")(using registry))
    val settingsFound = paletteState(activated.openSettings.updateSearchTerm("font")(using registry))
    val paletteTyped  = typed(palette)
    val paletteMoved  = arrowDown(paletteFound)
    val settingsTyped = typed(settings)
    val settingsMoved = arrowDown(settingsFound)
    List(
      BenchmarkRunner.Benchmark(
        "command_runner.palette.keystroke",
        3,
        20,
        () => assert(paletteTyped.searchTerm == "font" && hasSettingsMatches(paletteTyped.visibleItems)),
        () => typed(palette).visibleItems
      ),
      BenchmarkRunner.Benchmark(
        "command_runner.palette.arrow_press",
        3,
        20,
        () => assert(paletteMoved.selectedIndex == 1 && hasSettingsMatches(paletteMoved.visibleItems)),
        () => arrowDown(paletteFound).visibleItems
      ),
      BenchmarkRunner.Benchmark(
        "command_runner.settings.keystroke",
        3,
        20,
        () => assert(settingsTyped.searchTerm == "font" && settingsTyped.settingsSurfaceItems.size > 1),
        () => typed(settings).settingsSurfaceItems
      ),
      BenchmarkRunner.Benchmark(
        "command_runner.settings.arrow_press",
        3,
        20,
        () => assert(settingsMoved.settingsSurfaceSelectedIndex == 1 && settingsMoved.settingsSurfaceItems.size > 1),
        () => arrowDown(settingsFound).settingsSurfaceItems
      )
    )

  private def activated: CommandRunner =
    CommandRunner.empty.copy(fontFamilies = fontCatalog).activate(registry, AppConfig.default)

  private def typed(state: AppState): CommandRunner =
    runnerIn(CommandRunnerReducer.reduce(RunnerInsertChar('t'), state, registry).state)

  private def arrowDown(state: AppState): CommandRunner =
    runnerIn(CommandRunnerReducer.reduce(RunnerNavigate(Direction.Down), state, registry).state)

  private def hasSettingsMatches(items: List[CommandSurfaceItem]): Boolean =
    items.exists {
      case _: CommandSurfaceItem.SettingSearchItem | _: CommandSurfaceItem.GroupItem => true
      case _                                                                         => false
    }

  private def paletteState(runner: CommandRunner): AppState =
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val base = BenchmarkFixtures.editorState("", None)
    base.copy(
      persisted = base.persisted.copy(focus = Focus.Surface(surface.id)),
      runtime = base.runtime.copy(uiSurfaces = List(surface))
    )

  private def runnerIn(state: AppState): CommandRunner =
    state.commandRunnerSurface
      .flatMap(_.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None)
      .getOrElse(CommandRunner.empty)

end CommandRunnerBenchmarks
