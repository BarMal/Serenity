package com.serenity.state.reducers

import com.serenity.command.*
import com.serenity.config.{AppConfig, AppMode, AutoSaveMode, MarkdownViewMode, StatusSegment}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.session.SessionState
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the command runner's settings pickers do to the state and which effects they ask for: a highlighted value is
  * previewed through an unrecorded command over a [[PendingSetting]], Enter commits it, and abandoning it restores the
  * committed config and theme.
  */
class SettingsPreviewReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.default
  private val catalog =
    FontLoader.FontFamilyCatalog(monospace = List("Mono A", "Mono B"), text = List("Serif A"), ui = Nil)
  private val dusk    = Theme.dark.copy(name = "dusk")
  private val context = CommandRunnerContext(themeNames = List("dusk", "dawn"), currentThemeName = Some(dusk.name))

  private def stateWith(runner: CommandRunner, theme: Theme = Theme.default): AppState =
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    AppState(
      persisted = Persisted(
        layout = Layout.empty,
        buffers = Map.empty,
        focus = Focus.Surface(surface.id),
        theme = theme
      ),
      runtime = Runtime(uiSurfaces = List(surface))
    )

  private def activated: CommandRunner =
    CommandRunner.empty
      .copy(fontFamilies = catalog)
      .activate(registry, AppConfig.default, context = context)

  private def drilledOn(groupId: String, itemId: String): AppState =
    val opened = activated.openSettings
    val index  = opened.submenuItems(groupId).indexWhere(_.id == itemId).max(0)
    stateWith(opened.withDrilledSettingsSurface(SettingsSurfaceState(SettingsPage.Group(groupId, index))), dusk)

  private def runnerOf(state: AppState): CommandRunner =
    state.commandRunnerSurface
      .flatMap(_.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None)
      .getOrElse(fail("Expected the command runner to be open"))

  private def press(event: CommandRunnerEvent, state: AppState): ReducerResult =
    CommandRunnerReducer.reduce(event, state, registry)

  private def unrecorded(result: ReducerResult): List[CommandIntent] =
    result.effects.collect { case AppEffect.ExecuteCommandUnrecorded(command) => command.intent }

  private def recorded(result: ReducerResult): List[CommandIntent] =
    result.effects.collect { case AppEffect.ExecuteCommand(command) => command.intent }

  private def lineWrapIntent(enabled: Boolean): CommandIntent =
    CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.SetWordWrap(enabled)))

  "Cycling a setting option" should "preview it as an unrecorded command and record the pending value" in {
    val cycled = press(RunnerNavigate(Direction.Right), drilledOn("settings-text-display", "line-wrap"))

    unrecorded(cycled) shouldBe List(lineWrapIntent(false))
    recorded(cycled) shouldBe empty
    cycled.state.runtime.pendingSetting.map(_.scope) shouldBe Some("line-wrap")
    cycled.state.runtime.pendingSetting.map(_.committedConfig) shouldBe Some(AppConfig.default)
    cycled.state.persisted.commandUsage shouldBe empty
  }

  it should "keep the config from before the first preview while the preview moves on" in {
    val first  = press(RunnerNavigate(Direction.Right), drilledOn("settings-text-display", "line-wrap"))
    val second = press(RunnerNavigate(Direction.Right), first.state)

    unrecorded(second) shouldBe List(lineWrapIntent(true))
    second.state.runtime.pendingSetting.map(_.committedConfig) shouldBe Some(AppConfig.default)
  }

  it should "put the option back and end the preview on Escape, leaving the runner open" in {
    val cycled  = press(RunnerNavigate(Direction.Right), drilledOn("settings-text-display", "line-wrap"))
    val escaped = press(RunnerDismiss, cycled.state)

    escaped.state.runtime.pendingSetting shouldBe None
    escaped.state.commandRunnerSurface.isDefined shouldBe true
    runnerOf(escaped.state).optionSelections.get("line-wrap") shouldBe Some(0)
    escaped.effects shouldBe List(AppEffect.Settings(SettingsEffect.ReapplyConfig))
  }

  it should "commit on Enter with a recorded command and end the preview" in {
    val cycled    = press(RunnerNavigate(Direction.Right), drilledOn("settings-text-display", "line-wrap"))
    val submitted = press(RunnerSubmit, cycled.state)

    recorded(submitted) shouldBe List(lineWrapIntent(false))
    unrecorded(submitted) shouldBe empty
    submitted.state.runtime.pendingSetting shouldBe None
    submitted.state.commandRunnerSurface.isDefined shouldBe true
  }

  it should "abandon the preview when the highlight moves to another row" in {
    val cycled = press(RunnerNavigate(Direction.Right), drilledOn("settings-text-display", "line-wrap"))
    val moved  = press(RunnerNavigate(Direction.Down), cycled.state)

    moved.state.runtime.pendingSetting shouldBe None
    moved.effects shouldBe List(AppEffect.Settings(SettingsEffect.ReapplyConfig))
  }

  it should "abandon the preview when the command runner is toggled closed" in {
    val cycled = press(RunnerNavigate(Direction.Right), drilledOn("settings-text-display", "line-wrap"))
    val closed = AppEventReducer.reduce(ToggleCommandRunner, cycled.state, registry)

    closed.state.runtime.pendingSetting shouldBe None
    closed.state.commandRunnerSurface shouldBe None
    closed.effects should contain(AppEffect.Settings(SettingsEffect.ReapplyConfig))
  }

  "Stepping a numeric setting" should "preview the step and commit it on Enter" in {
    val stepped   = press(RunnerNavigate(Direction.Right), drilledOn("settings-code-font", "code-font-size"))
    val submitted = press(RunnerSubmit, stepped.state)

    unrecorded(stepped) should have size 1
    stepped.state.runtime.pendingSetting.map(_.scope) shouldBe Some("code-font-size")
    recorded(submitted) shouldBe stepped.state.runtime.pendingSetting.map(_.previewed.intent).toList
    submitted.state.runtime.pendingSetting shouldBe None
  }

  "Highlighting a font family" should "preview it, and Escape should end the preview" in {
    val moved = press(RunnerNavigate(Direction.Down), drilledOn("code-font", "code-font-0-mono-a"))

    unrecorded(moved) shouldBe List(
      CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetCodeFontFamily("Mono B")))
    )
    moved.state.runtime.pendingSetting.map(_.scope) shouldBe Some("code-font")
    press(RunnerDismiss, moved.state).state.runtime.pendingSetting shouldBe None
  }

  "Highlighting a theme" should "preview it, keep the saved theme, and restore the theme on Escape" in {
    val moved = press(RunnerNavigate(Direction.Down), drilledOn("theme", "theme-dusk"))

    unrecorded(moved) shouldBe List(CommandIntent.Theme(ThemeIntent.ApplyTheme("dawn")))
    moved.state.runtime.pendingSetting.map(_.committedTheme) shouldBe Some(dusk)

    val applied = ThemeStateReducer.applyTheme(Theme.light, moved.state).state
    applied.committedTheme shouldBe dusk
    SessionState.fromAppState(applied).themeName shouldBe dusk.name

    val escaped = press(RunnerDismiss, applied)

    escaped.state.persisted.theme shouldBe dusk
    escaped.state.runtime.themeDiscovery.requestedThemeName shouldBe Some(dusk.name)
    escaped.state.runtime.pendingSetting shouldBe None
  }

  "A pending setting" should "leave the saved session on the committed config" in {
    val cycled = press(RunnerNavigate(Direction.Right), drilledOn("settings-text-display", "line-wrap"))
    val live =
      cycled.state.copy(persisted = cycled.state.persisted.copy(config = AppConfig.default.withWordWrap(false)))

    SessionState.fromAppState(live).config shouldBe AppConfig.default
    live.committedConfig shouldBe AppConfig.default
  }

  "Switching between pickers" should "restore the first and preview the second" in {
    val first     = press(RunnerNavigate(Direction.Right), drilledOn("settings-code-font", "code-font-size"))
    val second    = press(RunnerNavigate(Direction.Up), first.state)
    val previewed = press(RunnerNavigate(Direction.Right), second.state)

    second.state.runtime.pendingSetting shouldBe None
    previewed.state.runtime.pendingSetting.map(_.scope) shouldBe Some("code-ligatures")
    previewed.state.runtime.pendingSetting.map(_.committedConfig) shouldBe Some(AppConfig.default)
  }

  "Settings that reshape the workspace" should "stay commit-on-select" in {
    val cycled = press(RunnerNavigate(Direction.Right), drilledOn("settings-document-defaults", "markdown-view"))

    recorded(cycled) shouldBe List(
      CommandIntent.View(ViewIntent.SetMarkdownViewMode(MarkdownViewMode.SplitPreview))
    )
    cycled.state.runtime.pendingSetting shouldBe None
  }

  "isPreviewable" should "accept value setters and refuse toggles, movers and commands" in {
    import SettingsPreviewReducer.isPreviewable

    isPreviewable(lineWrapIntent(true)) shouldBe true
    isPreviewable(CommandIntent.Theme(ThemeIntent.ApplyTheme("dawn"))) shouldBe true
    isPreviewable(CommandIntent.Settings(SettingsIntent.Font(FontIntent.SetUiFontFamily("Sans")))) shouldBe true
    isPreviewable(CommandIntent.Settings(SettingsIntent.TextDisplay(TextDisplayIntent.ToggleWordWrap))) shouldBe false
    isPreviewable(CommandIntent.Settings(SettingsIntent.Font(FontIntent.IncreaseFontSize))) shouldBe false
    isPreviewable(
      CommandIntent.Settings(SettingsIntent.StatusLine(StatusLineIntent.MoveSegmentEarlier(StatusSegment.Title)))
    ) shouldBe false
    isPreviewable(CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.ResetSettings))) shouldBe false
    isPreviewable(CommandIntent.View(ViewIntent.SetAppMode(AppMode.Prose))) shouldBe false
    isPreviewable(
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetAutoSaveMode(AutoSaveMode.AfterDelay)))
    ) shouldBe false
    isPreviewable(
      CommandIntent.Settings(SettingsIntent.General(GeneralSettingsIntent.SetAutoSaveDelayMillis(500)))
    ) shouldBe false
  }
