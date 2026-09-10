package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.{CommandRunner, SettingsPage, SettingsSurfaceState, UiPresetsIntent}
import com.serenity.config.{AppConfig, PreferredWindowSize}
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence, SessionSaveTrigger}
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.PanelPosition
import com.serenity.ui.presets.{UiPreset, UiPresetStore}
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Exercises [[StateManagerUiPresetEffects]] on its own: a real [[UiPresetStore]] backed by a temp file, a
  * session-persistence double that records triggers instead of writing, and plain closures for the remaining
  * collaborators, so each [[UiPresetsIntent]] case can be checked for the state it lands, the preset store it writes,
  * and the collaborators it fires -- rather than through a fully composed `StateManager`.
  */
class StateManagerUiPresetEffectsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private class RecordingSessionPersistence(triggers: Ref[IO, List[SessionSaveTrigger]], root: Path)
      extends SessionPersistence(
        SessionManager.create(root, AppThemeManager.create, NoOpLogger.impl[IO], SessionManager.SessionPolicy()),
        SessionManager.SessionPolicy()
      ):
    override def maybeSaveSession(appState: AppState, trigger: SessionSaveTrigger): IO[Unit] =
      triggers.update(_ :+ trigger)

  final private class Harness(
      val stateRef: Ref[IO, AppState],
      val store: UiPresetStore,
      val sessionTriggers: Ref[IO, List[SessionSaveTrigger]],
      val fontConfigs: Ref[IO, List[FontLoader.FontConfig]],
      val persistedConfigs: Ref[IO, List[AppConfig]],
      val markdownPreviewOpens: Ref[IO, Int],
      val pinnedDirectoryLoads: Ref[IO, List[(PanelPosition, Path)]],
      val presets: StateManagerUiPresetEffects
  ):
    def currentState: AppState = stateRef.get.unsafeRunSync()

    def currentRunner: CommandRunner =
      currentState.commandRunnerSurface
        .map(_.content)
        .collectFirst {
          case SurfaceContent.CommandPalette(runner) =>
            runner
        }
        .getOrElse(fail("Expected a CommandPalette surface to be present"))

  private def harness(
    initialState: AppState = AppState.initial,
    windowSize: Option[PreferredWindowSize] = None
  ): Harness =
    val root      = Files.createTempDirectory("ui-preset-effects-spec")
    val store     = UiPresetStore(root.resolve("ui-presets.json"))
    val stateRef  = Ref.of[IO, AppState](initialState).unsafeRunSync()
    val triggers  = Ref.of[IO, List[SessionSaveTrigger]](Nil).unsafeRunSync()
    val fonts     = Ref.of[IO, List[FontLoader.FontConfig]](Nil).unsafeRunSync()
    val persisted = Ref.of[IO, List[AppConfig]](Nil).unsafeRunSync()
    val previews  = Ref.of[IO, Int](0).unsafeRunSync()
    val loads     = Ref.of[IO, List[(PanelPosition, Path)]](Nil).unsafeRunSync()

    new Harness(
      stateRef,
      store,
      triggers,
      fonts,
      persisted,
      previews,
      loads,
      new StateManagerUiPresetEffects(
        stateRef,
        NoOpLogger.impl[IO],
        store,
        IO.pure(windowSize),
        AppThemeManager.create,
        fontConfig => fonts.update(_ :+ fontConfig),
        new RecordingSessionPersistence(triggers, root),
        config => persisted.update(_ :+ config),
        (state, _) => state,
        previews.update(_ + 1),
        (position, path) => loads.update(_ :+ (position -> path))
      )
    )

  /** A live workspace with an open `CommandPalette` surface -- the surface every "context" side effect
    * (`editingPresetName`, `statusMessage`, drilled settings group, focus) is written onto.
    */
  private def commandPaletteState(base: AppState = AppState.initial): AppState =
    val surface =
      UiSurface(
        SurfaceId("palette"),
        SurfaceContent.CommandPalette(CommandRunner.empty),
        SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
      )
    base.copy(runtime = base.runtime.copy(uiSurfaces = base.runtime.uiSurfaces :+ surface))

  "StateManagerUiPresetEffects" should "ignore SaveUiPresetAsNew with a blank name" in {
    val fixture = harness(commandPaletteState())

    fixture.presets.interpret(UiPresetsIntent.SaveUiPresetAsNew("   ")).unsafeRunSync()

    fixture.store.list().unsafeRunSync() shouldBe Nil
    fixture.currentRunner.statusMessage shouldBe None
  }

  it should "save the live workspace as a new preset and focus its editing group" in {
    val fixture = harness(commandPaletteState())

    fixture.presets.interpret(UiPresetsIntent.SaveUiPresetAsNew("My Layout")).unsafeRunSync()

    fixture.store.list().unsafeRunSync().map(_.name) shouldBe List("My Layout")

    val after  = fixture.currentState
    val runner = fixture.currentRunner
    runner.editingPresetName shouldBe Some("My Layout")
    runner.statusMessage shouldBe Some("Preset saved. Configure My Layout.")
    runner.editingItemId shouldBe None
    runner.activeSettingsSurface shouldBe Some(
      SettingsSurfaceState(
        SettingsPage.Group("settings-preset-edit"),
        List(SettingsPage.Group("settings-ui-presets", 2))
      )
    )
    after.persisted.focus shouldBe Focus.Surface(after.commandRunnerSurface.get.id)
  }

  it should "report a failure rather than creating a preset whose name already exists" in {
    val fixture = harness(commandPaletteState())
    fixture.store.create(UiPreset.capture("Existing", AppState.initial, None)).unsafeRunSync()

    fixture.presets.interpret(UiPresetsIntent.SaveUiPresetAsNew("Existing")).unsafeRunSync()

    fixture.store.list().unsafeRunSync().size shouldBe 1
    fixture.currentRunner.statusMessage.get should include("Could not save Existing")
  }

  it should "refuse to overwrite a built-in preset" in {
    val fixture = harness(commandPaletteState())

    fixture.presets.interpret(UiPresetsIntent.OverwriteUiPreset("Writing")).unsafeRunSync()

    fixture.store.list().unsafeRunSync() shouldBe Nil
    fixture.currentRunner.statusMessage shouldBe Some("Built-in preset cannot be overwritten. Duplicate Writing first.")
    fixture.currentRunner.editingPresetName shouldBe Some("Writing")
  }

  it should "report a missing custom preset on overwrite instead of creating one" in {
    val fixture = harness(commandPaletteState())

    fixture.presets.interpret(UiPresetsIntent.OverwriteUiPreset("Ghost")).unsafeRunSync()

    fixture.store.list().unsafeRunSync() shouldBe Nil
    fixture.currentRunner.statusMessage shouldBe Some("Custom preset 'Ghost' was not found. Use Save As New Preset.")
  }

  it should "overwrite an existing custom preset with the live workspace" in {
    val liveState =
      commandPaletteState(AppState.initial.copy(persisted = AppState.initial.persisted.copy(theme = Theme.light)))
    val fixture = harness(liveState)
    fixture.store.create(UiPreset.capture("Custom", AppState.initial, None)).unsafeRunSync()

    fixture.presets.interpret(UiPresetsIntent.OverwriteUiPreset("Custom")).unsafeRunSync()

    fixture.store.find("Custom").unsafeRunSync().map(_.themeName) shouldBe Some("light")
    fixture.currentRunner.statusMessage shouldBe Some("Preset overwritten. Configure Custom.")
  }

  it should "leave state untouched when applying a preset that does not exist" in {
    val initial = commandPaletteState()
    val fixture = harness(initial)

    fixture.presets.interpret(UiPresetsIntent.ApplyUiPreset("Ghost")).unsafeRunSync()

    fixture.currentState shouldBe initial
    fixture.persistedConfigs.get.unsafeRunSync() shouldBe Nil
  }

  it should "apply a custom preset's theme, config, and font, and auto-save the session" in {
    val fixture = harness(commandPaletteState())
    val preset = UiPreset(name = "Custom", config = AppConfig.default, themeName = Theme.light.name, pinnedPanels = Nil)
    fixture.store.create(preset).unsafeRunSync()

    fixture.presets.interpret(UiPresetsIntent.ApplyUiPreset("Custom")).unsafeRunSync()

    val after = fixture.currentState
    after.persisted.theme.name shouldBe "light"
    fixture.persistedConfigs.get.unsafeRunSync().size shouldBe 1
    fixture.fontConfigs.get.unsafeRunSync() shouldBe List(after.persisted.config.editorConfig.fontConfig)
    fixture.sessionTriggers.get.unsafeRunSync() shouldBe List(SessionSaveTrigger.Manual)
    fixture.markdownPreviewOpens.get.unsafeRunSync() shouldBe 0
  }

  it should "apply a built-in workflow preset and reload its pinned directory panels" in {
    val fixture = harness(commandPaletteState())

    fixture.presets.interpret(UiPresetsIntent.ApplyUiPreset("Code")).unsafeRunSync()

    val after = fixture.currentState
    after.persisted.theme.name shouldBe "dark"
    after.pinnedSurfaces.exists(surface =>
      after.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id)).contains(PanelPosition.Left)
    ) shouldBe true
    fixture.pinnedDirectoryLoads.get.unsafeRunSync() shouldBe List(PanelPosition.Left -> Path.of("."))
  }

  it should "reject applying a preset that requires an unavailable font" in {
    val fixture    = harness(commandPaletteState())
    val fontConfig = FontLoader.FontConfig().copy(textFontFamily = "Definitely-Not-An-Installed-Font-XYZ")
    val preset = UiPreset(
      name = "Custom",
      config = AppConfig.default.withFontConfig(fontConfig),
      themeName = Theme.dark.name,
      pinnedPanels = Nil
    )
    fixture.store.create(preset).unsafeRunSync()

    fixture.presets.interpret(UiPresetsIntent.ApplyUiPreset("Custom")).unsafeRunSync()

    fixture.currentRunner.statusMessage shouldBe Some(
      "Cannot preview Custom: Preset requires unavailable text font 'Definitely-Not-An-Installed-Font-XYZ'."
    )
    fixture.persistedConfigs.get.unsafeRunSync() shouldBe Nil
  }

  it should "reject applying a preset whose theme cannot be loaded" in {
    val fixture = harness(commandPaletteState())
    val preset =
      UiPreset(name = "Custom", config = AppConfig.default, themeName = "does-not-exist-theme", pinnedPanels = Nil)
    fixture.store.create(preset).unsafeRunSync()

    fixture.presets.interpret(UiPresetsIntent.ApplyUiPreset("Custom")).unsafeRunSync()

    val message = fixture.currentRunner.statusMessage.getOrElse(fail("Expected a status message"))
    message should startWith("Cannot preview Custom: Theme 'does-not-exist-theme' could not be loaded")
    fixture.persistedConfigs.get.unsafeRunSync() shouldBe Nil
  }

  it should "report a missing source preset on duplicate instead of creating one" in {
    val fixture = harness(commandPaletteState())

    fixture.presets.interpret(UiPresetsIntent.DuplicateUiPreset("Ghost", "Copy")).unsafeRunSync()

    fixture.store.list().unsafeRunSync() shouldBe Nil
  }

  it should "duplicate an existing custom preset under a new name" in {
    val fixture = harness(commandPaletteState())
    fixture.store.create(UiPreset.capture("Custom", AppState.initial, None)).unsafeRunSync()

    fixture.presets.interpret(UiPresetsIntent.DuplicateUiPreset("Custom", "Custom Copy")).unsafeRunSync()

    fixture.store.list().unsafeRunSync().map(_.name).toSet shouldBe Set("Custom", "Custom Copy")
    fixture.currentRunner.statusMessage shouldBe Some("Preset duplicated. Configure Custom Copy.")
  }

  it should "duplicate a built-in preset into a new custom preset" in {
    val fixture = harness(commandPaletteState())

    fixture.presets.interpret(UiPresetsIntent.DuplicateUiPreset("Writing", "My Writing")).unsafeRunSync()

    fixture.store.find("My Writing").unsafeRunSync().map(_.themeName) shouldBe Some(Theme.dark.name)
  }

  it should "refuse to rename a built-in preset" in {
    val fixture = harness(commandPaletteState())

    fixture.presets.interpret(UiPresetsIntent.RenameUiPreset("Writing", "My Writing")).unsafeRunSync()

    fixture.store.list().unsafeRunSync() shouldBe Nil
    fixture.currentRunner.statusMessage shouldBe Some("Built-in preset cannot be renamed. Duplicate Writing first.")
  }

  it should "rename an existing custom preset" in {
    val fixture = harness(commandPaletteState())
    fixture.store.create(UiPreset.capture("Custom", AppState.initial, None)).unsafeRunSync()

    fixture.presets.interpret(UiPresetsIntent.RenameUiPreset("Custom", "Renamed")).unsafeRunSync()

    fixture.store.list().unsafeRunSync().map(_.name) shouldBe List("Renamed")
    fixture.currentRunner.statusMessage shouldBe Some("Preset renamed. Configure Renamed.")
  }

  it should "refuse to delete a built-in preset" in {
    val fixture = harness(commandPaletteState())

    fixture.presets.interpret(UiPresetsIntent.DeleteUiPreset("Writing")).unsafeRunSync()

    fixture.currentRunner.statusMessage shouldBe Some(
      "Built-in preset cannot be deleted. Use Reset Preset to discard overrides."
    )
  }

  it should "delete an existing custom preset and clear the editing context" in {
    val fixture = harness(commandPaletteState())
    fixture.store.create(UiPreset.capture("Custom", AppState.initial, None)).unsafeRunSync()

    fixture.presets.interpret(UiPresetsIntent.DeleteUiPreset("Custom")).unsafeRunSync()

    fixture.store.list().unsafeRunSync() shouldBe Nil
    fixture.currentRunner.statusMessage shouldBe Some("Preset deleted.")
    fixture.currentRunner.editingPresetName shouldBe None
  }

  it should "reset a built-in preset, discarding any stored override" in {
    val fixture = harness(commandPaletteState())

    fixture.presets.interpret(UiPresetsIntent.ResetUiPreset("Writing")).unsafeRunSync()

    fixture.currentRunner.statusMessage shouldBe Some("Preset reset. Configure Writing.")
    fixture.currentRunner.editingPresetName shouldBe Some("Writing")
  }

  it should "warn rather than reset a preset name that is not built-in" in {
    val fixture = harness(commandPaletteState())
    fixture.store.create(UiPreset.capture("Custom", AppState.initial, None)).unsafeRunSync()

    fixture.presets.interpret(UiPresetsIntent.ResetUiPreset("Custom")).unsafeRunSync()

    fixture.currentRunner.statusMessage shouldBe None
    fixture.store.list().unsafeRunSync().map(_.name) shouldBe List("Custom")
  }
