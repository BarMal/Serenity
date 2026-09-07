package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.*
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, MotionAccessibility}
import com.serenity.keystroke.events.Event
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence, SessionSaveTrigger}
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Exercises [[StateManagerConfigEffects]] on its own: a real config path in a temp directory, a session-persistence
  * double that records triggers instead of writing, and an [[EffectEditorPort]] double, so each settings intent can be
  * checked for the config it writes *and* the collaborator it calls.
  */
class StateManagerConfigEffectsSpec extends AnyFlatSpec with Matchers:

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
      val sessionTriggers: Ref[IO, List[SessionSaveTrigger]],
      val fontConfigs: Ref[IO, List[FontLoader.FontConfig]],
      val analysisRuns: Ref[IO, Int],
      val events: Ref[IO, List[Event]],
      val committedStates: Ref[IO, List[AppState]],
      val configPath: Option[Path],
      val config: StateManagerConfigEffects
  ):
    def currentConfig: AppConfig = stateRef.get.unsafeRunSync().persisted.config

  private def harness(
    initialState: AppState = AppState.initial,
    persistConfig: Boolean = true,
    deviceTextScale: Double = 1.0
  ): Harness =
    val root       = Files.createTempDirectory("config-effects-spec")
    val configPath = Option.when(persistConfig)(root.resolve("config.json"))
    val stateRef   = Ref.of[IO, AppState](initialState).unsafeRunSync()
    val triggers   = Ref.of[IO, List[SessionSaveTrigger]](Nil).unsafeRunSync()
    val fonts      = Ref.of[IO, List[FontLoader.FontConfig]](Nil).unsafeRunSync()
    val analyses   = Ref.of[IO, Int](0).unsafeRunSync()
    val events     = Ref.of[IO, List[Event]](Nil).unsafeRunSync()
    val committed  = Ref.of[IO, List[AppState]](Nil).unsafeRunSync()
    val animations =
      Ref.of[IO, Map[BufferId, com.serenity.animation.AnimationState]](Map.empty).unsafeRunSync()

    val editor = new EffectEditorPort:
      def updateState(update: AppState => AppState): IO[Unit] = stateRef.update(update)
      def enqueueEvent(event: Event): IO[Unit]                = events.update(_ :+ event)
      def validateAndUpdateState(newState: AppState, fallbackState: AppState): IO[Unit] =
        committed.update(_ :+ newState) >> stateRef.set(newState)
      def scheduleDocumentAnalysis(): IO[Unit]                     = analyses.update(_ + 1)
      def scheduleFindSearch(request: FindSearchRequest): IO[Unit] = IO.unit

    new Harness(
      stateRef,
      triggers,
      fonts,
      analyses,
      events,
      committed,
      configPath,
      new StateManagerConfigEffects(
        stateRef,
        NoOpLogger.impl[IO],
        configPath,
        new RecordingSessionPersistence(triggers, root),
        animations,
        fontConfig => fonts.update(_ :+ fontConfig),
        IO.pure(deviceTextScale),
        editor
      )
    )

  "StateManagerConfigEffects" should "apply a config update to state, persist it, and auto-save the session" in {
    val fixture = harness()

    fixture.config.updateConfig(_.withWheelScrollLines(7)).unsafeRunSync().inputConfig.wheelScrollLines shouldBe 7

    fixture.currentConfig.inputConfig.wheelScrollLines shouldBe 7
    fixture.sessionTriggers.get.unsafeRunSync() shouldBe List(SessionSaveTrigger.Manual)
    fixture.configPath.map(path => Files.exists(path)) shouldBe Some(true)
  }

  it should "still auto-save the session when no config file is configured" in {
    val fixture = harness(persistConfig = false)

    fixture.config.updateConfig(_.withWheelScrollLines(3)).unsafeRunSync()

    fixture.currentConfig.inputConfig.wheelScrollLines shouldBe 3
    fixture.sessionTriggers.get.unsafeRunSync() shouldBe List(SessionSaveTrigger.Manual)
  }

  it should "resolve the device text scale and report the resulting font config" in {
    val fixture = harness(deviceTextScale = 2.0)

    fixture.config
      .updateFontConfig(_.copy(textScaleMode = FontLoader.TextScaleMode.Auto))
      .unsafeRunSync()

    val reported = fixture.fontConfigs.get.unsafeRunSync()
    reported.size shouldBe 1
    reported.head shouldBe fixture.currentConfig.editorConfig.fontConfig
    reported.head.textScaleMultiplier shouldBe
      FontLoader.FontConfig.clampTextScale(2.0)
  }

  it should "clamp font sizes at both ends of the supported range" in {
    val fixture = harness()

    fixture.config.interpret(SettingsIntent.Font(FontIntent.SetFontSize(200.0f)), AppState.initial).unsafeRunSync()
    fixture.currentConfig.editorConfig.fontConfig.fontSize shouldBe 48.0f

    fixture.config.interpret(SettingsIntent.Font(FontIntent.SetFontSize(1.0f)), AppState.initial).unsafeRunSync()
    fixture.currentConfig.editorConfig.fontConfig.fontSize shouldBe 8.0f
  }

  it should "reschedule document analysis after a spell-check setting changes" in {
    val fixture = harness()

    fixture.config
      .interpret(SettingsIntent.SpellCheck(SpellCheckIntent.SetSpellCheckEnabled(true)), AppState.initial)
      .unsafeRunSync()

    fixture.currentConfig.languageToolsConfig.spellCheck.enabled shouldBe true
    fixture.analysisRuns.get.unsafeRunSync() shouldBe 1
  }

  it should "route the contextual toolbar toggle through the editor event queue rather than the config" in {
    val fixture = harness()

    fixture.config
      .interpret(SettingsIntent.PanelChrome(PanelChromeIntent.ToggleContextualToolbar), AppState.initial)
      .unsafeRunSync()

    fixture.events.get.unsafeRunSync() shouldBe List(com.serenity.keystroke.events.ToggleContextualToolbar)
    fixture.sessionTriggers.get.unsafeRunSync() shouldBe Nil
  }

  it should "discard live motion state when motion accessibility is turned off" in {
    val ghostId = SurfaceId("ghost")
    val motionState = AppState.initial.copy(
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            ghostId,
            SurfaceContent
              .GhostOverlay(SurfaceContent.Diagnostics(Nil), com.serenity.ui.layout.LayoutRect(0, 0, 10, 10)),
            SurfacePresentation.Modal
          )
        ),
        windowSitter = com.serenity.animation.WindowSitter.fromConfig(AppConfig.default.windowSitterConfig)
      )
    )
    val fixture = harness(motionState)

    fixture.config
      .interpret(
        SettingsIntent.Motion(MotionIntent.SetMotionAccessibility(MotionAccessibility.Off)),
        motionState
      )
      .unsafeRunSync()

    val after = fixture.stateRef.get.unsafeRunSync()
    after.runtime.uiSurfaces.map(_.id) should not contain ghostId
    after.runtime.themeTransition shouldBe None
    after.runtime.surfaceAnimations shouldBe Map.empty
  }

  it should "propagate a contextual toolbar display mode change into the live toolbar surface" in {
    val toolbarId = SurfaceId("toolbar")
    val toolbar   = ContextualToolbarState()
    val state = AppState.initial.copy(
      runtime = AppState.initial.runtime.copy(uiSurfaces =
        List(UiSurface(toolbarId, SurfaceContent.ContextualToolbar(toolbar), SurfacePresentation.Modal))
      )
    )
    val fixture = harness(state)
    val mode    = com.serenity.config.ToolbarDisplayMode.values.find(_ != toolbar.displayMode).get

    fixture.config
      .interpret(SettingsIntent.PanelChrome(PanelChromeIntent.SetContextualToolbarDisplayMode(mode)), state)
      .unsafeRunSync()

    fixture.stateRef.get.unsafeRunSync().surfaceById(toolbarId).map(_.content) shouldBe
      Some(SurfaceContent.ContextualToolbar(toolbar.copy(displayMode = mode)))
  }

  it should "write the current config to disk on an explicit save without touching the session" in {
    val fixture = harness()

    fixture.config.persistConfigFile(AppConfig.default.withWheelScrollLines(9)).unsafeRunSync()

    fixture.configPath.map(path => Files.exists(path)) shouldBe Some(true)
    fixture.sessionTriggers.get.unsafeRunSync() shouldBe Nil
  }
