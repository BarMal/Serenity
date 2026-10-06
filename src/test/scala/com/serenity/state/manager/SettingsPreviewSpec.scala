package com.serenity.state.manager

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.command.*
import com.serenity.config.{AppConfig, ConfigError}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence, SessionSaveTrigger}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, CommandRunnerReducer, SettingsEffect}
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** A settings value changed from the command runner is a preview until it is committed: nothing reaches the config file
  * or the session until Enter, and Escape puts back what was there when the preview began. These specs drive the real
  * reducer and the real config interpreter side by side, counting what is written.
  */
class SettingsPreviewSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.default
  private val catalog =
    FontLoader.FontFamilyCatalog(monospace = List("Mono A", "Mono B", "Mono C"), text = List("Serif A"), ui = Nil)

  final private class Fixture(initial: AppState):
    private val modelRef = Ref.of[IO, Model](Model(initial, UndoState())).unsafeRunSync()
    private val stateRef = ModelViews.appRef(modelRef)
    private val writes   = Ref.of[IO, List[AppConfig]](Nil).unsafeRunSync()
    private val saves    = Ref.of[IO, List[SessionSaveTrigger]](Nil).unsafeRunSync()
    private val fonts    = Ref.of[IO, List[FontLoader.FontConfig]](Nil).unsafeRunSync()
    private val root     = Files.createTempDirectory("settings-preview-spec")

    final private class RecordingSessionPersistence
        extends SessionPersistence(
          SessionManager.create(root, AppThemeManager.create, NoOpLogger.impl[IO], SessionManager.SessionPolicy()),
          SessionManager.SessionPolicy()
        ):
      override def maybeSaveSession(appState: AppState, trigger: SessionSaveTrigger): IO[Unit] =
        saves.update(_ :+ trigger)

    private val editor = new EffectEditorPort:
      def enqueueEvent(event: Event): IO[Unit]                               = IO.unit
      def commitState(newState: AppState, fallbackState: AppState): IO[Unit] = stateRef.set(newState)
      def updateModelValidated(transition: Model => Option[Model]): IO[Unit] =
        modelRef.get.flatMap(model => transition(model).fold(IO.unit)(modelRef.set))
      def scheduleDocumentAnalysis(): IO[Unit]                                               = IO.unit
      def scheduleFindSearch(request: FindSearchRequest): IO[Unit]                           = IO.unit
      def submitEffect(lane: com.serenity.state.effects.Lane.Keyed, job: IO[Unit]): IO[Unit] = job
      def dispatchEffectResult(result: EffectResult, onApplied: AppState => IO[Unit]): IO[Unit] =
        IO.unit

    private val configEffects = new StateManagerConfigEffects(
      stateRef.get,
      NoOpLogger.impl[IO],
      Some(root.resolve("config.json")),
      new RecordingSessionPersistence,
      fontConfig => fonts.update(_ :+ fontConfig),
      IO.pure(1.0),
      editor,
      RenderCaches.create(),
      saveConfig = (config, _) => writes.update(_ :+ config).as(Right(()): Either[ConfigError, Unit])
    )

    def state: AppState                          = stateRef.get.unsafeRunSync()
    def config: AppConfig                        = state.persisted.config
    def configWrites: List[AppConfig]            = writes.get.unsafeRunSync()
    def sessionSaves: Int                        = saves.get.unsafeRunSync().size
    def loadedFonts: List[FontLoader.FontConfig] = fonts.get.unsafeRunSync()

    def runner: CommandRunner =
      state.commandRunnerSurface
        .flatMap(_.content match
          case SurfaceContent.CommandPalette(runner) => Some(runner)
          case _                                     => None)
        .getOrElse(fail("Expected the command runner to be open"))

    def runnerIsOpen: Boolean = state.commandRunnerSurface.isDefined

    def press(event: CommandRunnerEvent): Fixture =
      val result = CommandRunnerReducer.reduce(event, state, registry)
      (stateRef.set(result.state) >> result.effects.traverse_(interpret)).unsafeRunSync()
      this

    def right: Fixture  = press(RunnerNavigate(Direction.Right))
    def left: Fixture   = press(RunnerNavigate(Direction.Left))
    def down: Fixture   = press(RunnerNavigate(Direction.Down))
    def up: Fixture     = press(RunnerNavigate(Direction.Up))
    def enter: Fixture  = press(RunnerSubmit)
    def escape: Fixture = press(RunnerDismiss)

    private def interpret(effect: AppEffect): IO[Unit] =
      effect match
        case AppEffect.ExecuteCommand(command)                => execute(command)
        case AppEffect.ExecuteCommandUnrecorded(command)      => execute(command)
        case AppEffect.Settings(SettingsEffect.ReapplyConfig) => configEffects.reapplyConfig
        case _                                                => IO.unit

    private def execute(command: Command): IO[Unit] =
      command.intent match
        case CommandIntent.Settings(intent) => configEffects.interpret(intent, state)
        case _                              => IO.unit

  private def openOn(groupId: String, itemId: String): Fixture =
    val opened = CommandRunner.empty.copy(fontFamilies = catalog).activate(registry, AppConfig.default).openSettings
    val index  = opened.submenuItems(groupId).indexWhere(_.id == itemId).max(0)
    val runner = opened.withDrilledSettingsSurface(SettingsSurfaceState(SettingsPage.Group(groupId, index)))
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    new Fixture(
      AppState(
        persisted = Persisted(layout = Layout.empty, buffers = Map.empty, focus = Focus.Surface(surface.id)),
        runtime = Runtime(uiSurfaces = List(surface))
      )
    )

  private def openOnSearchResult(query: String): Fixture =
    given CommandRegistry = registry
    val runner =
      CommandRunner.empty.copy(fontFamilies = catalog).activate(registry, AppConfig.default).updateSearchTerm(query)
    val surface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    new Fixture(
      AppState(
        persisted = Persisted(layout = Layout.empty, buffers = Map.empty, focus = Focus.Surface(surface.id)),
        runtime = Runtime(uiSurfaces = List(surface))
      )
    )

  private val defaultFontSize = AppConfig.default.editorConfig.fontConfig.fontSize

  "A numeric setting" should "preview a step without writing the config or the session" in {
    val fixture = openOn("settings-code-font", "code-font-size").right

    fixture.config.editorConfig.fontConfig.fontSize should be > defaultFontSize
    fixture.configWrites shouldBe empty
    fixture.sessionSaves shouldBe 0
  }

  it should "restore the original value on Escape without closing the runner or writing" in {
    val fixture = openOn("settings-code-font", "code-font-size").right.right.escape

    fixture.config.editorConfig.fontConfig.fontSize shouldBe defaultFontSize
    fixture.runnerIsOpen shouldBe true
    fixture.configWrites shouldBe empty
    fixture.sessionSaves shouldBe 0
    fixture.loadedFonts.lastOption.map(_.fontSize) shouldBe Some(defaultFontSize)
  }

  it should "commit the previewed value with one config write on Enter" in {
    val fixture   = openOn("settings-code-font", "code-font-size").right.right
    val previewed = fixture.config.editorConfig.fontConfig.fontSize

    fixture.enter

    fixture.config.editorConfig.fontConfig.fontSize shouldBe previewed
    fixture.configWrites.map(_.editorConfig.fontConfig.fontSize) shouldBe List(previewed)
    fixture.sessionSaves shouldBe 1
  }

  "A boolean setting" should "preview a flip, then put it back on Escape" in {
    val fixture = openOn("settings-text-display", "line-wrap").right

    fixture.config.surfaceConfig.wordWrapEnabled shouldBe false
    fixture.configWrites shouldBe empty

    fixture.escape

    fixture.config.surfaceConfig.wordWrapEnabled shouldBe AppConfig.default.surfaceConfig.wordWrapEnabled
    fixture.configWrites shouldBe empty
    fixture.sessionSaves shouldBe 0
  }

  it should "commit a previewed flip once on Enter" in {
    val fixture = openOn("settings-text-display", "line-wrap").right.enter

    fixture.config.surfaceConfig.wordWrapEnabled shouldBe false
    fixture.configWrites.map(_.surfaceConfig.wordWrapEnabled) shouldBe List(false)
  }

  "A font family picker" should "preview the highlighted family and restore the original on Escape" in {
    val original = AppConfig.default.editorConfig.fontConfig.codeFontFamily
    val fixture  = openOn("code-font", "code-font-0-mono-a").down

    fixture.config.editorConfig.fontConfig.codeFontFamily shouldBe "Mono B"
    fixture.down.config.editorConfig.fontConfig.codeFontFamily shouldBe "Mono C"
    fixture.configWrites shouldBe empty

    fixture.escape

    fixture.config.editorConfig.fontConfig.codeFontFamily shouldBe original
    fixture.configWrites shouldBe empty
    fixture.sessionSaves shouldBe 0
  }

  it should "commit the highlighted family on Enter, writing the config once" in {
    val fixture = openOn("code-font", "code-font-0-mono-a").down.down.up.enter

    fixture.config.editorConfig.fontConfig.codeFontFamily shouldBe "Mono B"
    fixture.configWrites.map(_.editorConfig.fontConfig.codeFontFamily) shouldBe List("Mono B")
  }

  "Switching between settings while previewing" should "drop the first preview and keep the second" in {
    val fixture = openOn("settings-code-font", "code-font-size").right.up

    fixture.config.editorConfig.fontConfig.fontSize shouldBe defaultFontSize
    fixture.configWrites shouldBe empty

    fixture.right

    fixture.config.editorConfig.fontConfig.codeLigatures shouldBe false
    fixture.configWrites shouldBe empty

    fixture.escape

    fixture.config shouldBe AppConfig.default
    fixture.configWrites shouldBe empty
  }

  it should "commit only the setting that was previewed last" in {
    val fixture = openOn("settings-code-font", "code-font-size").right.up.right.enter

    fixture.config.editorConfig.fontConfig.fontSize shouldBe defaultFontSize
    fixture.config.editorConfig.fontConfig.codeLigatures shouldBe false
    fixture.configWrites.size shouldBe 1
  }

  "A setting found by search" should "cycle inline with Left and Right, previewing each value" in {
    val fixture = openOnSearchResult("\"line-wrap\"")

    fixture.runner.selectedItem.map(_.id) shouldBe Some("settings-search:line-wrap")

    fixture.right

    fixture.config.surfaceConfig.wordWrapEnabled shouldBe false
    fixture.configWrites shouldBe empty

    fixture.escape

    fixture.config.surfaceConfig.wordWrapEnabled shouldBe true
    fixture.runnerIsOpen shouldBe true
  }

  it should "commit the previewed value on Enter instead of opening the setting" in {
    val fixture = openOnSearchResult("\"line-wrap\"").right.enter

    fixture.config.surfaceConfig.wordWrapEnabled shouldBe false
    fixture.configWrites.size shouldBe 1
    fixture.runnerIsOpen shouldBe true
  }
