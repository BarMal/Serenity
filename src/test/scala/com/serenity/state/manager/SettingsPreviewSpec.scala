package com.serenity.state.manager

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.TestTemp
import com.serenity.command.*
import com.serenity.config.AppConfigOps.*
import com.serenity.config.{AppConfig, AppMode, ConfigError, ConfigManager}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, CommandRunnerReducer, SettingsEffect}
import com.serenity.state.undo.UndoState
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** A settings value changed from the command runner is a preview until it is committed: nothing reaches the config file
  * until Enter, and Escape puts back what was there when the preview began. These specs drive the real reducer and the
  * real config interpreter side by side, counting what is written.
  */
class SettingsPreviewSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.default
  private val catalog =
    FontLoader.FontFamilyCatalog(monospace = List("Mono A", "Mono B", "Mono C"), text = List("Serif A"), ui = Nil)

  final private class Fixture(
      initial: AppState,
      configOnDisk: Option[AppConfig] = None,
      deferConfigJobs: Boolean = false
  ):
    private val modelRef = Ref.of[IO, Model](Model(initial, UndoState())).unsafeRunSync()
    private val stateRef = ModelViews.appRef(modelRef)
    private val writes   = Ref.of[IO, List[AppConfig]](Nil).unsafeRunSync()
    private val notices  = Ref.of[IO, List[Notice]](Nil).unsafeRunSync()
    private val queued   = Ref.of[IO, List[IO[Unit]]](Nil).unsafeRunSync()
    private val executed = Ref.of[IO, List[AppEffect]](Nil).unsafeRunSync()
    private val fonts    = Ref.of[IO, List[FontLoader.FontConfig]](Nil).unsafeRunSync()
    private val root     = TestTemp.directory("settings-preview-spec")

    private val editor = new EffectEditorPort:
      def enqueueEvent(event: Event): IO[Unit]                               = IO.unit
      def commitState(newState: AppState, fallbackState: AppState): IO[Unit] = stateRef.set(newState)
      def updateModelValidated(transition: Model => Option[Model]): IO[Unit] =
        modelRef.get.flatMap(model => transition(model).fold(IO.unit)(modelRef.set))
      def scheduleDocumentAnalysis(): IO[Unit]                     = IO.unit
      def scheduleFindSearch(request: FindSearchRequest): IO[Unit] = IO.unit
      def submitEffect(lane: com.serenity.state.effects.Lane.Keyed, job: IO[Unit]): IO[Unit] =
        if deferConfigJobs then queued.update(_ :+ job) else job
      def dispatchEffectResult(result: EffectResult, onApplied: AppState => IO[Unit]): IO[Unit] =
        stateRef.get.flatMap { current =>
          val next = EffectResult.applyIfCurrent(current, result)
          if next eq current then IO.unit else stateRef.set(next) >> onApplied(next)
        }

    private val configEffects = new StateManagerConfigEffects(
      stateRef.get,
      NoOpLogger.impl[IO],
      Some(root.resolve("config.json")),
      fontConfig => fonts.update(_ :+ fontConfig),
      IO.pure(1.0),
      editor,
      RenderCaches.create(),
      saveConfig = (config, _) => writes.update(_ :+ config).as(Right(()): Either[ConfigError, Unit]),
      showNotice = notice => notices.update(_ :+ notice),
      configOnDisk = configOnDisk
    )

    def state: AppState                  = stateRef.get.unsafeRunSync()
    def config: AppConfig                = state.persisted.config
    def configWrites: List[AppConfig]    = writes.get.unsafeRunSync()
    def noticeTexts: List[String]        = notices.get.unsafeRunSync().map(_.message)
    def executedEffects: List[AppEffect] = executed.get.unsafeRunSync()
    def configFile: java.nio.file.Path   = root.resolve("config.json")

    def queuedConfigJobs: Int = queued.get.unsafeRunSync().size

    def runQueuedConfigJobs(): Unit =
      queued.getAndSet(Nil).flatMap(_.sequence_).unsafeRunSync()

    def editConfigFile(edited: AppConfig): Fixture =
      Files.writeString(configFile, ConfigManager.configToString(edited)): Unit
      this

    def reloadConfigFile(): Fixture =
      configEffects.watch.foreach(_.reload.unsafeRunSync())
      this

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
      executed.update(_ :+ effect) >> run(effect)

    private def run(effect: AppEffect): IO[Unit] =
      effect match
        case AppEffect.ExecuteCommand(command)                => execute(command)
        case AppEffect.ExecuteCommandUnrecorded(command)      => execute(command)
        case AppEffect.Settings(SettingsEffect.ReapplyConfig) => configEffects.reapplyConfig
        case _                                                => IO.unit

    private def execute(command: Command): IO[Unit] =
      command.intent match
        case CommandIntent.Settings(intent) => configEffects.interpret(intent, state)
        case _                              => IO.unit

  private def openOn(
    groupId: String,
    itemId: String,
    configOnDisk: Option[AppConfig] = None,
    deferConfigJobs: Boolean = false
  ): Fixture =
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
      ),
      configOnDisk,
      deferConfigJobs
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

  "A numeric setting" should "preview a step without writing the config" in {
    val fixture = openOn("settings-code-font", "code-font-size").right

    fixture.config.editorConfig.fontConfig.fontSize should be > defaultFontSize
    fixture.configWrites shouldBe empty
  }

  it should "restore the original value on Escape without closing the runner or writing" in {
    val fixture = openOn("settings-code-font", "code-font-size").right.right.escape

    fixture.config.editorConfig.fontConfig.fontSize shouldBe defaultFontSize
    fixture.runnerIsOpen shouldBe true
    fixture.configWrites shouldBe empty
    fixture.loadedFonts.lastOption.map(_.fontSize) shouldBe Some(defaultFontSize)
  }

  it should "commit the previewed value with one config write on Enter" in {
    val fixture   = openOn("settings-code-font", "code-font-size").right.right
    val previewed = fixture.config.editorConfig.fontConfig.fontSize

    fixture.enter

    fixture.config.editorConfig.fontConfig.fontSize shouldBe previewed
    fixture.configWrites.map(_.editorConfig.fontConfig.fontSize) shouldBe List(previewed)
  }

  "A boolean setting" should "preview a flip, then put it back on Escape" in {
    val fixture = openOn("settings-text-display", "line-wrap").right

    fixture.config.surfaceConfig.wordWrapEnabled shouldBe false
    fixture.configWrites shouldBe empty

    fixture.escape

    fixture.config.surfaceConfig.wordWrapEnabled shouldBe AppConfig.default.surfaceConfig.wordWrapEnabled
    fixture.configWrites shouldBe empty
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

  "A config write already requested when a preview begins" should "still write the committed value, not the preview" in {
    val fixture  = openOn("settings-code-font", "code-font-size", deferConfigJobs = true).right.enter
    val accepted = fixture.config.editorConfig.fontConfig.fontSize

    fixture.right

    fixture.config.editorConfig.fontConfig.fontSize should be > accepted
    fixture.runQueuedConfigJobs()

    fixture.configWrites.map(_.editorConfig.fontConfig.fontSize) shouldBe List(accepted)
  }

  "Requesting a config write" should "happen once for an accepted preview and never for the previews before it" in {
    val fixture = openOn("settings-code-font", "code-font-size", deferConfigJobs = true).right.right.right

    fixture.queuedConfigJobs shouldBe 0

    fixture.enter

    fixture.queuedConfigJobs shouldBe 1
  }

  "A config file reload while a preview is open" should "become what Escape returns to, not be undone by it" in {
    val fixture =
      openOn("settings-code-font", "code-font-size", configOnDisk = Some(AppConfig.default)).right
    val previewed = fixture.config.editorConfig.fontConfig.fontSize

    previewed should be > defaultFontSize

    fixture.editConfigFile(AppConfig.default.withWheelScrollLines(7)).reloadConfigFile()

    fixture.config.inputConfig.wheelScrollLines shouldBe 7
    fixture.escape

    fixture.config shouldBe AppConfig.default.withWheelScrollLines(7)
    fixture.configWrites shouldBe empty
  }

  it should "end the preview, since the value it was previewed against has changed" in {
    val fixture =
      openOn("settings-code-font", "code-font-size", configOnDisk = Some(AppConfig.default)).right

    fixture.editConfigFile(AppConfig.default.withWheelScrollLines(7)).reloadConfigFile()

    fixture.state.runtime.pendingSetting shouldBe None
    fixture.config.editorConfig.fontConfig.fontSize shouldBe defaultFontSize
    fixture.loadedFonts.lastOption.map(_.fontSize) shouldBe Some(defaultFontSize)
    fixture.noticeTexts shouldBe Nil
  }

  it should "win over the previewed value when it sets the very setting being previewed" in {
    val fixture =
      openOn("settings-code-font", "code-font-size", configOnDisk = Some(AppConfig.default)).right
    val edited = AppConfig.default.withFontConfig(AppConfig.default.editorConfig.fontConfig.copy(fontSize = 20.0f))

    fixture.editConfigFile(edited).reloadConfigFile()
    fixture.escape

    fixture.config.editorConfig.fontConfig.fontSize shouldBe 20.0f
    fixture.loadedFonts.lastOption.map(_.fontSize) shouldBe Some(20.0f)
    fixture.configWrites shouldBe empty
  }

  it should "leave a preview alone when the file is reloaded to what it already held" in {
    val fixture =
      openOn("settings-code-font", "code-font-size", configOnDisk = Some(AppConfig.default)).right
    val previewed = fixture.config.editorConfig.fontConfig.fontSize

    fixture.editConfigFile(AppConfig.default).reloadConfigFile()

    fixture.state.runtime.pendingSetting.isDefined shouldBe true
    fixture.config.editorConfig.fontConfig.fontSize shouldBe previewed
  }

  "A preview of the app mode" should "not exist: the mode is committed at once, because leaving code mode is not undone by Escape" in {
    val fixture = openOn("settings-workspace", "app-mode").right

    fixture.state.runtime.pendingSetting shouldBe None
    fixture.executedEffects.collect { case AppEffect.ExecuteCommandUnrecorded(command) => command } shouldBe Nil
    fixture.executedEffects.collect { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe
      List(CommandIntent.View(ViewIntent.SetAppMode(AppMode.Prose)))
  }

  it should "leave nothing for Escape to revert once the mode has been chosen" in {
    val fixture = openOn("settings-workspace", "app-mode").right

    fixture.escape

    fixture.executedEffects.collect { case AppEffect.Settings(SettingsEffect.ReapplyConfig) => () } shouldBe Nil
    fixture.configWrites shouldBe empty
  }
