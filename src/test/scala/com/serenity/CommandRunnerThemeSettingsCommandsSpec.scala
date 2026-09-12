package com.serenity

import java.awt.Font
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.{ConfigManager, SpellCheckConfig}
import com.serenity.io.FileDialog
import com.serenity.keystroke.events.*
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.ThemeConfigLoader
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class CommandRunnerThemeSettingsCommandsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def testFileDialog(
    openSelection: Option[Path] = None,
    saveSelection: Option[Path] = None
  ): FileDialog =
    FileDialog(
      chooseOpenFile = _ => IO.pure(openSelection),
      chooseSaveFile = (_, _) => IO.pure(saveSelection)
    )

  private def createStateManager(
    sessionRootOverride: Option[Path] = None,
    configPersistencePath: Option[Path] = None,
    fileDialog: Option[FileDialog] = None
  ): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("CommandRunnerThemeSettingsCommandsSpec"))
    StateManager
      .apply(
        logger,
        sessionRootOverride = sessionRootOverride,
        configPersistencePath = configPersistencePath,
        fileDialog = fileDialog
      )
      .unsafeRunSync()

  private def executeCommandThroughRunner(
    stateManager: StateManager,
    searchTerm: String,
    expectedCommandName: String
  ): Unit =
    val beforeOpen = stateManager.getCurrentState.unsafeRunSync()
    if beforeOpen.commandRunnerSurface
          .flatMap {
            _.content match
              case SurfaceContent.CommandPalette(runner) => Some(runner.isActive)
              case _                                     => None
          }
          .getOrElse(false) == false
    then stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    searchTerm.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => runner.selectedCommand.map(_.name)
        case _                                     => None
    } shouldBe Some(expectedCommandName)

    stateManager.applyEvent(Enter).unsafeRunSync()

  private def awaitDiagnosticMessages(
    stateManager: StateManager,
    uri: String,
    expectedMessages: List[String],
    attempts: Int = 40
  ): List[String] =
    def readMessages: IO[List[String]] =
      stateManager.getCurrentState.map(_.runtime.diagnosticsState.diagnostics.getOrElse(uri, Nil).map(_.message))

    def loop(remaining: Int): IO[List[String]] =
      readMessages.flatMap { messages =>
        if messages == expectedMessages || remaining <= 0 then IO.pure(messages)
        else IO.sleep(100.millis) >> loop(remaining - 1)
      }

    loop(attempts).unsafeRunSync()


  "Command runner" should "toggle between dark and light themes for the toggle-theme command" in {
    val stateManager = createStateManager()
    val initialState = stateManager.getCurrentState.unsafeRunSync()

    initialState.persisted.theme.name shouldBe "dark"

    executeCommandThroughRunner(stateManager, "toggle-theme", "toggle-theme")

    val lightState = stateManager.getCurrentState.unsafeRunSync()
    lightState.commandRunnerSurface shouldBe None
    lightState.persisted.theme.name shouldBe "light"

    executeCommandThroughRunner(stateManager, "toggle-theme", "toggle-theme")

    val darkState = stateManager.getCurrentState.unsafeRunSync()
    darkState.persisted.theme.name shouldBe "dark"
  }

  it should "reload the current theme for the reload-theme command" in {
    val stateManager = createStateManager()

    stateManager
      .updateState(state => state.copy(persisted = state.persisted.copy(theme = com.serenity.ui.theme.Theme.light)))
      .unsafeRunSync()

    executeCommandThroughRunner(stateManager, "reload-theme", "reload-theme")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.persisted.theme.name shouldBe "light"
  }

  it should "export the current theme through the native save-file dialog" in {
    val targetPath   = Files.createTempDirectory("serenity-theme-export").resolve("quiet-focus.conf")
    val stateManager = createStateManager(fileDialog = Some(testFileDialog(saveSelection = Some(targetPath))))
    val theme = Theme.light.copy(
      name = "quiet-focus",
      background = new java.awt.Color(0x112233),
      panelBorder = new java.awt.Color(0x445566)
    )
    stateManager.updateState(state => state.copy(persisted = state.persisted.copy(theme = theme))).unsafeRunSync()

    executeCommandThroughRunner(stateManager, "export-theme", "export-theme")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    Files.exists(targetPath) shouldBe true
    val loaded = ThemeConfigLoader().loadThemeFromFile(targetPath).unsafeRunSync()
    loaded.name shouldBe "quiet-focus"
    loaded.ui.background shouldBe "#112233"
    loaded.ui.panelBorder shouldBe Some("#445566")
  }

  it should "enable spell-checking from the command runner and refresh diagnostics asynchronously" in {
    val stateManager = createStateManager()
    val bufferId     = BufferId(0)

    stateManager
      .updateState { state =>
        val buffer =
          state.persisted
            .buffers(bufferId)
            .copy(document = state.persisted.buffers(bufferId).document.copy(content = com.serenity.rope.Rope("wurld")))
        state.copy(persisted =
          state.persisted.copy(
            buffers = state.persisted.buffers + (bufferId -> buffer),
            config = state.persisted.config.withSpellCheck(SpellCheckConfig(enabled = false))
          )
        )
      }
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().runtime.diagnosticsState.diagnostics shouldBe empty

    executeCommandThroughRunner(stateManager, "spellcheck-on", "spellcheck-on")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()

    updatedState.persisted.config.languageToolsConfig.spellCheck.enabled shouldBe true

    awaitDiagnosticMessages(
      stateManager,
      SpellChecker.bufferDiagnosticsUri(bufferId),
      List("Possible spelling issue: wurld")
    ) shouldBe List("Possible spelling issue: wurld")
  }

  it should "write the current upgraded config from the command runner" in {
    val configFile   = Files.createTempDirectory("serenity-save-config").resolve("config.conf")
    val stateManager = createStateManager(configPersistencePath = Some(configFile))

    executeCommandThroughRunner(stateManager, "save-config", "save-config")

    val saved = Files.readString(configFile)
    saved should include("config.version = 1")
    saved should include("ui.motion.preset = smooth")
    stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface shouldBe None
  }

  it should "set command runner visible rows from a typed settings command" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "command-runner-visible-rows",
          "Set command runner visible rows.",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerVisibleRows(Some(9)))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.commandRunnerVisibleRows shouldBe Some(
      9
    )

    stateManager
      .executeCommand(
        Command.typed(
          "command-runner-visible-rows-auto",
          "Reset command runner visible rows.",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerVisibleRows(None))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.surfaceConfig.commandRunnerVisibleRows shouldBe None
  }

  it should "set command runner item and cursor gaps from typed settings commands" in {
    val stateManager = createStateManager()

    stateManager
      .executeCommand(
        Command.typed(
          "command-runner-item-gap-rows",
          "Set command runner item gaps.",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerItemGapRows(1))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "command-runner-cursor-gap-rows",
          "Set command runner cursor gap.",
          CommandIntent.Settings(SettingsIntent.Motion(MotionIntent.SetCommandRunnerCursorGapRows(Some(3)))),
          CommandCategory.Settings
        )
      )
      .unsafeRunSync()

    val config = stateManager.getCurrentState.unsafeRunSync().persisted.config
    config.surfaceConfig.commandRunnerItemGapRows shouldBe 1
    config.surfaceConfig.commandRunnerCursorGapRows shouldBe Some(3)
  }

  it should "apply a built-in writing preset from a searchable command" in {
    val stateManager = createStateManager()

    executeCommandThroughRunner(stateManager, "apply-writing-preset", "apply-writing-preset")

    val updatedState = stateManager.getCurrentState.unsafeRunSync()
    updatedState.commandRunnerSurface shouldBe None
    updatedState.persisted.config.editorConfig.fontConfig.textFontFamily shouldBe Font.SERIF
    updatedState.persisted.config.surfaceConfig.showLineNumbers shouldBe false
    updatedState.persisted.config.surfaceConfig.showGutter shouldBe false
    updatedState.persisted.config.surfaceConfig.showPaneHeaders shouldBe false
    updatedState.pinnedSurfaces shouldBe Nil
  }

  it should "persist the selected compact workflow for a later session" in {
    val configFile   = Files.createTempFile("serenity-compact-workflow", ".conf")
    val stateManager = createStateManager(configPersistencePath = Some(configFile))

    executeCommandThroughRunner(stateManager, "apply-compact-preset", "apply-compact-preset")

    val updated   = stateManager.getCurrentState.unsafeRunSync().persisted.config
    val persisted = ConfigManager.loadConfig(Some(configFile.toString))

    updated.surfaceConfig.showLineNumbers shouldBe true
    updated.surfaceConfig.showGutter shouldBe true
    updated.surfaceConfig.showPaneHeaders shouldBe true
    updated.surfaceConfig.wordWrapEnabled shouldBe false
    updated.surfaceConfig.contextualToolbarEnabled shouldBe false
    persisted.surfaceConfig.showLineNumbers shouldBe updated.surfaceConfig.showLineNumbers
    persisted.surfaceConfig.showGutter shouldBe updated.surfaceConfig.showGutter
    persisted.surfaceConfig.showPaneHeaders shouldBe updated.surfaceConfig.showPaneHeaders
    persisted.surfaceConfig.wordWrapEnabled shouldBe updated.surfaceConfig.wordWrapEnabled
    persisted.surfaceConfig.contextualToolbarEnabled shouldBe updated.surfaceConfig.contextualToolbarEnabled
  }
