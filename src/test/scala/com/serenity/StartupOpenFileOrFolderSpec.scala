package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.AppStartup
import com.serenity.command.{CommandAvailability, CommandIntent, CommandRegistry, FileIntent, SessionIntent}
import com.serenity.frontend.FrontendCapabilities
import com.serenity.io.FileDialog
import com.serenity.keystroke.events.*
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

/** macOS's single Open... that takes a file or a folder: what the start page offers, and where each choice goes. */
class StartupOpenFileOrFolderSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private val combined = FrontendCapabilities.gui.copy(opensFileOrFolder = true)

  private def combinedDialog(selection: Option[Path], plainFile: Option[Path] = None): FileDialog =
    FileDialog(
      chooseOpenFile = _ => IO.pure(plainFile),
      chooseSaveFile = (_, _) => IO.pure(None),
      chooseFolder = _ => IO.pure(None),
      chooseFileOrFolder = _ => IO.pure(selection),
      supportsFileOrFolder = true
    )

  private def startPageOf(state: AppState): StartupPage =
    state.startPageSurface
      .map(_.content)
      .collect { case SurfaceContent.StartPage(page) => page }
      .getOrElse(fail("no start page"))

  private def explorerRoots(state: AppState): List[Path] =
    state.pinnedSurfaces.map(_.content).collect { case SurfaceContent.DirectoryTree(tree, _, _) => tree.rootPath }

  private def started(stateManager: StateManager, capabilities: FrontendCapabilities): IO[AppState] =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    AppStartup.initializeState(
      stateManager,
      stateManager.sessionStartupInfo,
      Theme.default,
      ViewportSize(80, 24),
      capabilities = capabilities
    )

  "The start page where one Open... is supported" should "list New document, Open... and then the recent files" in {
    val recent = TestTemp.file("serenity-combined-recent", ".md")
    val page   = AppStartup.createStartPage(sessionExists = false, recentFiles = List(recent), fileOrFolderOpen = true)

    page.actions.map(_.id) shouldBe List("new-session", "open", s"recent:${recent.toAbsolutePath.normalize()}")
    page.actions.take(2).map(_.label) shouldBe List("New document", "Open...")
    page.actions.take(2).flatMap(_.shortcut) shouldBe List('1', '2')
    page.actions(1).command.intent shouldBe CommandIntent.Session(SessionIntent.StartupOpenFileOrFolder)
    Files.deleteIfExists(recent)
  }

  it should "keep the separate Open file and Open folder everywhere else" in {
    val page = AppStartup.createStartPage(sessionExists = false, recentFiles = Nil)

    page.actions.map(_.label) shouldBe List("New document", "Open file", "Open folder")
  }

  it should "be built from the frontend's capability at launch" in {
    val program = for
      stateManager <- createStateManagerIO("StartupOpenFileOrFolderSpec")
      state        <- started(stateManager, combined)
    yield startPageOf(state).actions.map(_.label) shouldBe List("New document", "Open...")

    program.unsafeRunSync()
  }

  "Open... on the start page" should "pin the Explorer to a chosen folder and leave the start page" in {
    val folder = TestTemp.directory("serenity-combined-folder")

    val program = for
      stateManager <- createStateManagerIO(
        "StartupOpenFileOrFolderSpec",
        fileDialog = Some(combinedDialog(Some(folder)))
      )
      _          <- started(stateManager, combined)
      _          <- stateManager.applyEvent(InsertChar('2'))
      finalState <- awaitState(stateManager)(explorerRoots(_).contains(folder))
    yield
      finalState.startPageSurface shouldBe None
      finalState.persisted.buffers.values.flatMap(_.document.filePath) shouldBe empty
      explorerRoots(finalState) shouldBe List(folder)

    program.unsafeRunSync()
    Files.deleteIfExists(folder)
  }

  it should "open a chosen file in a buffer, without pinning the Explorer" in {
    val file = TestTemp.file("serenity-combined-file", ".txt")
    Files.writeString(file, "from the combined dialog")

    val program = for
      stateManager <- createStateManagerIO(
        "StartupOpenFileOrFolderSpec",
        fileDialog = Some(combinedDialog(Some(file)))
      )
      _          <- started(stateManager, combined)
      _          <- stateManager.applyEvent(MoveDown)
      _          <- stateManager.applyEvent(Enter)
      finalState <- awaitOpened(stateManager, file)
    yield
      finalState.startPageSurface shouldBe None
      explorerRoots(finalState) shouldBe empty

    program.unsafeRunSync()
    Files.deleteIfExists(file)
  }

  it should "leave the start page unchanged when the dialog is cancelled" in {
    val program = for
      stateManager <- createStateManagerIO("StartupOpenFileOrFolderSpec", fileDialog = Some(combinedDialog(None)))
      _            <- started(stateManager, combined)
      _            <- stateManager.applyEvent(MoveDown)
      before       <- stateManager.getCurrentState
      _            <- stateManager.applyEvent(Enter) >> stateManager.runtimeLifecycle.awaitEffects
      after        <- stateManager.getCurrentState
    yield after shouldBe before

    program.unsafeRunSync()
  }

  it should "ask the combined dialog, not the file dialog, when the open hotkey is pressed" in {
    val file = TestTemp.file("serenity-combined-hotkey", ".txt")
    Files.writeString(file, "x")
    val plain = TestTemp.file("serenity-combined-plain", ".txt")

    val program = for
      stateManager <- createStateManagerIO(
        "StartupOpenFileOrFolderSpec",
        fileDialog = Some(combinedDialog(Some(file), plainFile = Some(plain)))
      )
      _     <- started(stateManager, combined)
      _     <- stateManager.applyEvent(OpenFile)
      state <- awaitOpened(stateManager, file)
    yield state.persisted.buffers.values.flatMap(_.document.filePath).toList shouldBe List(file)

    program.unsafeRunSync()
    Files.deleteIfExists(file)
    Files.deleteIfExists(plain)
  }

  "The Open... command on a dialog without a combined mode" should "fall back to choosing a file" in {
    val file = TestTemp.file("serenity-combined-fallback", ".txt")
    Files.writeString(file, "x")
    def dialogRecordingTo(asked: Ref[IO, List[String]]): FileDialog =
      FileDialog(
        chooseOpenFile = _ => asked.update(_ :+ "file").as(Some(file)),
        chooseSaveFile = (_, _) => IO.pure(None),
        chooseFolder = _ => IO.pure(None),
        chooseFileOrFolder = _ => asked.update(_ :+ "combined").as(None)
      )

    val program = for
      asked        <- Ref.of[IO, List[String]](Nil)
      stateManager <- createStateManagerIO("StartupOpenFileOrFolderSpec", fileDialog = Some(dialogRecordingTo(asked)))
      _            <- started(stateManager, FrontendCapabilities.gui)
      command = CommandRegistry.default.findCommand("open-file-or-folder").getOrElse(fail("no open-file-or-folder"))
      _      <- stateManager.executeCommand(command)
      _      <- awaitOpened(stateManager, file)
      asking <- asked.get
    yield asking shouldBe List("file")

    program.unsafeRunSync()
    Files.deleteIfExists(file)
  }

  "The Open... palette command" should "be hidden where there is no combined dialog and offered where there is" in {
    val command = CommandRegistry.default.findCommand("open-file-or-folder").getOrElse(fail("no open-file-or-folder"))
    val context = AppState.empty.commandRunnerContext

    command.intent shouldBe CommandIntent.File(FileIntent.OpenFileOrFolder)
    CommandAvailability.of(command, context).isOffered shouldBe false
    CommandAvailability.of(command, context.copy(opensFileOrFolder = true)).isRunnable shouldBe true
  }

  it should "leave Open File... and Open Folder... offered either way" in {
    for names <- List("open", "open-folder"); supported <- List(false, true) do
      val command = CommandRegistry.default.findCommand(names).getOrElse(fail(s"no $names"))
      val context = AppState.empty.commandRunnerContext.copy(opensFileOrFolder = supported)
      CommandAvailability.of(command, context).isRunnable shouldBe true
  }

  "The command context" should "take the combined dialog from the running frontend's capabilities" in {
    val state = AppState.empty
    state.commandRunnerContext.opensFileOrFolder shouldBe false
    state
      .copy(runtime = state.runtime.copy(capabilities = combined))
      .commandRunnerContext
      .opensFileOrFolder shouldBe true
  }
