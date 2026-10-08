package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.io.FileDialog
import com.serenity.keystroke.events.*
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

class StartupCommandsSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  behavior of "Startup Commands"

  private def testFileDialog(
    openSelection: Option[java.nio.file.Path],
    folderSelection: Option[java.nio.file.Path] = None
  ): FileDialog =
    FileDialog(
      chooseOpenFile = _ => IO.pure(openSelection),
      chooseSaveFile = (_, _) => IO.pure(None),
      chooseFolder = _ => IO.pure(folderSelection)
    )

  private def explorerRoots(state: AppState): List[java.nio.file.Path] =
    state.pinnedSurfaces.map(_.content).collect { case SurfaceContent.DirectoryTree(tree, _, _) => tree.rootPath }

  private def openForm(state: AppState): Option[FileWorkflowState] =
    state.topModal.map(_.modal).collect { case Modal.FileWorkflow(workflow) => workflow }

  private def startPageOf(state: AppState): StartupPage =
    state.startPageSurface
      .map(_.content)
      .collect { case SurfaceContent.StartPage(page) => page }
      .getOrElse(fail("no start page"))

  it should "open the selected native-dialog file when Open file is selected" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val selectedFile = java.nio.file.Files.createTempFile("serenity-startup-open", ".txt")
    java.nio.file.Files.writeString(selectedFile, "opened from startup")

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec", fileDialog = Some(testFileDialog(Some(selectedFile))))
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _ <- AppStartup.initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        theme,
        viewportSize
      )
      _ <- stateManager.applyEvent(MoveDown)

      stateAfterNav <- stateManager.getCurrentState
      startPage = stateAfterNav.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _         = startPage.selectedIndex shouldBe 1

      _          <- stateManager.applyEvent(Enter)
      finalState <- awaitOpened(stateManager, selectedFile)
    yield
      finalState.startPageSurface shouldBe None
      finalState.modalSurface shouldBe None
      finalState.persisted.buffers.values
        .find(_.document.filePath.contains(selectedFile))
        .map(_.document.content.collect()) shouldBe Some(
        "opened from startup"
      )
      finalState.persisted.focus should matchPattern { case Focus.EditorPane(_) => }

    program.unsafeRunSync()
    java.nio.file.Files.deleteIfExists(selectedFile)
  }

  it should "keep the startup page focused when the native open-file dialog is cancelled" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec", fileDialog = Some(testFileDialog(None)))
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _ <- AppStartup.initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        theme,
        viewportSize
      )
      _          <- stateManager.applyEvent(MoveDown)
      _          <- stateManager.applyEvent(Enter)
      finalState <- stateManager.getCurrentState
    yield
      finalState.startPageSurface should not be None
      finalState.modalSurface shouldBe None
      finalState.persisted.focus match
        case Focus.Surface(_) => succeed
        case other            => fail(s"Expected startup page focus, got $other")

    program.unsafeRunSync()
  }

  it should "list Open file and Open folder as separate actions, each with its own digit key" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec")
      _ <- AppStartup.initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        ViewportSize(80, 24)
      )
      state <- stateManager.getCurrentState
    yield startPageOf(state).actions.map(action => (action.id, action.label, action.shortcut)) shouldBe List(
      ("new-session", "New document", Some('1')),
      ("open-file", "Open file", Some('2')),
      ("open-folder", "Open folder", Some('3'))
    )

    program.unsafeRunSync()
  }

  it should "pin the Explorer to the folder chosen in the native dialog and leave the start page" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val folder = java.nio.file.Files.createTempDirectory("serenity-startup-folder")

    val program = for
      stateManager <- createStateManagerIO(
        "StartupCommandsSpec",
        fileDialog = Some(testFileDialog(None, folderSelection = Some(folder)))
      )
      _ <- AppStartup.initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        ViewportSize(80, 24)
      )
      _          <- stateManager.applyEvent(MoveDown)
      _          <- stateManager.applyEvent(MoveDown)
      _          <- stateManager.applyEvent(Enter)
      finalState <- awaitState(stateManager)(explorerRoots(_).contains(folder))
    yield
      finalState.startPageSurface shouldBe None
      finalState.modalSurface shouldBe None
      explorerRoots(finalState) shouldBe List(folder)
      finalState.persisted.buffers.values.flatMap(_.document.filePath) shouldBe empty
      finalState.persisted.focus should matchPattern { case Focus.EditorPane(_) => }

    program.unsafeRunSync()
    java.nio.file.Files.deleteIfExists(folder)
  }

  it should "choose a folder from the digit key too" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val folder = java.nio.file.Files.createTempDirectory("serenity-startup-folder-key")

    val program = for
      stateManager <- createStateManagerIO(
        "StartupCommandsSpec",
        fileDialog = Some(testFileDialog(None, folderSelection = Some(folder)))
      )
      _ <- AppStartup.initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        ViewportSize(80, 24)
      )
      _          <- stateManager.applyEvent(InsertChar('3'))
      finalState <- awaitState(stateManager)(explorerRoots(_).contains(folder))
    yield finalState.startPageSurface shouldBe None

    program.unsafeRunSync()
    java.nio.file.Files.deleteIfExists(folder)
  }

  it should "leave the start page and state unchanged when the native folder dialog is cancelled" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec", fileDialog = Some(testFileDialog(None, None)))
      _ <- AppStartup.initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        ViewportSize(80, 24)
      )
      _      <- stateManager.applyEvent(MoveDown)
      _      <- stateManager.applyEvent(MoveDown)
      before <- stateManager.getCurrentState
      _      <- stateManager.applyEvent(Enter) >> stateManager.runtimeLifecycle.awaitEffects
      after  <- stateManager.getCurrentState
    yield
      startPageOf(after).selectedIndex shouldBe 2
      after shouldBe before

    program.unsafeRunSync()
  }

  it should "open the in-app Open form for Open folder when there is no native dialog, and open its folder as the root" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val folder = java.nio.file.Files.createTempDirectory("serenity-startup-folder-tui")

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec")
      _ <- AppStartup.initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        ViewportSize(80, 24)
      )
      _     <- stateManager.applyEvent(MoveDown)
      _     <- stateManager.applyEvent(MoveDown)
      _     <- stateManager.applyEvent(Enter)
      shown <- stateManager.getCurrentState
      _ = openForm(shown).map(_.mode) shouldBe Some(FileWorkflowMode.Open)
      _ <- stateManager.updateState(state =>
        state.copy(runtime =
          state.runtime.copy(modalStack =
            state.runtime.modalStack.map(dialog =>
              dialog.modal match
                case Modal.FileWorkflow(workflow) =>
                  dialog.copy(modal = Modal.FileWorkflow(workflow.updated(path = folder.toString)))
                case _ => dialog
            )
          )
        )
      )
      _          <- stateManager.applyEvent(ModalOpenAsProjectRoot) >> stateManager.runtimeLifecycle.awaitEffects
      finalState <- stateManager.getCurrentState
    yield
      finalState.topModal shouldBe None
      finalState.startPageSurface shouldBe None
      explorerRoots(finalState) shouldBe List(folder)
      finalState.persisted.focus should matchPattern { case Focus.EditorPane(_) => }

    program.unsafeRunSync()
    java.nio.file.Files.deleteIfExists(folder)
  }

  it should "ignore an unavailable Restore shortcut when no saved session exists" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec")
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _ <- AppStartup.initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        theme,
        viewportSize
      )
      _          <- stateManager.applyEvent(InsertChar('9'))
      finalState <- stateManager.getCurrentState
    yield
      finalState.startPageSurface shouldBe defined
      finalState.persisted.focus match
        case Focus.Surface(_) => succeed
        case other            => fail(s"Expected startup surface focus, got $other")

    program.unsafeRunSync()
  }

  it should "include the new buffer in bufferOrder after starting a new session" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec")
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _ <- AppStartup.initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        theme,
        viewportSize
      )
      _          <- stateManager.applyEvent(Enter) // Select first option: New Session
      finalState <- stateManager.getCurrentState
    yield
      finalState.persisted.bufferOrder should not be empty
      finalState.persisted.buffers.keys.toList.foreach(bufferId =>
        finalState.persisted.bufferOrder should contain(bufferId)
      )

    program.unsafeRunSync()
  }
