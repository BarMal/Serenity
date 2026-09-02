package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.io.FileDialog
import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

class StartupCommandsSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  behavior of "Startup Commands"

  final private case class TestFileDialog(openSelection: Option[java.nio.file.Path]) extends FileDialog:
    override def chooseOpenFile(initialDirectory: Option[java.nio.file.Path]): IO[Option[java.nio.file.Path]] =
      IO.pure(openSelection)

    override def chooseSaveFile(
      initialDirectory: Option[java.nio.file.Path],
      suggestedFileName: Option[String]
    ): IO[Option[java.nio.file.Path]] =
      IO.pure(None)

  it should "open the selected native-dialog file when Open file is selected" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val selectedFile = java.nio.file.Files.createTempFile("serenity-startup-open", ".txt")
    java.nio.file.Files.writeString(selectedFile, "opened from startup")

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec", fileDialog = Some(TestFileDialog(Some(selectedFile))))
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _ <- AppStartup.initializeState(stateManager, theme, viewportSize)
      _ <- stateManager.applyEvent(MoveDown)

      stateAfterNav <- stateManager.getCurrentState
      startPage = stateAfterNav.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _         = startPage.selectedIndex shouldBe 1

      _          <- stateManager.applyEvent(Enter)
      finalState <- stateManager.getCurrentState
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
      stateManager <- createStateManagerIO("StartupCommandsSpec", fileDialog = Some(TestFileDialog(None)))
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _          <- AppStartup.initializeState(stateManager, theme, viewportSize)
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

  it should "ignore an unavailable Restore shortcut when no saved session exists" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec")
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _          <- AppStartup.initializeState(stateManager, theme, viewportSize)
      _          <- stateManager.applyEvent(InsertChar('3'))
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

      _          <- AppStartup.initializeState(stateManager, theme, viewportSize)
      _          <- stateManager.applyEvent(Enter) // Select first option: New Session
      finalState <- stateManager.getCurrentState
    yield
      finalState.persisted.bufferOrder should not be empty
      finalState.persisted.buffers.keys.toList.foreach(bufferId =>
        finalState.persisted.bufferOrder should contain(bufferId)
      )

    program.unsafeRunSync()
  }

  it should "place restore-session at index 2 when a session exists with no recent files" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec")
      _            <- stateManager.saveSession()
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      initialState <- AppStartup.initializeState(stateManager, theme, viewportSize)
      startPage = initialState.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
    yield
      startPage.launchActions.map(_.id) should contain("restore-session")
      startPage.launchActions(2).id shouldBe "restore-session"

    program.unsafeRunSync()
  }

  it should "dismiss the startup page and restore session when Enter is pressed at the restore-session action" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec")
      _            <- stateManager.saveSession()
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _          <- AppStartup.initializeState(stateManager, theme, viewportSize)
      _          <- stateManager.applyEvent(MoveDown)
      _          <- stateManager.applyEvent(MoveDown)
      navState   <- stateManager.getCurrentState
      startPage  = navState.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage.selectedIndex shouldBe 2
      _          = startPage.selectedAction.map(_.id) shouldBe Some("restore-session")

      _          <- stateManager.applyEvent(Enter)
      finalState <- stateManager.getCurrentState
    yield
      finalState.startPageSurface shouldBe None
      finalState.persisted.focus should matchPattern { case Focus.EditorPane(_) => }

    program.unsafeRunSync()
  }

  it should "dismiss the startup page and restore session when NewLine is pressed at the restore-session action" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    // NewLine is the event EditorInputTranslator produces for the Enter key (via the editor keymap binding),
    // which is what the running app dispatches. Enter and NewLine both map to FocusIntent.Submit in
    // SurfaceInput.intentOf, but production goes through NewLine — this test exercises that path.
    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec")
      _            <- stateManager.saveSession()
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _          <- AppStartup.initializeState(stateManager, theme, viewportSize)
      _          <- stateManager.applyEvent(MoveDown)
      _          <- stateManager.applyEvent(MoveDown)
      _          <- stateManager.applyEvent(NewLine)
      finalState <- stateManager.getCurrentState
    yield
      finalState.startPageSurface shouldBe None
      finalState.persisted.focus should matchPattern { case Focus.EditorPane(_) => }

    program.unsafeRunSync()
  }

  it should "restore the saved session content when the restore-session action is executed" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec")

      // Create a buffer with known content and save the session
      savedState   <- stateManager.getCurrentState
      savedBufferId = savedState.persisted.bufferOrder.head
      _ <- stateManager.updateState(s =>
             val buf = s.persisted.buffers(savedBufferId)
             val updated = buf.copy(document = buf.document.copy(content = com.serenity.rope.Rope("restored content")))
             s.copy(persisted = s.persisted.copy(buffers = s.persisted.buffers + (savedBufferId -> updated)))
           )
      _ <- stateManager.saveSession()

      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _          <- AppStartup.initializeState(stateManager, theme, viewportSize)
      _          <- stateManager.applyEvent(MoveDown)
      _          <- stateManager.applyEvent(MoveDown)
      _          <- stateManager.applyEvent(Enter)
      finalState <- stateManager.getCurrentState
    yield
      val restoredContent = finalState.persisted.buffers.values
        .map(_.document.content.collect())
        .find(_ == "restored content")
      restoredContent shouldBe Some("restored content")

    program.unsafeRunSync()
  }
