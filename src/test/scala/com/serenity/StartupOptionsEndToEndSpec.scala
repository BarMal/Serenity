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

class StartupOptionsEndToEndSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  behavior of "Startup Options End-to-End"

  private case class TestFileDialog(openSelection: Option[java.nio.file.Path]) extends FileDialog:
    override def chooseOpenFile(initialDirectory: Option[java.nio.file.Path]): IO[Option[java.nio.file.Path]] =
      IO.pure(openSelection)

    override def chooseSaveFile(
      initialDirectory: Option[java.nio.file.Path],
      suggestedFileName: Option[String]
    ): IO[Option[java.nio.file.Path]] =
      IO.pure(None)

  it should "handle available startup actions correctly" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val selectedFile = java.nio.file.Files.createTempFile("serenity-startup-options-open", ".txt")
    java.nio.file.Files.writeString(selectedFile, "opened from startup options")

    val program = for
      // Test Option 1: New Session
      stateManager1 <- createStateManagerIO("StartupOptionsEndToEndSpec")
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)
      _ <- AppStartup.initializeState(stateManager1, theme, viewportSize)
      // Option 1 is selected by default, just press Enter
      _               <- stateManager1.applyEvent(Enter)
      newSessionState <- stateManager1.getCurrentState

      _ =
        newSessionState.startPageSurface shouldBe None
        newSessionState.layout.editorPanes should not be empty
        newSessionState.buffers should not be empty
        newSessionState.focus should matchPattern { case Focus.EditorPane(_) => }

      // Test Option 2: Open File
      stateManager3 <- createStateManagerIO(
        "StartupOptionsEndToEndSpec",
        fileDialog = TestFileDialog(Some(selectedFile))
      )
      _             <- AppStartup.initializeState(stateManager3, theme, viewportSize)
      _             <- stateManager3.applyEvent(MoveDown) // Move to option 2
      _             <- stateManager3.applyEvent(Enter)
      openFileState <- stateManager3.getCurrentState

      _ =
        openFileState.startPageSurface shouldBe None
        openFileState.modalSurface shouldBe None
        openFileState.buffers.values.find(_.filePath.contains(selectedFile)).map(_.content.collect()) shouldBe Some(
          "opened from startup options"
        )
        openFileState.focus should matchPattern { case Focus.EditorPane(_) => }
    yield succeed

    program.unsafeRunSync()
    java.nio.file.Files.deleteIfExists(selectedFile)
  }

  it should "handle navigation between all options correctly" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupOptionsEndToEndSpec")
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _ <- AppStartup.initializeState(stateManager, theme, viewportSize)

      // Test full navigation cycle
      // Start at option 0
      state0 <- stateManager.getCurrentState
      startPage0 = state0.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage0.selectedIndex shouldBe 0

      // Move down to option 1
      _      <- stateManager.applyEvent(MoveDown)
      state1 <- stateManager.getCurrentState
      startPage1 = state1.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage1.selectedIndex shouldBe 1

      // Move down to option 2
      _      <- stateManager.applyEvent(MoveDown)
      state2 <- stateManager.getCurrentState
      startPage2 = state2.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage2.selectedIndex shouldBe 2

      // Move through the workflow choices.
      _         <- stateManager.applyEvent(MoveDown)
      _         <- stateManager.applyEvent(MoveDown)
      stateLast <- stateManager.getCurrentState
      lastPage = stateLast.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _        = lastPage.selectedIndex shouldBe 4

      // Move down again (should wrap to option 0)
      _      <- stateManager.applyEvent(MoveDown)
      state3 <- stateManager.getCurrentState
      startPage3 = state3.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage3.selectedIndex shouldBe 0

      // Move up (should wrap to the final action)
      _      <- stateManager.applyEvent(MoveUp)
      state4 <- stateManager.getCurrentState
      startPage4 = state4.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage4.selectedIndex shouldBe 4
    yield succeed

    program.unsafeRunSync()
  }

  it should "start an editor session when a workflow preset is chosen" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupOptionsEndToEndSpec-workflow")
      _ <- AppStartup.initializeState(stateManager, Theme.default, ViewportSize(80, 24))
      _ <- stateManager.applyEvent(MoveDown)
      _ <- stateManager.applyEvent(MoveDown)
      _ <- stateManager.applyEvent(Enter)
      state <- stateManager.getCurrentState
    yield state

    val state = program.unsafeRunSync()
    state.startPageSurface shouldBe None
    state.layout.editorPanes should not be empty
    state.buffers should not be empty
    state.focus should matchPattern { case Focus.EditorPane(_) => }
  }
