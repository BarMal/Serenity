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

  private def testFileDialog(openSelection: Option[java.nio.file.Path]): FileDialog =
    FileDialog(
      chooseOpenFile = _ => IO.pure(openSelection),
      chooseSaveFile = (_, _) => IO.pure(None)
    )

  it should "handle available startup actions correctly" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val selectedFile = java.nio.file.Files.createTempFile("serenity-startup-options-open", ".txt")
    java.nio.file.Files.writeString(selectedFile, "opened from startup options")

    val program = for
      // Test Option 1: New Session
      stateManager1 <- createStateManagerIO("StartupOptionsEndToEndSpec")
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)
      _ <- AppStartup.initializeState(
        stateManager1,
        stateManager1.sessionStartupInfo,
        theme,
        viewportSize
      )
      // Option 1 is selected by default, just press Enter
      _               <- stateManager1.applyEvent(Enter)
      newSessionState <- stateManager1.getCurrentState

      _ =
        newSessionState.startPageSurface shouldBe None
        newSessionState.persisted.layout.editorPanes should not be empty
        newSessionState.persisted.buffers should not be empty
        newSessionState.persisted.focus should matchPattern { case Focus.EditorPane(_) => }

      // Test Option 2: Open File
      stateManager3 <- createStateManagerIO(
        "StartupOptionsEndToEndSpec",
        fileDialog = Some(testFileDialog(Some(selectedFile)))
      )
      _ <- AppStartup.initializeState(
        stateManager3,
        stateManager3.sessionStartupInfo,
        theme,
        viewportSize
      )
      _             <- stateManager3.applyEvent(MoveDown) // Move to option 2
      _             <- stateManager3.applyEvent(Enter)
      openFileState <- stateManager3.getCurrentState

      _ =
        openFileState.startPageSurface shouldBe None
        openFileState.modalSurface shouldBe None
        openFileState.persisted.buffers.values
          .find(_.document.filePath.contains(selectedFile))
          .map(_.document.content.collect()) shouldBe Some(
          "opened from startup options"
        )
        openFileState.persisted.focus should matchPattern { case Focus.EditorPane(_) => }
    yield succeed

    program.unsafeRunSync()
    java.nio.file.Files.deleteIfExists(selectedFile)
  }

  it should "keep the session choice available when a workflow preset is chosen" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupOptionsEndToEndSpec-workflow")
      _ <- AppStartup.initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        ViewportSize(80, 24)
      )
      _     <- stateManager.applyEvent(MoveDown)
      _     <- stateManager.applyEvent(MoveDown)
      _     <- stateManager.applyEvent(Enter)
      state <- stateManager.getCurrentState
    yield state

    val state = program.unsafeRunSync()
    state.startPageSurface shouldBe defined
    state.startPageSurface.map(_.content) should not be empty
  }
