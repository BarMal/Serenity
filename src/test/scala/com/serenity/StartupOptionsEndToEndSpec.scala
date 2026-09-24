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
      openFileState <- awaitOpened(stateManager3, selectedFile)

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

  it should "dismiss the splash and enter the editor when a workflow preset is chosen" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupOptionsEndToEndSpec-workflow")
      _ <- AppStartup.initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        ViewportSize(80, 24)
      )
      _ <- stateManager.applyEvent(InsertChar('w'))
      // The preset loads on the Presets lane and is applied once that settles (#1697).
      _     <- stateManager.runtimeLifecycle.awaitEffects
      state <- stateManager.getCurrentState
    yield state

    val state = program.unsafeRunSync()
    // Choosing a workflow preset from the splash must transition into the editor -- like new/restore/open-recent -- not
    // leave the (now stale) splash painted over the applied workspace, which looked like the option did nothing (#1524).
    state.startPageSurface shouldBe empty
    state.persisted.focus should matchPattern { case Focus.EditorPane(_) => }
    // ...and it must land in a real, empty, editable buffer -- from the splash the preset otherwise produced a
    // buffer-less pane, so the first keystroke had nowhere to go.
    state.persisted.buffers should not be empty
    val landedBuffer = state.persisted.layout.activeEditorPaneId
      .flatMap(state.persisted.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .flatMap(state.persisted.buffers.get)
    landedBuffer.map(_.document.content.collect()) shouldBe Some("")
  }

  it should "enter the editor when the Code workflow preset is chosen, even though it has no editor-pane target" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupOptionsEndToEndSpec-code")
      _ <- AppStartup.initializeState(
        stateManager,
        stateManager.sessionStartupInfo,
        Theme.default,
        ViewportSize(80, 24)
      )
      _ <- stateManager.applyEvent(InsertChar('c'))
      // The preset loads on the Presets lane and is applied once that settles (#1697).
      _     <- stateManager.runtimeLifecycle.awaitEffects
      state <- stateManager.getCurrentState
    yield state

    val state = program.unsafeRunSync()
    // Code docks a directory tree and sets no targetEditorPaneCount, so from the splash it previously left no editor
    // pane at all and the choice appeared to do nothing.
    state.startPageSurface shouldBe empty
    state.persisted.focus should matchPattern { case Focus.EditorPane(_) => }
    state.persisted.buffers should not be empty
  }
