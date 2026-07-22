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

  private case class TestFileDialog(openSelection: Option[java.nio.file.Path]) extends FileDialog:
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
      stateManager <- createStateManagerIO("StartupCommandsSpec", fileDialog = TestFileDialog(Some(selectedFile)))
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
      finalState.buffers.values.find(_.filePath.contains(selectedFile)).map(_.content.collect()) shouldBe Some(
        "opened from startup"
      )
      finalState.focus should matchPattern { case Focus.EditorPane(_) => }

    program.unsafeRunSync()
    java.nio.file.Files.deleteIfExists(selectedFile)
  }

  it should "keep the startup page focused when the native open-file dialog is cancelled" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupCommandsSpec", fileDialog = TestFileDialog(None))
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      _          <- AppStartup.initializeState(stateManager, theme, viewportSize)
      _          <- stateManager.applyEvent(MoveDown)
      _          <- stateManager.applyEvent(Enter)
      finalState <- stateManager.getCurrentState
    yield
      finalState.startPageSurface should not be None
      finalState.modalSurface shouldBe None
      finalState.focus match
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
      finalState.focus match
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
      finalState.bufferOrder should not be empty
      finalState.buffers.keys.toList.foreach(bufferId => finalState.bufferOrder should contain(bufferId))

    program.unsafeRunSync()
  }
