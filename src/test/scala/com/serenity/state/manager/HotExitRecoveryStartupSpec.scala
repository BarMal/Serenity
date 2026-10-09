package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.app.AppStartup
import com.serenity.command.{Command, CommandCategory, CommandIntent, SessionIntent}
import com.serenity.keystroke.events.{Direction, InsertChar, ModalNavigate, ModalSubmit}
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import com.serenity.{StateManagerTestSupport, TestTemp}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1904 end to end: unsaved edits saved into the session, then a restart that resumes it over the unchanged file. */
class HotExitRecoveryStartupSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private val resume =
    Command.typed(
      "resume-session",
      "Resume the last session.",
      CommandIntent.Session(SessionIntent.StartupRestoreSession),
      CommandCategory.File
    )

  /** Opens `file`, types `typed` at its start and saves the session without saving the file. */
  private def editWithoutSaving(sessionRoot: Path, file: Path, typed: String): IO[Unit] =
    for
      editor <- StateManager(
        testLogger("HotExitRecoveryStartupSpec"),
        sessionRootOverride = Some(sessionRoot),
        dictionaryCache = SharedDictionary.default
      )
      _ <- AppStartup.initializeState(editor, editor.sessionStartupInfo, Theme.default, ViewportSize(80, 24))
      _ <- editor.fileOpener.openFile(file)
      _ <- awaitState(editor)(s =>
        s.focusedBufferId.flatMap(s.persisted.buffers.get).exists(_.document.filePath.contains(file))
      )
      _ <- typed.toList.traverse_(char => editor.applyEvent(InsertChar(char)))
      _ <- editor.saveSession
    yield ()

  private def restarted(sessionRoot: Path): IO[StateManager] =
    for
      editor <- StateManager(
        testLogger("HotExitRecoveryStartupSpec-restart"),
        sessionRootOverride = Some(sessionRoot),
        dictionaryCache = SharedDictionary.default
      )
      _ <- AppStartup.initializeState(editor, editor.sessionStartupInfo, Theme.default, ViewportSize(80, 24))
      _ <- editor.executeCommand(resume)
    yield editor

  private def recoveryPrompts(state: AppState): List[ConfirmPrompt] =
    state.runtime.modalStack.map(_.modal).collect {
      case Modal.Confirm(prompt) if prompt.title == "Recover unsaved changes" => prompt
    }

  private def textOf(state: AppState, file: Path): Option[String] =
    state.persisted.buffers.values.find(_.document.filePath.contains(file)).map(_.document.content.collect())

  private def fixture: IO[(Path, Path)] =
    IO.blocking {
      val directory = TestTemp.directory("hot-exit-recovery")
      val file      = Files.writeString(directory.resolve("draft.txt"), "saved text")
      (directory.resolve("session"), file)
    }

  "Resuming a session at startup" should "offer recovery of unsaved edits newer than the file, and keep them" in {
    val (offered, kept, file) = (for
      (sessionRoot, file) <- fixture
      _                   <- editWithoutSaving(sessionRoot, file, "new ")
      editor              <- restarted(sessionRoot)
      offered             <- awaitState(editor)(state => recoveryPrompts(state).nonEmpty)
      _                   <- editor.applyEvent(ModalSubmit)
      kept                <- editor.getCurrentState
    yield (offered, kept, file)).timeout(30.seconds).unsafeRunSync()

    textOf(offered, file) shouldBe Some("new saved text")
    recoveryPrompts(offered).map(_.message) shouldBe List(
      List("draft.txt", "Unsaved changes from your last session are newer than the file on disk.")
    )
    recoveryPrompts(kept) shouldBe empty
    textOf(kept, file) shouldBe Some("new saved text")
  }

  it should "open the file from disk instead when that is chosen" in {
    val (reopened, file) = (for
      (sessionRoot, file) <- fixture
      _                   <- editWithoutSaving(sessionRoot, file, "new ")
      editor              <- restarted(sessionRoot)
      _                   <- awaitState(editor)(state => recoveryPrompts(state).nonEmpty)
      _                   <- editor.applyEvent(ModalNavigate(Direction.Down)) >> editor.applyEvent(ModalSubmit)
      reopened            <- awaitState(editor)(state => textOf(state, file).contains("saved text"))
    yield (reopened, file)).timeout(30.seconds).unsafeRunSync()

    reopened.persisted.buffers.values.find(_.document.filePath.contains(file)).map(_.document.isDirty) shouldBe
      Some(false)
  }

  it should "not ask when the session holds no unsaved edits" in {
    val resumed = (for
      (sessionRoot, file) <- fixture
      _                   <- editWithoutSaving(sessionRoot, file, "")
      editor              <- restarted(sessionRoot)
      resumed             <- awaitState(editor)(_.persisted.buffers.values.exists(_.document.filePath.isDefined))
    yield resumed).timeout(30.seconds).unsafeRunSync()

    recoveryPrompts(resumed) shouldBe empty
  }

end HotExitRecoveryStartupSpec
