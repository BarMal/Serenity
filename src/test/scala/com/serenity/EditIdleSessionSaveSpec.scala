package com.serenity

import java.nio.file.Path

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.app.AppStartup
import com.serenity.keystroke.events.{Enter, InsertChar}
import com.serenity.session.SessionManager
import com.serenity.state.manager.StateManager
import com.serenity.state.models.AppState
import com.serenity.testkit.{AwaitCondition, SharedDictionary}
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Unsaved edits reach the session once typing pauses (#1904), so a crash before any file save still leaves text for
  * the next launch to restore. A second `StateManager` over the same session root stands in for that next launch.
  */
class EditIdleSessionSaveSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private val EditIdle       = 1500.millis
  private val editIdlePolicy = SessionManager.SessionPolicy(saveOnEditIdle = Some(EditIdle))

  private def startEditor(sessionRoot: Path, policy: SessionManager.SessionPolicy): IO[StateManager] =
    for
      editor <- StateManager(
        testLogger("EditIdleSessionSaveSpec"),
        policy,
        sessionRootOverride = Some(sessionRoot),
        dictionaryCache = SharedDictionary.default
      )
      _ <- AppStartup.initializeState(editor, editor.sessionStartupInfo, Theme.default, ViewportSize(80, 24))
      _ <- editor.applyEvent(Enter)
    yield editor

  private def bufferTexts(restored: Option[AppState]): List[String] =
    restored.toList.flatMap(_.persisted.buffers.values.map(_.document.content.toString))

  private val readerLogger = testLogger("EditIdleSessionSaveSpec-reader")

  /** What the session on disk would restore, read without starting an editor. */
  private def savedTexts(sessionRoot: Path): IO[List[String]] =
    val policy = SessionManager.SessionPolicy()
    SessionManager.create(sessionRoot, AppThemeManager.create, readerLogger, policy).loadSession().map(bufferTexts)

  private def restartedTexts(sessionRoot: Path): IO[List[String]] =
    StateManager(
      testLogger("EditIdleSessionSaveSpec-restart"),
      sessionRootOverride = Some(sessionRoot),
      dictionaryCache = SharedDictionary.default
    )
      .flatMap(_.sessionService.loadSession)
      .map(bufferTexts)

  private val tempSessionRoot: IO[Path] = IO.blocking(TestTemp.directory("edit-idle-session-save"))

  behavior of "Saving the session once edits pause"

  it should "save unsaved text into the session with no file save, so a restart restores it" in {
    val (stillUnsaved, restored) = (for
      sessionRoot  <- tempSessionRoot
      editor       <- startEditor(sessionRoot, editIdlePolicy)
      _            <- "draft".toList.traverse_(char => editor.applyEvent(InsertChar(char)))
      _            <- AwaitCondition.awaitValue(savedTexts(sessionRoot))(_.contains("draft"))
      stillUnsaved <- editor.getCurrentState.map(_.persisted.buffers.values.exists(_.hasUnsavedChanges))
      restored     <- restartedTexts(sessionRoot)
    yield (stillUnsaved, restored)).timeout(30.seconds).unsafeRunSync()

    stillUnsaved shouldBe true
    restored should contain("draft")
  }

  it should "not save while typing continues inside the idle window" in {
    val whileTyping = (for
      sessionRoot <- tempSessionRoot
      editor      <- startEditor(sessionRoot, editIdlePolicy)
      _           <- "steady".toList.traverse_(char => editor.applyEvent(InsertChar(char)) >> IO.sleep(EditIdle / 5))
      typingTexts <- savedTexts(sessionRoot)
      _           <- AwaitCondition.awaitValue(savedTexts(sessionRoot))(_.contains("steady"))
    yield typingTexts).timeout(30.seconds).unsafeRunSync()

    whileTyping.filter(text => text.nonEmpty && "steady".startsWith(text)) shouldBe empty
  }

  it should "leave unsaved edits out of the session when the policy sets no idle window" in {
    val saved = (for
      sessionRoot <- tempSessionRoot
      editor      <- startEditor(sessionRoot, SessionManager.SessionPolicy())
      _           <- "draft".toList.traverse_(char => editor.applyEvent(InsertChar(char)))
      _           <- IO.sleep(EditIdle * 2)
      texts       <- savedTexts(sessionRoot)
    yield texts).timeout(30.seconds).unsafeRunSync()

    saved should not contain "draft"
  }
