package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.config.{AppConfig, AppMode}
import com.serenity.io.FileManager
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.session.{SessionManager, SessionPersistence, SessionSaveTrigger}
import com.serenity.state.models.*
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Exercises [[StateManagerFilePersistence]] on its own (#1442): a real `FileManager` writing to a temp directory, a
  * [[SessionPersistence]] double recording save triggers instead of touching disk, and a real [[LspEffectQueue]] so
  * `refreshLspBindingAfterSaveAs`'s open/close bookkeeping can be observed directly.
  */
class StateManagerFilePersistenceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private class RecordingSessionPersistence(triggers: Ref[IO, List[SessionSaveTrigger]], root: Path)
      extends SessionPersistence(
        SessionManager.create(root, AppThemeManager.create, NoOpLogger.impl[IO], SessionManager.SessionPolicy()),
        SessionManager.SessionPolicy()
      ):
    override def maybeSaveSession(appState: AppState, trigger: SessionSaveTrigger): IO[Unit] =
      triggers.update(_ :+ trigger)

  final private class Harness(
      val stateRef: Ref[IO, AppState],
      val sessionTriggers: Ref[IO, List[SessionSaveTrigger]],
      val lspQueue: LspEffectQueue,
      val persistence: StateManagerFilePersistence
  ):
    def currentState: AppState = stateRef.get.unsafeRunSync()

    /** Drains at most one queued LSP effect, or `None` if nothing arrives within the window -- used both to assert an
      * expected enqueue and to assert the queue stays empty.
      */
    def nextLspEffect(within: FiniteDuration = 500.millis): Option[LspEffect] =
      lspQueue.stream.take(1).compile.last.timeout(within).handleError(_ => None).unsafeRunSync()

  private def harness(initialState: AppState): Harness =
    val stateRefVar   = Ref.of[IO, AppState](initialState).unsafeRunSync()
    val sessionRoot   = Files.createTempDirectory("file-persistence-spec")
    val triggersVar   = Ref.of[IO, List[SessionSaveTrigger]](Nil).unsafeRunSync()
    val lspQueueVar   = LspEffectQueue.create.unsafeRunSync()
    val sessionPersistence = new RecordingSessionPersistence(triggersVar, sessionRoot)

    new Harness(
      stateRefVar,
      triggersVar,
      lspQueueVar,
      new StateManagerFilePersistence(stateRefVar, new FileManager(), sessionPersistence, NoOpLogger.impl[IO], lspQueueVar)
    )

  private def stateWithBuffer(buffer: Buffer): AppState =
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(buffer.id -> buffer)))

  "saveExistingBuffer" should "write the buffer's content to its existing file path and update state with the saved buffer" in {
    val path   = Files.createTempFile("existing", ".txt")
    Files.writeString(path, "old content")
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "new content")
      .copy(document = Buffer.fromString(bufferId, "new content").document.copy(filePath = Some(path), isDirty = true))
    val h = harness(stateWithBuffer(buffer))

    h.persistence.saveExistingBuffer(bufferId).unsafeRunSync()

    Files.readString(path) shouldBe "new content"
    h.currentState.persisted.buffers(bufferId).document.isDirty shouldBe false
  }

  it should "trigger a file-change session save after a successful save" in {
    val path     = Files.createTempFile("existing", ".txt")
    val bufferId = BufferId(1)
    val buffer =
      Buffer.fromString(bufferId, "x").copy(document = Buffer.fromString(bufferId, "x").document.copy(filePath = Some(path)))
    val h = harness(stateWithBuffer(buffer))

    h.persistence.saveExistingBuffer(bufferId).unsafeRunSync()

    h.sessionTriggers.get.unsafeRunSync() shouldBe List(SessionSaveTrigger.FileChange)
  }

  it should "no-op for a buffer that has never been saved to a path" in {
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "x")
    val before   = stateWithBuffer(buffer)
    val h        = harness(before)

    h.persistence.saveExistingBuffer(bufferId).unsafeRunSync()

    h.currentState shouldBe before
    h.sessionTriggers.get.unsafeRunSync() shouldBe Nil
  }

  it should "no-op for a buffer id that isn't tracked" in {
    val before = AppState.initial
    val h      = harness(before)

    h.persistence.saveExistingBuffer(BufferId(999)).unsafeRunSync()

    h.currentState shouldBe before
  }

  "saveBufferAs" should "write the buffer to the new path, update state, and remember it as a recent file" in {
    val newPath  = Files.createTempFile("save-as", ".txt")
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "hello")
    val h        = harness(stateWithBuffer(buffer))

    h.persistence.saveBufferAs(bufferId, newPath).unsafeRunSync()

    Files.readString(newPath) shouldBe "hello"
    h.currentState.persisted.buffers(bufferId).document.filePath shouldBe Some(newPath)
    h.currentState.persisted.recentFiles shouldBe List(newPath)
    h.currentState.persisted.recentFilesByMode.getOrElse(AppMode.Code, Nil) shouldBe List(newPath)
  }

  it should "move a path to the front of recentFiles rather than duplicate it" in {
    val pathA    = Files.createTempFile("recent-a", ".txt")
    val pathB    = Files.createTempFile("recent-b", ".txt")
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "x")
    val h        = harness(stateWithBuffer(buffer))

    h.persistence.saveBufferAs(bufferId, pathA).unsafeRunSync()
    h.persistence.saveBufferAs(bufferId, pathB).unsafeRunSync()
    h.persistence.saveBufferAs(bufferId, pathA).unsafeRunSync()

    h.currentState.persisted.recentFiles shouldBe List(pathA, pathB)
  }

  it should "trigger a file-change session save after a successful save-as" in {
    val newPath  = Files.createTempFile("save-as", ".txt")
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "x")
    val h        = harness(stateWithBuffer(buffer))

    h.persistence.saveBufferAs(bufferId, newPath).unsafeRunSync()

    h.sessionTriggers.get.unsafeRunSync() shouldBe List(SessionSaveTrigger.FileChange)
  }

  it should "no-op for a buffer id that isn't tracked" in {
    val newPath = Files.createTempFile("save-as", ".txt")
    val before  = AppState.initial
    val h       = harness(before)

    h.persistence.saveBufferAs(BufferId(999), newPath).unsafeRunSync()

    h.currentState shouldBe before
  }

  "saveBufferAs in Code mode" should "enqueue an LSP file-opened effect for the new path when the buffer has a recognised language" in {
    val newPath  = Files.createTempFile("save-as", ".rs")
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "fn main() {}")
      .copy(document =
        Buffer.fromString(bufferId, "fn main() {}").document.copy(language = Some(LanguageId.Rust))
      )
    val state = stateWithBuffer(buffer).copy(persisted =
      stateWithBuffer(buffer).persisted.copy(config = AppConfig.default.withAppMode(AppMode.Code))
    )
    val h = harness(state)

    h.persistence.saveBufferAs(bufferId, newPath).unsafeRunSync()

    h.nextLspEffect() match
      case Some(LspEffect.FileOpened(uri, LanguageId.Rust, _)) => uri should include(newPath.getFileName.toString)
      case other                                               => fail(s"expected a FileOpened effect, got $other")
  }

  it should "enqueue nothing when the app is not in Code mode" in {
    val newPath  = Files.createTempFile("save-as", ".rs")
    val bufferId = BufferId(1)
    val buffer = Buffer
      .fromString(bufferId, "fn main() {}")
      .copy(document =
        Buffer.fromString(bufferId, "fn main() {}").document.copy(language = Some(LanguageId.Rust))
      )
    val state = stateWithBuffer(buffer).copy(persisted =
      stateWithBuffer(buffer).persisted.copy(config = AppConfig.default.withAppMode(AppMode.Prose))
    )
    val h = harness(state)

    h.persistence.saveBufferAs(bufferId, newPath).unsafeRunSync()

    h.nextLspEffect() shouldBe None
  }
