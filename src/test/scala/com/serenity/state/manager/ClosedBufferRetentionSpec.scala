package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.config.PreferredWindowSize
import com.serenity.keystroke.events.{CloseTab, Enter, Event, InsertChar, LspEvent}
import com.serenity.lsp.client.DocumentUri
import com.serenity.lsp.model.{Diagnostic, LspPosition, LspRange, SemanticToken}
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.*
import com.serenity.state.undo.{HistoryEntry, UndoState}
import com.serenity.testkit.AwaitCondition
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** Closing a buffer lets go of everything the session kept for it: undo history, the chapter-ghost cache entry, and the
  * language server's diagnostics and semantic tokens. A reopened file is a new buffer under a fresh id, so none of that
  * could be reached again.
  */
class ClosedBufferRetentionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private case class Fixture(stateManager: StateManager, directory: Path):
    def state: AppState = stateManager.getCurrentState.unsafeRunSync()

    def model: Model = stateManager.getModel.unsafeRunSync()

    def open(name: String, content: String): BufferId =
      val path = Files.writeString(directory.resolve(name), content)
      stateManager.fileOpener.openFile(path).unsafeRunSync()
      idOf(path)

    def idOf(path: Path): BufferId =
      state.persisted.buffers.values.find(_.document.filePath.contains(path)).map(_.id).getOrElse(fail("not opened"))

    def send(event: Event): Unit =
      stateManager.applyEvent(event).timeout(20.seconds).unsafeRunSync()

    def typeText(text: String): Unit = text.foreach(char => send(InsertChar(char)))

    def uriOf(id: BufferId): DocumentUri =
      state.persisted.buffers.get(id).map(state.runtime.bufferIndexMemos.uriFor).getOrElse(fail("no such buffer"))

    def closeActiveBufferSaving(id: BufferId): Unit =
      send(CloseTab)
      send(Enter)
      AwaitCondition.awaitValue(IO(state.persisted.buffers.contains(id)))(_ == false).unsafeRunSync()
      ()

    def editsFor(id: BufferId): List[HistoryEntry.BufferEdit] =
      val undo = model.undo
      (undo.undoStack ++ undo.redoStack ++ undo.pendingGroup).collect {
        case edit: HistoryEntry.BufferEdit if edit.bufferId == id => edit
      }.toList

    def publishDiagnostics(id: BufferId): Unit =
      val range = LspRange(LspPosition(0, 0), LspPosition(0, 1))
      send(LspEvent.LspDiagnosticsReceived(uriOf(id).value, List(Diagnostic(range, None, "message"))))

    def publishSemanticTokens(id: BufferId): Unit =
      send(LspEvent.LspSemanticTokensReceived(uriOf(id).value, List(SemanticToken(0, 0, 1, "variable", Set.empty))))

    def seedChapterGhosts(id: BufferId): Unit =
      val buffer = state.persisted.buffers.getOrElse(id, fail("no such buffer"))
      val noted  = buffer.copy(annotations = Annotations(notes = Map(NoteKey.Keyword("note") -> Notes(BufferId(900)))))
      val _      = stateManager.renderCaches.chapterGhosts.ghostsFor(noted, state.persisted.buffers)

  private def fixture(): Fixture =
    val directory = Files.createTempDirectory("closed-buffer-retention-spec")
    val program =
      for
        modelRef            <- Ref.of[IO, Model](Model(AppState.initial, UndoState()))
        themeNamesRef       <- Ref.of[IO, List[String]](Nil)
        quitSignal          <- Deferred[IO, Unit]
        lspQueue            <- LspEffectQueue.create
        mouseTargetCacheRef <- Ref.of[IO, Option[MouseTargetCache]](None)
        runtime = StateManagerRuntime.create(
          modelRef = modelRef,
          themeNamesRef = themeNamesRef,
          quitSignal = quitSignal,
          logger = NoOpLogger.impl[IO],
          policy = SessionManager.SessionPolicy(),
          sessionRootOverride = Some(directory.resolve("session")),
          themeManager = AppThemeManager.create,
          lspQueue = lspQueue,
          mouseTargetCacheRef = mouseTargetCacheRef,
          onFontConfigChanged = (_: FontConfig) => IO.unit,
          deviceTextScaleProvider = IO.pure(1.0),
          configPersistencePath = None,
          uiPresetStore = UiPresetStore(directory.resolve("presets.json")),
          windowSizeProvider = IO.pure(None),
          onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
          fileDialog = None
        )
        stateManager <- StateManager.fromRuntime(runtime)
      yield Fixture(stateManager, directory)
    program.unsafeRunSync()

  /** `b` is edited first, then `a`: closing `a` leaves `b`'s history to be told apart from it. */
  private def sessionWithEditedBuffers(): (Fixture, BufferId, BufferId) =
    val f = fixture()
    val b = f.open("b.txt", "beta")
    f.typeText("bbb")
    val a = f.open("a.txt", "alpha")
    f.typeText("a" * 10)
    (f, a, b)

  "Closing a buffer" should "leave no undo or redo entry that references it" in {
    val (f, a, b) = sessionWithEditedBuffers()
    f.editsFor(a) should not be empty
    val bEdits = f.editsFor(b)
    bEdits should not be empty

    f.closeActiveBufferSaving(a)

    f.editsFor(a) shouldBe empty
    f.editsFor(b) shouldBe bEdits
  }

  it should "leave no pending undo group for it" in {
    val (f, a, _) = sessionWithEditedBuffers()
    f.model.undo.pendingGroup.map(_.bufferId) shouldBe Some(a)

    f.closeActiveBufferSaving(a)

    f.model.undo.pendingGroup shouldBe empty
  }

  it should "drop its chapter-ghost cache entry and keep the other buffers'" in {
    val (f, a, b) = sessionWithEditedBuffers()
    f.seedChapterGhosts(a)
    f.seedChapterGhosts(b)
    f.stateManager.renderCaches.chapterGhosts.entryCount shouldBe 2

    f.closeActiveBufferSaving(a)

    f.stateManager.renderCaches.chapterGhosts.entryCount shouldBe 1
  }

  it should "drop the diagnostics and semantic tokens held for its document and keep the other buffers'" in {
    val (f, a, b) = sessionWithEditedBuffers()
    val aUri      = f.uriOf(a)
    val bUri      = f.uriOf(b)
    f.publishDiagnostics(a)
    f.publishDiagnostics(b)
    f.publishSemanticTokens(a)
    f.publishSemanticTokens(b)
    f.state.runtime.languageService.diagnosticsState.diagnostics.keySet should contain(aUri)
    f.state.runtime.languageService.semanticTokensState.byUri.keySet should contain(aUri)

    f.closeActiveBufferSaving(a)

    val languageService = f.state.runtime.languageService
    languageService.diagnosticsState.diagnostics.keySet shouldBe Set(bUri)
    languageService.semanticTokensState.byUri.keySet shouldBe Set(bUri)
  }

  "Reopening a closed file" should "open it as a new buffer with no history carried over" in {
    val (f, a, b) = sessionWithEditedBuffers()
    val bEdits    = f.editsFor(b)
    f.closeActiveBufferSaving(a)

    val reopened = f.open("a.txt", "alpha")

    reopened should not be a
    f.editsFor(reopened) shouldBe empty
    f.editsFor(b) shouldBe bEdits
  }
