package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.TestTemp
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

/** Opening a file past the large-file thresholds warns, and past the tooling limit keeps it from the language server
  * (#1867).
  */
class StateManagerFilePersistenceLargeFileSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private class NoSessionSaves
      extends SessionPersistence(
        SessionManager.create(
          TestTemp.directory("large-file-session"),
          AppThemeManager.create,
          NoOpLogger.impl[IO],
          SessionManager.SessionPolicy()
        ),
        SessionManager.SessionPolicy()
      ):
    override def maybeSaveSession(appState: AppState, trigger: SessionSaveTrigger): IO[Unit] = IO.unit

  final private class Harness(
      val lspQueue: LspEffectQueue,
      val notices: Ref[IO, List[Notice]],
      val persistence: StateManagerFilePersistence
  ):
    def nextLspEffect(within: FiniteDuration = 500.millis): Option[LspEffect] =
      lspQueue.stream.take(1).compile.last.timeout(within).handleError(_ => None).unsafeRunSync()

  private val policy = LargeFilePolicy(warnAboveChars = 10, toolingOffAboveChars = 20)

  private def harness: Harness =
    val initial    = AppState.initial.copy(persisted = AppState.initial.persisted.copy(config = codeConfig))
    val modelRef   = ModelViews.modelOf(initial).unsafeRunSync()
    val stateRef   = ModelViews.appRef(modelRef)
    val operations = StateManagerOperationBoundary.create(modelRef, NoOpLogger.impl[IO]).unsafeRunSync()
    val queue      = LspEffectQueue.create.unsafeRunSync()
    val notices    = Ref.of[IO, List[Notice]](Nil).unsafeRunSync()
    new Harness(
      queue,
      notices,
      new StateManagerFilePersistence(
        stateRef.get,
        operations.modelCommit.commitState,
        new FileManager(),
        new NoSessionSaves,
        NoOpLogger.impl[IO],
        queue,
        operations.fileLanes,
        notice => notices.update(_ :+ notice)
      )
    )

  private def codeConfig: AppConfig =
    AppConfig.default.withAppMode(AppMode.Code).withLargeFilePolicy(policy)

  private def fileOf(chars: Int): Path =
    val path = TestTemp.file("large-file", ".rs")
    Files.writeString(path, "x" * chars)
    path

  "Opening a file" should "announce one under the limit to the language server without a notice" in {
    val h = harness

    h.persistence.openFile(fileOf(5)).unsafeRunSync()

    h.nextLspEffect().collect { case LspEffect.FileOpened(_, language, _) => language } shouldBe Some(LanguageId.Rust)
    h.notices.get.unsafeRunSync() shouldBe Nil
  }

  it should "warn about a large file and still announce it" in {
    val h = harness

    h.persistence.openFile(fileOf(15)).unsafeRunSync()

    h.nextLspEffect().collect { case LspEffect.FileOpened(_, language, _) => language } shouldBe Some(LanguageId.Rust)
    h.notices.get.unsafeRunSync().map(_.level) shouldBe List(NoticeLevel.Warning)
  }

  it should "warn about a huge file and keep it from the language server" in {
    val h = harness

    h.persistence.openFile(fileOf(25)).unsafeRunSync()

    h.nextLspEffect() shouldBe None
    h.notices.get.unsafeRunSync().map(_.message).exists(_.contains("language server features")) shouldBe true
  }
