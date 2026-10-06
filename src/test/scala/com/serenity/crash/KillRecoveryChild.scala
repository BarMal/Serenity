package com.serenity.crash

import java.nio.file.Paths

import scala.concurrent.duration.DurationInt

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.FileManager
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.{AppState, Buffer}
import com.serenity.ui.theme.config.AppThemeManager
import org.typelevel.log4cats.noop.NoOpLogger

/** Run by [[KillRecoverySpec]] in a separate JVM that the spec then kills. Each round saves the target file, then the
  * session holding a dirty buffer of that file with a newer unsaved text, through the production [[FileManager]] and
  * [[SessionManager]], and reports each completed save on stdout so the spec can kill it mid-way through the next.
  */
object KillRecoveryChild:

  private given Balance = Balance.default

  // Only reached when the spec never kills the child, so that it cannot outlive the test run.
  private val MaxLifetime = 120.seconds

  def main(args: Array[String]): Unit =
    val Array(sessionRoot, target) = args
    val targetPath                 = Paths.get(target)
    val sessions = SessionManager.create(
      Paths.get(sessionRoot),
      AppThemeManager.create,
      NoOpLogger[IO],
      SessionManager.SessionPolicy()
    )
    val files = new FileManager()

    def report(line: String): IO[Unit] = IO.blocking {
      System.out.println(line)
      System.out.flush()
    }

    def round(version: Int): IO[Unit] =
      val bufferId = AppState.initial.persisted.bufferOrder.head
      val saved = Buffer.fromFile(bufferId, targetPath, KillRecoveryPayload.text(KillRecoveryPayload.FileKind, version))
      val unsaved =
        Buffer.fromFile(bufferId, targetPath, KillRecoveryPayload.text(KillRecoveryPayload.HotKind, version))
      val dirty = unsaved.copy(document = unsaved.document.copy(isDirty = true))
      val state =
        AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> dirty)))
      files.saveBuffer(saved, targetPath) >> report(KillRecoveryPayload.fileAck(version)) >>
        sessions.saveSession(state) >> report(KillRecoveryPayload.sessionAck(version))

    def rounds(version: Int): IO[Unit] = round(version) >> rounds(version + 1)

    (report("started") >> rounds(1)).timeout(MaxLifetime).unsafeRunSync()
