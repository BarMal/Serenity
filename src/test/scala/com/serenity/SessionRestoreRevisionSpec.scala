package com.serenity

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import java.time.Instant

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.SettledClock
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.*
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class SessionRestoreRevisionSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def stateWithCleanFile(path: Path, text: String): AppState =
    val buffer  = Buffer.fromFile(BufferId(7), path, text)
    val initial = AppState.initial
    initial.copy(persisted = initial.persisted.copy(buffers = Map(buffer.id -> buffer), bufferOrder = List(buffer.id)))

  "A restored clean file-backed buffer" should "take its revision from a read made on the session manager's clock" in {
    val root = TestTemp.directory("session-restore-revision")
    val manager = SessionManager(
      root,
      AppThemeManager.create,
      LoggerFactory[IO].getLogger(using LoggerName("SessionRestoreRevisionSpec")),
      SessionManager.SessionPolicy(persistUnsavedBuffers = false),
      clock = SettledClock.aMinuteAhead
    )
    val path = TestTemp.file("session-restore-revision", ".txt")
    Files.writeString(path, "just written")
    // Ahead of the real clock but behind the manager's, so only a read on the manager's clock finds the stamp old enough.
    Files.setLastModifiedTime(path, FileTime.from(Instant.now.plusSeconds(30)))

    val restored = (for
      sessionId <- manager.saveSessionAs("Clean file", stateWithCleanFile(path, "just written"))
      loaded    <- manager.loadSession(sessionId)
    yield loaded.flatMap(_.persisted.buffers.values.headOption)).unsafeRunSync()

    restored.flatMap(_.document.revision) shouldBe SettledClock.fileManager.currentRevision(path).unsafeRunSync()
  }
