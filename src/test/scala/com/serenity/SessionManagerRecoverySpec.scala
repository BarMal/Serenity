package com.serenity

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.session.{PendingSessionWrite, SessionId, SessionIndex, SessionManager, SessionMetadata}
import com.serenity.state.models.*
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Crash-recovery and path-safety coverage for [[com.serenity.session.SessionManager]], split out of
  * `SessionManagerSpec` to keep both files under the architecture ratchet's file-length target.
  */
class SessionManagerRecoverySpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createManagerAt(
    tempDirectory: Path,
    policy: SessionManager.SessionPolicy = SessionManager.SessionPolicy()
  ): SessionManager =
    val themeManager = AppThemeManager.create
    val logger       = LoggerFactory[IO].getLogger(using LoggerName("SessionManagerRecoverySpec"))
    SessionManager.create(tempDirectory, themeManager, logger, policy)

  private def currentSessionFile(sessionRoot: Path): Path =
    sessionRoot.resolve("sessions").resolve("session.json")

  private def stateWithText(text: String): AppState =
    val initial  = AppState.initial
    val bufferId = initial.persisted.bufferOrder.head
    val buffer   = Buffer.fromString(bufferId, text)
    initial.copy(persisted = initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def writeIndex(sessionRoot: Path, index: SessionIndex): Unit =
    Files.writeString(
      sessionRoot.resolve("session-index.json"),
      _root_.io.circe.syntax.EncoderOps(index).asJson.spaces2
    )

  private def metadata(id: String, sessionFileName: String): SessionMetadata =
    SessionMetadata(
      id = SessionId(id),
      displayName = id,
      sessionFileName = sessionFileName,
      createdAtEpochMillis = 1L,
      updatedAtEpochMillis = 1L
    )

  private def pendingFile(sessionRoot: Path): Path =
    sessionRoot.resolve("session-write.pending.json")

  private def writePending(sessionRoot: Path, pending: PendingSessionWrite): Unit =
    Files.writeString(pendingFile(sessionRoot), _root_.io.circe.syntax.EncoderOps(pending).asJson.spaces2)

  private def quarantinedPendingFiles(sessionRoot: Path): List[Path] =
    val stream = Files.list(sessionRoot)
    try stream.filter(_.getFileName.toString.startsWith("session-write.pending.json.corrupt-")).iterator.asScala.toList
    finally stream.close()

  // saveSession/saveSessionAs write the session file before the index, and pruneHistory/deleteSession
  // delete session files before the index is rewritten -- so a crash between the two steps can only ever
  // leave an orphaned session file (unreferenced by the index) or an index entry pointing at a session
  // file that no longer exists, never the reverse. Both interim states are exercised directly below to
  // prove the existing load-time recovery paths (`sanitizeIndex`'s safe-path filter and `loadSessionFile`'s
  // exists-check) already tolerate a crash landing between the two writes, without needing the two writes
  // to be merged into one atomic operation.
  "SessionManager" should "tolerate an index entry left pointing at a session file removed by an interim crash" in {
    val sessionRoot    = Files.createTempDirectory("session-manager-index-ahead-of-file")
    val sessionManager = createManagerAt(sessionRoot)
    writeIndex(
      sessionRoot,
      SessionIndex(
        sessions = List(metadata("ghost", "ghost.json")),
        currentSessionId = Some(SessionId("ghost"))
      )
    )

    val program =
      for loaded <- sessionManager.loadSession()
      yield loaded shouldBe None

    program.unsafeRunSync()
  }

  it should "tolerate an orphaned session file left on disk by an interim crash before the index was written" in {
    val sessionRoot       = Files.createTempDirectory("session-manager-file-ahead-of-index")
    val sessionManager    = createManagerAt(sessionRoot)
    val sessionsDirectory = sessionRoot.resolve("sessions")

    val program = for
      _ <- sessionManager.saveSession(stateWithText("valid"))
      _ <- IO.blocking {
        Files.createDirectories(sessionsDirectory)
        Files.writeString(sessionsDirectory.resolve("orphan.json"), "{ not referenced by the index }")
      }
      loaded   <- sessionManager.loadSession()
      sessions <- sessionManager.listSessions()
    yield
      loaded.map(_.persisted.buffers.values.head.document.content.toString) shouldBe Some("valid")
      sessions.map(_.sessionFileName) should not contain "orphan.json"

    program.unsafeRunSync()
  }

  it should "replay a pending session write left behind by a crash between recording it and applying it" in {
    val sessionRoot       = Files.createTempDirectory("session-manager-pending-write")
    val sessionManager    = createManagerAt(sessionRoot)
    val sessionsDirectory = sessionRoot.resolve("sessions")

    val program = for
      // A real, well-formed session-file body to reuse as the pending write's content -- what matters
      // here is that recovery writes it out verbatim, not how a SessionState serializes.
      _              <- sessionManager.saveSession(stateWithText("first"))
      recoveredBytes <- IO.blocking(Files.readString(currentSessionFile(sessionRoot)))
      _ <- IO.blocking {
        Files.createDirectories(sessionsDirectory)
        writePending(
          sessionRoot,
          PendingSessionWrite(
            writes = Map("recovered.json" -> recoveredBytes),
            deletes = Nil,
            indexJson = _root_.io.circe.syntax
              .EncoderOps(
                SessionIndex(List(metadata("recovered", "recovered.json")), Some(SessionId("recovered")))
              )
              .asJson
              .spaces2
          )
        )
      }
      loaded   <- sessionManager.loadSession()
      sessions <- sessionManager.listSessions()
    yield
      loaded.map(_.persisted.buffers.values.head.document.content.toString) shouldBe Some("first")
      sessions.map(_.id.value) shouldBe List("recovered")
      Files.exists(pendingFile(sessionRoot)) shouldBe false
      Files.exists(sessionsDirectory.resolve("recovered.json")) shouldBe true

    program.unsafeRunSync()
  }

  it should "replay a pending session delete left behind by a crash between recording it and applying it" in {
    val sessionRoot    = Files.createTempDirectory("session-manager-pending-delete")
    val sessionManager = createManagerAt(sessionRoot)

    val program = for
      sessionId <- sessionManager.saveSessionAs("To delete", stateWithText("gone"))
      before    <- sessionManager.listSessions()
      sessionFileName = before.find(_.id == sessionId).value.sessionFileName
      _ <- IO.blocking {
        writePending(
          sessionRoot,
          PendingSessionWrite(
            writes = Map.empty,
            deletes = List(sessionFileName),
            indexJson = _root_.io.circe.syntax.EncoderOps(SessionIndex.empty).asJson.spaces2
          )
        )
      }
      sessions <- sessionManager.listSessions()
    yield
      sessions shouldBe Nil
      Files.exists(pendingFile(sessionRoot)) shouldBe false
      Files.exists(sessionRoot.resolve("sessions").resolve(sessionFileName)) shouldBe false

    program.unsafeRunSync()
  }

  it should "quarantine an unreadable pending session write and keep operating normally" in {
    val sessionRoot    = Files.createTempDirectory("session-manager-corrupt-pending")
    val sessionManager = createManagerAt(sessionRoot)
    Files.writeString(pendingFile(sessionRoot), "{ not valid pending json")

    val program = for
      sessions <- sessionManager.listSessions()
      _        <- sessionManager.saveSession(stateWithText("after recovery"))
      loaded   <- sessionManager.loadSession()
    yield
      sessions shouldBe Nil
      loaded.map(_.persisted.buffers.values.head.document.content.toString) shouldBe Some("after recovery")
      Files.exists(pendingFile(sessionRoot)) shouldBe false
      quarantinedPendingFiles(sessionRoot) should not be empty

    program.unsafeRunSync()
  }

  it should "never load an absolute legacy session path" in {
    val sessionRoot = Files.createTempDirectory("session-manager-absolute-path")
    val outsideFile = Files.createTempFile("session-manager-outside", ".json")
    val original    = "outside session content"
    Files.writeString(outsideFile, original)
    val sessionManager = createManagerAt(sessionRoot)
    writeIndex(
      sessionRoot,
      SessionIndex(List(metadata("unsafe", outsideFile.toAbsolutePath.toString)), Some(SessionId("unsafe")))
    )

    val loaded = sessionManager.loadSession(SessionId("unsafe")).unsafeRunSync()

    loaded shouldBe None
    Files.readString(outsideFile) shouldBe original
    sessionManager.listSessions().unsafeRunSync() shouldBe Nil
  }

  it should "never follow a session-file symlink outside the session root" in {
    val sessionRoot = Files.createTempDirectory("session-manager-symlink")
    val outsideFile = Files.createTempFile("session-manager-symlink-outside", ".json")
    Files.writeString(outsideFile, "outside")
    val sessionsDirectory = Files.createDirectories(sessionRoot.resolve("sessions"))
    Files.createSymbolicLink(sessionsDirectory.resolve("link.json"), outsideFile)
    val sessionManager = createManagerAt(sessionRoot)
    writeIndex(
      sessionRoot,
      SessionIndex(List(metadata("linked", "link.json")), Some(SessionId("linked")))
    )

    sessionManager.loadSession(SessionId("linked")).unsafeRunSync() shouldBe None
    Files.readString(outsideFile) shouldBe "outside"
  }

  it should "reject a symlinked sessions directory for every persistence operation" in {
    val sessionRoot = Files.createTempDirectory("session-manager-sessions-symlink")
    val outsideRoot = Files.createTempDirectory("session-manager-sessions-target")
    val outsideFile = outsideRoot.resolve("session.json")
    Files.writeString(outsideFile, "malformed outside session")
    Files.createSymbolicLink(sessionRoot.resolve("sessions"), outsideRoot)
    val sessionManager = createManagerAt(sessionRoot)
    writeIndex(
      sessionRoot,
      SessionIndex(List(metadata("current", "session.json")), Some(SessionId("current")))
    )

    sessionManager.loadSession().unsafeRunSync() shouldBe None
    sessionManager.saveSession(stateWithText("must stay inside")).attempt.unsafeRunSync().isLeft shouldBe true
    sessionManager.deleteSession(SessionId("current")).unsafeRunSync()
    sessionManager.sessionExists.unsafeRunSync() shouldBe false
    sessionManager.currentSessionThemeName.unsafeRunSync() shouldBe None
    Files.writeString(sessionRoot.resolve("session-index.json"), "not valid index json")
    sessionManager.listSessions().unsafeRunSync() shouldBe Nil

    Files.readString(outsideFile) shouldBe "malformed outside session"
    Files.list(outsideRoot).iterator().asScala.map(_.getFileName.toString).toList shouldBe List("session.json")
  }

  it should "reject traversal expressed with mixed path separators" in {
    val sessionRoot = Files.createTempDirectory("session-manager-mixed-path")
    val outsideFile = sessionRoot.getParent.resolve("mixed-session-outside.json")
    Files.writeString(outsideFile, "outside")
    val sessionManager = createManagerAt(sessionRoot)
    writeIndex(
      sessionRoot,
      SessionIndex(List(metadata("unsafe", "..\\" + outsideFile.getFileName.toString)), Some(SessionId("unsafe")))
    )

    sessionManager.deleteSession(SessionId("unsafe")).unsafeRunSync()

    Files.exists(outsideFile) shouldBe true
  }

  it should "never prune through an unsafe legacy session path" in {
    val sessionRoot = Files.createTempDirectory("session-manager-unsafe-prune")
    val outsideFile = Files.createTempFile("session-manager-prune-outside", ".json")
    Files.writeString(outsideFile, "outside")
    val sessionManager = createManagerAt(sessionRoot, SessionManager.SessionPolicy(maxSessionHistory = 0))
    writeIndex(
      sessionRoot,
      SessionIndex(List(metadata("unsafe", outsideFile.toAbsolutePath.toString)), None)
    )

    sessionManager.saveSessionAs("New", stateWithText("new")).unsafeRunSync()

    Files.readString(outsideFile) shouldBe "outside"
  }

  it should "use the canonical current filename when saving over unsafe legacy metadata" in {
    val sessionRoot = Files.createTempDirectory("session-manager-canonical-save")
    val outsideFile = Files.createTempFile("session-manager-canonical-outside", ".json")
    Files.writeString(outsideFile, "untouched")
    val sessionManager = createManagerAt(sessionRoot)
    writeIndex(
      sessionRoot,
      SessionIndex(List(metadata("current", outsideFile.toAbsolutePath.toString)), Some(SessionId("current")))
    )

    sessionManager.saveSession(stateWithText("safe")).unsafeRunSync()

    Files.readString(outsideFile) shouldBe "untouched"
    Files.exists(currentSessionFile(sessionRoot)) shouldBe true
    sessionManager
      .loadSession()
      .unsafeRunSync()
      .map(_.persisted.buffers.values.head.document.content.toString) shouldBe Some(
      "safe"
    )
  }

  it should "reject a hostile session id before canonicalizing its filename" in {
    val sessionRoot = Files.createTempDirectory("session-manager-hostile-id")
    val outsideFile = sessionRoot.getParent.resolve("hostile-session.json")
    Files.deleteIfExists(outsideFile)
    val sessionManager = createManagerAt(sessionRoot)
    writeIndex(
      sessionRoot,
      SessionIndex(List(metadata("../hostile", "safe.json")), Some(SessionId("../hostile")))
    )

    sessionManager.saveSession(stateWithText("must not escape")).attempt.unsafeRunSync().isLeft shouldBe true
    Files.exists(outsideFile) shouldBe false
  }

  it should "preserve session files when recovering a corrupt index" in {
    val sessionRoot    = Files.createTempDirectory("session-manager-corrupt-index")
    val sessionManager = createManagerAt(sessionRoot)
    val sessionId      = sessionManager.saveSessionAs("Recoverable", stateWithText("preserve me")).unsafeRunSync()
    Files.writeString(sessionRoot.resolve("session-index.json"), "not valid index json")

    sessionManager.saveSession(stateWithText("current after recovery")).unsafeRunSync()

    sessionManager
      .loadSession(sessionId)
      .unsafeRunSync()
      .map(_.persisted.buffers.values.head.document.content.toString) shouldBe
      Some("preserve me")
    Files
      .list(sessionRoot)
      .iterator()
      .asScala
      .map(_.getFileName.toString)
      .exists(_.startsWith("session-index.json.corrupt-")) shouldBe true
  }
