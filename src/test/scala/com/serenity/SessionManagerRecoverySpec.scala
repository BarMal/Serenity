package com.serenity

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.SettledClock
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
    SessionManager(tempDirectory, themeManager, logger, policy, clock = SettledClock.aMinuteAhead)

  private def currentSessionFile(sessionRoot: Path): Path =
    sessionRoot.resolve("sessions").resolve("session.json")

  private def stateWithText(text: String): AppState =
    val initial  = AppState.initial
    val bufferId = initial.persisted.bufferOrder.head
    val buffer   = Buffer.fromString(bufferId, text)
    initial.copy(persisted = initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def inlineBufferText(sessionJson: String, text: String): String =
    _root_.io.circe.parser
      .parse(sessionJson)
      .map(
        _.hcursor
          .downField("buffers")
          .withFocus(
            _.mapArray(
              _.map(
                _.mapObject(_.remove("contentRef").add("unsavedContent", _root_.io.circe.Json.fromString(text)))
              )
            )
          )
          .top
          .getOrElse(_root_.io.circe.Json.Null)
          .noSpaces
      )
      .getOrElse(sessionJson)

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
      // here is that recovery writes it out verbatim, not how a SessionState serializes. It carries its text inline,
      // as a session written before #1912 does, because a content file belongs to the session file it was saved for.
      _ <- sessionManager.saveSession(stateWithText("first"))
      recoveredBytes <- IO.blocking(
        inlineBufferText(Files.readString(currentSessionFile(sessionRoot)), "first")
      )
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

  // A non-empty directory where the session file belongs makes the final replace fail, leaving the commit exactly as a
  // crash between recording the marker and applying it would.
  private def blockSessionFile(sessionRoot: Path): Unit =
    Files.createDirectories(currentSessionFile(sessionRoot).resolve("blocker")): Unit

  private def unblockSessionFile(sessionRoot: Path): Unit =
    Files.delete(currentSessionFile(sessionRoot).resolve("blocker"))
    Files.delete(currentSessionFile(sessionRoot))

  it should "record a pending marker that names the staged session file rather than embedding its text (#1912)" in {
    val sessionRoot    = Files.createTempDirectory("session-manager-staged-marker")
    val sessionManager = createManagerAt(sessionRoot)
    blockSessionFile(sessionRoot)

    val interrupted = sessionManager.saveSession(stateWithText("text only the session file holds")).attempt
    interrupted.unsafeRunSync().isLeft shouldBe true

    Files.readString(pendingFile(sessionRoot)) should not include "text only the session file holds"
  }

  it should "finish a staged commit that was interrupted before the session file was replaced (#1912)" in {
    val sessionRoot = Files.createTempDirectory("session-manager-staged-replay")
    blockSessionFile(sessionRoot)
    createManagerAt(sessionRoot).saveSession(stateWithText("staged before the crash")).attempt.unsafeRunSync()
    unblockSessionFile(sessionRoot)

    val loaded = createManagerAt(sessionRoot).loadSession().unsafeRunSync()

    loaded.map(_.persisted.buffers.values.head.document.content.toString) shouldBe Some("staged before the crash")
    Files.exists(pendingFile(sessionRoot)) shouldBe false
  }

  it should "write session files as compact JSON (#1912)" in {
    val sessionRoot    = Files.createTempDirectory("session-manager-compact")
    val sessionManager = createManagerAt(sessionRoot)

    sessionManager.saveSession(stateWithText("compact")).unsafeRunSync()

    Files.readString(currentSessionFile(sessionRoot)) should not include "\n"
    Files.readString(sessionRoot.resolve("session-index.json")) should not include "\n"
  }

  it should "keep the index in memory instead of re-reading it on every save (#1912)" in {
    val sessionRoot    = Files.createTempDirectory("session-manager-index-cache")
    val sessionManager = createManagerAt(sessionRoot)
    val indexFile      = sessionRoot.resolve("session-index.json")
    sessionManager.saveSessionAs("Named", stateWithText("named")).unsafeRunSync()
    sessionManager.saveSession(stateWithText("current")).unsafeRunSync()

    // Same length, same inode, modification time put back: only reading the index could notice.
    val modified = Files.getLastModifiedTime(indexFile)
    Files.writeString(indexFile, "x" * Files.size(indexFile).toInt)
    Files.setLastModifiedTime(indexFile, modified)
    sessionManager.saveSession(stateWithText("current again")).unsafeRunSync()

    sessionManager.listSessions().unsafeRunSync().map(_.displayName) should contain("Named")
    Files.list(sessionRoot).iterator().asScala.exists(_.getFileName.toString.contains(".corrupt-")) shouldBe false
  }

  private def stateWithTexts(first: String, second: String): AppState =
    val initial = stateWithText(first)
    val firstId = initial.persisted.bufferOrder.head
    val otherId = BufferId(firstId.value + 1)
    initial.copy(persisted =
      initial.persisted.copy(
        buffers = initial.persisted.buffers + (otherId -> Buffer.fromString(otherId, second)),
        bufferOrder = initial.persisted.bufferOrder :+ otherId
      )
    )

  private def contentFiles(sessionRoot: Path): Map[String, java.nio.file.attribute.FileTime] =
    val directory = sessionRoot.resolve("sessions").resolve("session.content")
    if !Files.isDirectory(directory) then Map.empty
    else
      val listing = Files.list(directory)
      try
        listing.iterator().asScala.map(path => path.getFileName.toString -> Files.getLastModifiedTime(path)).toMap
      finally listing.close()

  it should "keep buffer text out of the session file and restore it from per-buffer content files (#1912)" in {
    val sessionRoot    = Files.createTempDirectory("session-manager-content-files")
    val sessionManager = createManagerAt(sessionRoot)

    sessionManager.saveSession(stateWithTexts("first buffer text", "second buffer text")).unsafeRunSync()

    Files.readString(currentSessionFile(sessionRoot)) should not include "buffer text"
    contentFiles(sessionRoot).size shouldBe 2
    val loaded = createManagerAt(sessionRoot).loadSession().unsafeRunSync().value
    loaded.persisted.buffers.values.map(_.document.content.toString).toSet shouldBe
      Set("first buffer text", "second buffer text")
  }

  it should "rewrite only the content file of the buffer that changed (#1912)" in {
    val sessionRoot    = Files.createTempDirectory("session-manager-one-changed")
    val sessionManager = createManagerAt(sessionRoot)
    sessionManager.saveSession(stateWithTexts("unchanged text", "text before the edit")).unsafeRunSync()
    val before = contentFiles(sessionRoot)
    IO.sleep(50.millis).unsafeRunSync()

    sessionManager.saveSession(stateWithTexts("unchanged text", "text after the edit")).unsafeRunSync()

    val after   = contentFiles(sessionRoot)
    val kept    = before.keySet.intersect(after.keySet)
    val written = after.keySet -- before.keySet
    kept.size shouldBe 1
    kept.foreach(name => after(name) shouldBe before(name))
    written.size shouldBe 1
    (before.keySet -- after.keySet).size shouldBe 1
  }

  it should "remove a deleted session's content files (#1912)" in {
    val sessionRoot    = Files.createTempDirectory("session-manager-content-delete")
    val sessionManager = createManagerAt(sessionRoot)
    sessionManager.saveSession(stateWithTexts("one", "two")).unsafeRunSync()

    sessionManager.clearSession().unsafeRunSync()

    Files.exists(sessionRoot.resolve("sessions").resolve("session.content")) shouldBe false
  }

  it should "restore a buffer without its text when the content file is missing instead of failing the session (#1912)" in {
    val sessionRoot    = Files.createTempDirectory("session-manager-content-missing")
    val sessionManager = createManagerAt(sessionRoot)
    sessionManager.saveSession(stateWithTexts("one", "two")).unsafeRunSync()
    contentFiles(sessionRoot).keys.foreach(name =>
      Files.delete(sessionRoot.resolve("sessions").resolve("session.content").resolve(name))
    )

    val loaded = createManagerAt(sessionRoot).loadSession().unsafeRunSync()

    loaded.map(_.persisted.buffers.size) shouldBe Some(2)
  }
