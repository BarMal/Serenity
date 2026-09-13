package com.serenity

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, SurfaceConfig}
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class SessionManagerSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createManager(policy: SessionManager.SessionPolicy = SessionManager.SessionPolicy()): SessionManager =
    val tempDirectory = Files.createTempDirectory("session-manager-spec")
    createManagerAt(tempDirectory, policy)

  private def createManagerAt(
    tempDirectory: Path,
    policy: SessionManager.SessionPolicy = SessionManager.SessionPolicy()
  ): SessionManager =
    val themeManager = AppThemeManager.create
    val logger       = LoggerFactory[IO].getLogger(using LoggerName("SessionManagerSpec"))
    SessionManager.create(tempDirectory, themeManager, logger, policy)

  private def currentSessionFile(sessionRoot: Path): Path =
    sessionRoot.resolve("sessions").resolve("session.json")

  private def quarantinedSessionFiles(sessionRoot: Path): List[Path] =
    val sessionsDirectory = sessionRoot.resolve("sessions")
    if Files.exists(sessionsDirectory) then
      val stream = Files.list(sessionsDirectory)
      try stream.filter(_.getFileName.toString.startsWith("session.json.corrupt-")).iterator.asScala.toList
      finally stream.close()
    else Nil

  private def dirtyStateWithText(text: String): AppState =
    val initial     = AppState.initial
    val bufferId    = initial.persisted.bufferOrder.head
    val plainBuffer = Buffer.fromString(bufferId, text)
    val buffer      = plainBuffer.copy(document = plainBuffer.document.copy(isDirty = true))
    initial.copy(persisted = initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def stateWithText(text: String): AppState =
    val initial  = AppState.initial
    val bufferId = initial.persisted.bufferOrder.head
    val buffer   = Buffer.fromString(bufferId, text)
    initial.copy(persisted = initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def cleanFileStateWithText(diskText: String): IO[AppState] =
    IO.blocking {
      val tempFile = Files.createTempFile("session-manager-file-backed-clean", ".txt")
      Files.writeString(tempFile, diskText)
      val buffer  = Buffer.fromFile(BufferId(7), tempFile, diskText)
      val initial = AppState.initial
      initial.copy(
        persisted = initial.persisted.copy(
          buffers = Map(buffer.id -> buffer),
          bufferOrder = List(buffer.id),
          layout = Layout(
            editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
            activeEditorPaneId = Some(PaneId(0)),
            workspaceTree = Some(TestWorkspaceTrees.linear(PaneId(0)))
          ),
          focus = Focus.EditorPane(PaneId(0))
        ),
        runtime = initial.runtime.copy(
          nextBufferId = BufferId(8),
          nextPaneId = PaneId(1)
        )
      )
    }

  private def dirtyFileStateWithText(diskText: String, unsavedText: String): IO[AppState] =
    IO.blocking {
      val tempFile = Files.createTempFile("session-manager-file-backed", ".txt")
      Files.writeString(tempFile, diskText)
      val plainBuffer = Buffer.fromFile(BufferId(7), tempFile, unsavedText)
      val buffer      = plainBuffer.copy(document = plainBuffer.document.copy(isDirty = true))
      val initial     = AppState.initial
      initial.copy(
        persisted = initial.persisted.copy(
          buffers = Map(buffer.id -> buffer),
          bufferOrder = List(buffer.id),
          layout = Layout(
            editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
            activeEditorPaneId = Some(PaneId(0)),
            workspaceTree = Some(TestWorkspaceTrees.linear(PaneId(0)))
          ),
          focus = Focus.EditorPane(PaneId(0))
        ),
        runtime = initial.runtime.copy(
          nextBufferId = BufferId(8),
          nextPaneId = PaneId(1)
        )
      )
    }

  "SessionManager" should "save and load named sessions through the index" in {
    val sessionManager = createManager()

    val program = for
      firstSessionId  <- sessionManager.saveSessionAs("Daily notes", stateWithText("alpha"))
      secondSessionId <- sessionManager.saveSessionAs("Refactor branch", stateWithText("beta"))
      sessions        <- sessionManager.listSessions()
      loadedFirst     <- sessionManager.loadSession(firstSessionId)
      loadedSecond    <- sessionManager.loadSession(secondSessionId)
    yield
      sessions.map(_.displayName).shouldBe(List("Daily notes", "Refactor branch"))
      loadedFirst.map(_.persisted.buffers.values.head.document.content.toString).shouldBe(Some("alpha"))
      loadedSecond.map(_.persisted.buffers.values.head.document.content.toString).shouldBe(Some("beta"))

    program.unsafeRunSync()
  }

  it should "rename and delete named sessions" in {
    val sessionManager = createManager()

    val program = for
      sessionId           <- sessionManager.saveSessionAs("Scratchpad", stateWithText("notes"))
      _                   <- sessionManager.renameSession(sessionId, "Project notes")
      renamedSessions     <- sessionManager.listSessions()
      _                   <- sessionManager.deleteSession(sessionId)
      sessionsAfterDelete <- sessionManager.listSessions()
      existsAfterDelete   <- sessionManager.sessionExists
    yield
      renamedSessions.map(_.displayName).shouldBe(List("Project notes"))
      sessionsAfterDelete.shouldBe(Nil)
      existsAfterDelete.shouldBe(false)

    program.unsafeRunSync()
  }

  it should "clear the current saved session" in {
    val sessionManager = createManager()

    val program = for
      _                <- sessionManager.saveSession(stateWithText("current"))
      existsAfterSave  <- sessionManager.sessionExists
      _                <- sessionManager.clearSession()
      existsAfterClear <- sessionManager.sessionExists
    yield
      existsAfterSave.shouldBe(true)
      existsAfterClear.shouldBe(false)

    program.unsafeRunSync()
  }

  it should "persist dirty buffer content even when persistUnsavedBuffers is false" in {
    val sessionManager = createManager(SessionManager.SessionPolicy(persistUnsavedBuffers = false))

    val program = for
      sessionId <- sessionManager.saveSessionAs("Draft", dirtyStateWithText("unsaved work"))
      loaded    <- sessionManager.loadSession(sessionId)
    yield loaded.map(_.persisted.buffers.values.head.document.content.toString).shouldBe(Some("unsaved work"))

    program.unsafeRunSync()
  }

  it should "restore clean file-backed buffers from disk when persisted session content is absent" in {
    val sessionManager = createManager(SessionManager.SessionPolicy(persistUnsavedBuffers = false))

    val program = for
      state     <- cleanFileStateWithText("saved on disk")
      sessionId <- sessionManager.saveSessionAs("File clean", state)
      loaded    <- sessionManager.loadSession(sessionId)
    yield
      loaded.map(_.persisted.buffers.values.head.document.content.toString) shouldBe Some("saved on disk")
      loaded.map(_.persisted.buffers.values.head.document.isDirty) shouldBe Some(false)

    program.unsafeRunSync()
  }

  it should "preserve dirty file-backed buffer content over disk when persistUnsavedBuffers is false" in {
    val sessionManager = createManager(SessionManager.SessionPolicy(persistUnsavedBuffers = false))

    val program = for
      state     <- dirtyFileStateWithText("saved on disk", "unsaved work")
      sessionId <- sessionManager.saveSessionAs("File draft", state)
      loaded    <- sessionManager.loadSession(sessionId)
    yield
      loaded.map(_.persisted.buffers.values.head.document.content.toString) shouldBe Some("unsaved work")
      loaded.map(_.persisted.buffers.values.head.document.isDirty) shouldBe Some(true)

    program.unsafeRunSync()
  }

  it should "prune the oldest named sessions when maxSessionHistory is exceeded" in {
    val sessionManager = createManager(SessionManager.SessionPolicy(maxSessionHistory = 2))

    val program = for
      _        <- sessionManager.saveSessionAs("First", stateWithText("a"))
      _        <- sessionManager.saveSessionAs("Second", stateWithText("b"))
      _        <- sessionManager.saveSessionAs("Third", stateWithText("c"))
      sessions <- sessionManager.listSessions()
    yield sessions.map(_.displayName) shouldBe List("Second", "Third")

    program.unsafeRunSync()
  }

  it should "round-trip content through saveSession and loadSession with no session ID" in {
    val sessionManager = createManager()

    val program = for
      _      <- sessionManager.saveSession(stateWithText("current session content"))
      loaded <- sessionManager.loadSession()
    yield loaded.map(_.persisted.buffers.values.head.document.content.toString) shouldBe Some("current session content")

    program.unsafeRunSync()
  }

  it should "return None and keep a quarantined copy when the saved session file is malformed" in {
    val sessionRoot    = Files.createTempDirectory("session-manager-corrupt")
    val sessionManager = createManagerAt(sessionRoot)
    val sessionFile    = currentSessionFile(sessionRoot)

    val program = for
      _      <- sessionManager.saveSession(stateWithText("recoverable"))
      _      <- IO.blocking(Files.writeString(sessionFile, "{ this is not valid json"))
      loaded <- sessionManager.loadSession()
    yield
      loaded shouldBe None
      Files.exists(sessionFile) shouldBe true
      Files.readString(sessionFile) should include("not valid json")
      quarantinedSessionFiles(sessionRoot) should not be empty

    program.unsafeRunSync()
  }

  it should "restore older session config JSON with formerly required fields missing" in {
    val sessionRoot    = Files.createTempDirectory("session-manager-legacy-config")
    val sessionManager = createManagerAt(sessionRoot)
    val sessionFile    = currentSessionFile(sessionRoot)
    val missingLegacyConfigKeys = List(
      "characterAnimation",
      "syntaxHighlightingEnabled",
      "fontConfig",
      "minimumPaneWidth",
      "showLineNumbers",
      "showGutter"
    )

    val program = for
      _            <- sessionManager.saveSession(stateWithText("legacy config"))
      originalJson <- IO.blocking(_root_.io.circe.parser.parse(Files.readString(sessionFile))).rethrow
      configObject <- IO.fromOption(originalJson.hcursor.downField("config").focus.flatMap(_.asObject))(
        new RuntimeException("Expected session config object")
      )
      migratedJson = originalJson.mapObject { sessionObject =>
        sessionObject
          .remove("schemaVersion")
          .add(
            "config",
            _root_.io.circe.Json.fromJsonObject(missingLegacyConfigKeys.foldLeft(configObject)(_.remove(_)))
          )
      }
      _      <- IO.blocking(Files.writeString(sessionFile, migratedJson.spaces2))
      loaded <- sessionManager.loadSession()
    yield
      loaded.map(_.persisted.buffers.values.head.document.content.toString) shouldBe Some("legacy config")
      loaded.map(_.persisted.config.editorConfig.characterAnimation) shouldBe Some(
        AppConfig.default.editorConfig.characterAnimation
      )
      loaded.map(_.persisted.config.languageToolsConfig.syntaxHighlightingEnabled) shouldBe Some(
        AppConfig.default.languageToolsConfig.syntaxHighlightingEnabled
      )
      loaded.map(_.persisted.config.editorConfig.fontConfig) shouldBe Some(AppConfig.default.editorConfig.fontConfig)
      loaded.map(_.persisted.config.editorConfig.minimumPaneWidth) shouldBe Some(
        AppConfig.default.editorConfig.minimumPaneWidth
      )
      loaded.map(_.persisted.config.surfaceConfig.showLineNumbers) shouldBe Some(
        AppConfig.default.surfaceConfig.showLineNumbers
      )
      loaded.map(_.persisted.config.surfaceConfig.showGutter) shouldBe Some(AppConfig.default.surfaceConfig.showGutter)

    program.unsafeRunSync()
  }

  it should "return None and quarantine sessions written by a newer schema version" in {
    val sessionRoot    = Files.createTempDirectory("session-manager-future-schema")
    val sessionManager = createManagerAt(sessionRoot)
    val sessionFile    = currentSessionFile(sessionRoot)

    val program = for
      _            <- sessionManager.saveSession(stateWithText("future schema"))
      originalJson <- IO.blocking(_root_.io.circe.parser.parse(Files.readString(sessionFile))).rethrow
      futureJson = originalJson.mapObject(_.add("schemaVersion", _root_.io.circe.Json.fromInt(999)))
      _      <- IO.blocking(Files.writeString(sessionFile, futureJson.spaces2))
      loaded <- sessionManager.loadSession()
    yield
      loaded shouldBe None
      quarantinedSessionFiles(sessionRoot) should not be empty

    program.unsafeRunSync()
  }

  it should "preserve config fields including blurRadius through full disk save/load" in {
    val sessionManager = createManager()

    val initial = AppState.initial
    val state =
      initial.copy(persisted =
        initial.persisted.copy(config =
          AppConfig(surfaceConfig = SurfaceConfig(blurRadius = 0.75f, showLineNumbers = false))
        )
      )

    val program = for
      _      <- sessionManager.saveSession(state)
      loaded <- sessionManager.loadSession()
    yield
      loaded.map(_.persisted.config.surfaceConfig.blurRadius) shouldBe Some(0.75f)
      loaded.map(_.persisted.config.surfaceConfig.showLineNumbers) shouldBe Some(false)

    program.unsafeRunSync()
  }

  it should "never prune the current auto-save session regardless of maxSessionHistory" in {
    val sessionManager = createManager(SessionManager.SessionPolicy(maxSessionHistory = 1))

    val program = for
      _             <- sessionManager.saveSession(stateWithText("auto"))
      _             <- sessionManager.saveSessionAs("Named", stateWithText("named"))
      sessions      <- sessionManager.listSessions()
      currentExists <- sessionManager.sessionExists
    yield
      currentExists shouldBe true
      sessions.exists(_.displayName == "Named") shouldBe true

    program.unsafeRunSync()
  }
