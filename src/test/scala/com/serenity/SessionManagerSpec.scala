package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.{AppState, Buffer}
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
    val themeManager  = AppThemeManager.create
    val logger        = LoggerFactory[IO].getLogger(using LoggerName("SessionManagerSpec"))
    SessionManager.create(tempDirectory, themeManager, logger, policy)

  private def dirtyStateWithText(text: String): AppState =
    val initial  = AppState.initial
    val bufferId = initial.bufferOrder.head
    val buffer   = Buffer.fromString(bufferId, text).copy(isDirty = true)
    initial.copy(buffers = Map(bufferId -> buffer))

  private def stateWithText(text: String): AppState =
    val initial  = AppState.initial
    val bufferId = initial.bufferOrder.head
    val buffer   = Buffer.fromString(bufferId, text)
    initial.copy(buffers = Map(bufferId -> buffer))

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
      loadedFirst.map(_.buffers.values.head.content.toString).shouldBe(Some("alpha"))
      loadedSecond.map(_.buffers.values.head.content.toString).shouldBe(Some("beta"))

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

  it should "not persist dirty buffer content when persistUnsavedBuffers is false" in {
    val sessionManager = createManager(SessionManager.SessionPolicy(persistUnsavedBuffers = false))

    val program = for
      sessionId <- sessionManager.saveSessionAs("Draft", dirtyStateWithText("unsaved work"))
      loaded    <- sessionManager.loadSession(sessionId)
    yield loaded.map(_.buffers.values.head.content.toString).shouldBe(Some(""))

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
    yield loaded.map(_.buffers.values.head.content.toString) shouldBe Some("current session content")

    program.unsafeRunSync()
  }

  it should "preserve config fields including blurRadius through full disk save/load" in {
    val sessionManager = createManager()
    import com.serenity.config.AppConfig

    val state = AppState.initial.copy(config = AppConfig(blurRadius = 0.75f, showLineNumbers = false))

    val program = for
      _      <- sessionManager.saveSession(state)
      loaded <- sessionManager.loadSession()
    yield
      loaded.map(_.config.blurRadius) shouldBe Some(0.75f)
      loaded.map(_.config.showLineNumbers) shouldBe Some(false)

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
