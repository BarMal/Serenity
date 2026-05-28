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

  given Balance = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def createManager(): SessionManager =
    val tempDirectory = Files.createTempDirectory("session-manager-spec")
    val themeManager = AppThemeManager.create
    val logger = LoggerFactory[IO].getLogger(using LoggerName("SessionManagerSpec"))
    SessionManager.create(tempDirectory, themeManager, logger)

  private def stateWithText(text: String): AppState =
    val initial = AppState.initial
    val bufferId = initial.bufferOrder.head
    val buffer = Buffer.fromString(bufferId, text)
    initial.copy(buffers = Map(bufferId -> buffer))

  "SessionManager" should "save and load named sessions through the index" in {
    val sessionManager = createManager()

    val program = for
      firstSessionId <- sessionManager.saveSessionAs("Daily notes", stateWithText("alpha"))
      secondSessionId <- sessionManager.saveSessionAs("Refactor branch", stateWithText("beta"))
      sessions <- sessionManager.listSessions()
      loadedFirst <- sessionManager.loadSession(firstSessionId)
      loadedSecond <- sessionManager.loadSession(secondSessionId)
    yield
      sessions.map(_.displayName).shouldBe(List("Daily notes", "Refactor branch"))
      loadedFirst.map(_.buffers.values.head.content.toString).shouldBe(Some("alpha"))
      loadedSecond.map(_.buffers.values.head.content.toString).shouldBe(Some("beta"))

    program.unsafeRunSync()
  }

  it should "rename and delete named sessions" in {
    val sessionManager = createManager()

    val program = for
      sessionId <- sessionManager.saveSessionAs("Scratchpad", stateWithText("notes"))
      _ <- sessionManager.renameSession(sessionId, "Project notes")
      renamedSessions <- sessionManager.listSessions()
      _ <- sessionManager.deleteSession(sessionId)
      sessionsAfterDelete <- sessionManager.listSessions()
      existsAfterDelete <- sessionManager.sessionExists
    yield
      renamedSessions.map(_.displayName).shouldBe(List("Project notes"))
      sessionsAfterDelete.shouldBe(Nil)
      existsAfterDelete.shouldBe(false)

    program.unsafeRunSync()
  }

  it should "clear the current saved session" in {
    val sessionManager = createManager()

    val program = for
      _ <- sessionManager.saveSession(stateWithText("current"))
      existsAfterSave <- sessionManager.sessionExists
      _ <- sessionManager.clearSession()
      existsAfterClear <- sessionManager.sessionExists
    yield
      existsAfterSave.shouldBe(true)
      existsAfterClear.shouldBe(false)

    program.unsafeRunSync()
  }
