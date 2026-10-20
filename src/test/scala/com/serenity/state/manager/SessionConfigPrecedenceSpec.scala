package com.serenity.state.manager

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.config.AppConfig
import com.serenity.config.AppConfigOps.*
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.AppState
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** config.conf is the one source of truth for settings (#1934): a session remembers the workspace, never the settings,
  * so a file edited while the editor was closed is not undone by resuming a session saved before the edit.
  */
class SessionConfigPrecedenceSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val savedWith  = AppConfig.default.withWheelScrollLines(3)
  private val editedTo   = AppConfig.default.withWheelScrollLines(9)
  private val sessionDir = TestTemp.directory("session-config-precedence")

  "Resuming a session" should "keep the config the editor started with, not the one the session was saved with" in {
    val root = TestTemp.directory("session-config-precedence-root")
    val manager =
      SessionManager.create(root, AppThemeManager.create, NoOpLogger.impl[IO], SessionManager.SessionPolicy())
    val saved = AppState.initial(savedWith)
    manager.saveSession(saved).unsafeRunSync()

    val stateManager =
      StateManager
        .apply(
          LoggerFactory[IO].getLogger(using LoggerName("SessionConfigPrecedenceSpec")),
          sessionRootOverride = Some(root),
          initialConfig = editedTo,
          dictionaryCache = SharedDictionary.cacheFor(editedTo)
        )
        .unsafeRunSync()
    stateManager.sessionService.loadSession.unsafeRunSync().map(_.persisted.bufferOrder.nonEmpty) shouldBe Some(true)

    stateManager.composition.restoreStartupSession().unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.config.inputConfig.wheelScrollLines shouldBe 9
  }

  "A settings change" should "not save the session" in {
    val root = TestTemp.directory("session-config-no-save")
    val stateManager =
      StateManager
        .apply(
          LoggerFactory[IO].getLogger(using LoggerName("SessionConfigPrecedenceSpec")),
          sessionRootOverride = Some(root),
          configPersistencePath = Some(sessionDir.resolve("config.conf")),
          dictionaryCache = SharedDictionary.default
        )
        .unsafeRunSync()

    stateManager.composition.updateConfig(_.withWheelScrollLines(7)).unsafeRunSync()
    stateManager.runtimeLifecycle.awaitEffects.unsafeRunSync()

    stateManager.sessionStartupInfo.sessionExists.unsafeRunSync() shouldBe false
  }
