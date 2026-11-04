package com.serenity

import java.nio.file.Path

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.io.SettledClock
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.AppState
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

/** An auto-save after a file save, the edit-idle save and the interval save can all fire together. Each stages its
  * write beside the session file under one shared name, so without serialisation one save's rename took the other's
  * staged file away and the loser failed with `NoSuchFileException` ("Auto-save after file save failed").
  */
class SessionConcurrentSaveSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val Rounds  = 25
  private val Writers = 16

  private def newManager(root: Path): SessionManager =
    SessionManager(
      root,
      AppThemeManager.create,
      NoOpLogger.impl[IO],
      SessionManager.SessionPolicy(),
      clock = SettledClock.aMinuteAhead
    )

  private def stateRecalling(file: String): AppState =
    val initial = AppState.initial
    initial.copy(persisted = initial.persisted.copy(recentFiles = List(Path.of(s"/notes/$file.txt"))))

  "Concurrent session saves" should "all succeed rather than losing one to a vanished staged file" in {
    val manager = newManager(TestTemp.directory("session-concurrent-save"))

    val outcomes = (0 until Rounds).toList.flatTraverse { round =>
      (0 until Writers).toList.parTraverse(writer =>
        manager.saveSession(stateRecalling(s"r$round-w$writer")).attempt
      )
    }.unsafeRunSync()

    outcomes.collect { case Left(error) => error.toString } shouldBe Nil
  }

  it should "leave a session that loads after the writers finish" in {
    val manager = newManager(TestTemp.directory("session-concurrent-save"))

    (0 until Writers).toList
      .parTraverse(writer => manager.saveSession(stateRecalling(s"w$writer")).attempt)
      .unsafeRunSync()

    manager.loadSession().unsafeRunSync().map(_.persisted.recentFiles.size) shouldBe Some(1)
  }
