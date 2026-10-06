package com.serenity

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.io.CoarseTickWorld
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.models.*
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** The session manager's stamp-vouched shortcuts (the cached index, the skipped no-op save) must sample the clock
  * before they stat, or a same-size rewrite landing inside a coarse timestamp tick is served stale or never repaired.
  * See [[com.serenity.io.FileStamp.vouchesForContent]].
  */
class SessionManagerFileStampOrderingSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def text(path: Path): String = Files.readString(path, StandardCharsets.UTF_8)

  private def state: AppState =
    val initial  = AppState.initial
    val bufferId = initial.persisted.bufferOrder.head
    initial.copy(persisted = initial.persisted.copy(buffers = Map(bufferId -> Buffer.fromString(bufferId, "hello"))))

  private def managerOver(root: Path, world: CoarseTickWorld): SessionManager =
    SessionManager(
      root,
      AppThemeManager.create,
      LoggerFactory[IO].getLogger(using LoggerName("SessionManagerFileStampOrderingSpec")),
      SessionManager.SessionPolicy(),
      world.clock,
      world.attributes
    )

  private def renameInIndex(content: Array[Byte]): Array[Byte] =
    new String(content, StandardCharsets.UTF_8).replace("Last Session", "Lost Session").getBytes(StandardCharsets.UTF_8)

  private def fillWithX(content: Array[Byte]): Array[Byte] = Array.fill(content.length)('x'.toByte)

  private def displayNames(manager: SessionManager): List[String] =
    manager.listSessions().unsafeRunSync().map(_.displayName)

  "readIndex" should "not cache an index an in-tick rewrite has since replaced" in {
    val root = Files.createTempDirectory("session-stamp-ordering")
    managerOver(root, CoarseTickWorld(root.resolve("unused"), identity)).saveSession(state).unsafeRunSync()
    val world   = CoarseTickWorld(root.resolve("session-index.json"), renameInIndex)
    val manager = managerOver(root, world)

    displayNames(manager) shouldBe List("Last Session")
    world.settle.unsafeRunSync()

    displayNames(manager) shouldBe List("Lost Session")
  }

  "remember" should "not cache the index a save wrote once an in-tick rewrite has replaced it" in {
    val root    = Files.createTempDirectory("session-stamp-ordering")
    val world   = CoarseTickWorld(root.resolve("session-index.json"), renameInIndex)
    val manager = managerOver(root, world)

    manager.saveSession(state).unsafeRunSync()
    world.settle.unsafeRunSync()

    displayNames(manager) shouldBe List("Lost Session")
  }

  "commitCurrent" should "not skip the next save because of a stamp an in-tick rewrite has outlived" in {
    val root        = Files.createTempDirectory("session-stamp-ordering")
    val sessionFile = root.resolve("sessions").resolve("session.json")
    val world       = CoarseTickWorld(sessionFile, fillWithX)
    val manager     = managerOver(root, world)

    manager.saveSession(state).unsafeRunSync()
    world.settle.unsafeRunSync()
    manager.saveSession(state).unsafeRunSync()

    text(sessionFile) should not include "xxxxxxxx"
  }
