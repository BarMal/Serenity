package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.input.{ClipboardEventSync, SystemClipboard}
import com.serenity.keystroke.events.{Copy, Paste}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.{LoggerFactory, LoggerName}
import org.typelevel.log4cats.slf4j.Slf4jFactory

class ClipboardEventSyncSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  "ClipboardEventSync.beforeEvent" should "import system clipboard text before paste" in new ClipboardFixture:
    val bufferId = setupBuffer("world")
    setCursor(0, 0)
    clipboard.seed("hello ")

    ClipboardEventSync.beforeEvent(Paste, stateManager, clipboard).unsafeRunSync()
    stateManager.applyEvent(Paste).unsafeRunSync()

    getContent(bufferId) shouldBe "hello world"

  it should "leave the internal clipboard unchanged when system clipboard is empty" in new ClipboardFixture:
    setupBuffer("world")
    stateManager.updateState(_.copy(clipboard = Some("existing"))).unsafeRunSync()

    ClipboardEventSync.beforeEvent(Paste, stateManager, clipboard).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().clipboard shouldBe Some("existing")

  "ClipboardEventSync.afterEvent" should "export copied text to the system clipboard" in new ClipboardFixture:
    setupBuffer("copied line")

    stateManager.applyEvent(Copy).unsafeRunSync()
    ClipboardEventSync.afterEvent(Copy, stateManager, clipboard).unsafeRunSync()

    clipboard.snapshot shouldBe Some("copied line")

  trait ClipboardFixture:
    val stateManager: StateManager = StateManager
      .apply(LoggerFactory[IO].getLogger(using LoggerName("ClipboardEventSyncSpec")))
      .unsafeRunSync()

    private val paneId = PaneId(0)

    val clipboard: TestClipboard = new TestClipboard

    def setupBuffer(content: String): BufferId =
      val bufferId = stateManager.createBuffer(content).unsafeRunSync()
      stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
      bufferId

    def setCursor(line: Int, column: Int): Unit =
      stateManager.setCursorPosition(paneId, line, column).unsafeRunSync()

    def getContent(bufferId: BufferId): String =
      stateManager.getCurrentState.unsafeRunSync().buffers(bufferId).content.collect()

  final class TestClipboard extends SystemClipboard[IO]:
    private var current: Option[String] = None

    def seed(text: String): Unit =
      current = Some(text)

    def snapshot: Option[String] =
      current

    override def readText: IO[Option[String]] =
      IO.pure(current)

    override def writeText(text: String): IO[Unit] =
      IO {
        current = Some(text)
      }
