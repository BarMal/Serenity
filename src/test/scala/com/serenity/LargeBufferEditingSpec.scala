package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class LargeBufferEditingSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "Editing large buffers"

  it should "insert correctly at the end of a very long single line" in new EditorFixture:
    val longLine = "a" * 10000
    val bufferId = stateManager.bufferManager.createBuffer(longLine, None).unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.persisted.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 0, longLine.length).unsafeRunSync()

    stateManager.applyEvent(InsertChar('X')).unsafeRunSync()

    val finalContent =
      stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).document.content.collect()
    finalContent.length shouldBe 10001
    finalContent.last shouldBe 'X'

  it should "insert correctly at the end of a buffer with many lines" in new EditorFixture:
    val manyLines = (1 to 1000).map(i => s"Line $i").mkString("\n")
    val bufferId  = stateManager.bufferManager.createBuffer(manyLines, None).unsafeRunSync()
    val state     = stateManager.getCurrentState.unsafeRunSync()
    val paneId    = state.persisted.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager.setCursorPosition(paneId, 999, "Line 1000".length).unsafeRunSync()

    stateManager.applyEvent(InsertChar('!')).unsafeRunSync()

    val finalContent =
      stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).document.content.collect()
    finalContent should endWith("Line 1000!")
    finalContent.count(_ == '\n') shouldBe 999

  trait EditorFixture:
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager: StateManager =
      StateManager.apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO]).unsafeRunSync()
