package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class DebugScrollingIssueSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Debug Horizontal Scrolling Issue" should "trace exactly what happens during character insertion and horizontal scrolling" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Debug"))
    val stateManager        = StateManager.apply(logger).unsafeRunSync()

    // Create empty buffer
    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager
      .updateState { current =>
        current.copy(
          buffers = current.buffers.updated(
            bufferId,
            current.buffers(bufferId).copy(language = Some(LanguageId.Scala))
          )
        )
      }
      .unsafeRunSync()

    println("=== HORIZONTAL SCROLLING DEBUG ===")
    println(s"BufferId: $bufferId, PaneId: $paneId")

    val initialState  = stateManager.getCurrentState.unsafeRunSync()
    val initialBuffer = initialState.buffers(bufferId)
    println(s"Initial buffer viewport: ${initialBuffer.viewport}")
    println(s"Initial cursor: ${initialBuffer.cursors.head}")

    // Insert long text like the failing test
    val alphabet = "abcdefghijklmnopqrstuvwxyz"
    val longText = alphabet * 5 // 130 characters

    println(s"=== INSERTING ${longText.length} CHARACTERS ===")
    longText.zipWithIndex.foreach {
      case (char, idx) =>
        stateManager.applyEvent(InsertChar(char)).unsafeRunSync()
        if idx % 10 == 9 then // Print every 10th character
          val currentState = stateManager.getCurrentState.unsafeRunSync()
          val buffer       = currentState.buffers(bufferId)
          println(s"After ${idx + 1} chars: cursor=${buffer.cursors.head}, viewport=${buffer.viewport}")
    }

    val finalState  = stateManager.getCurrentState.unsafeRunSync()
    val finalBuffer = finalState.buffers(bufferId)

    println("=== FINAL STATE ===")
    println(s"Final cursor: ${finalBuffer.cursors.head}")
    println(s"Final viewport: ${finalBuffer.viewport}")
    println(s"Expected leftColumn > 0, Actual: ${finalBuffer.viewport.leftColumn}")

    // The assertion that should pass
    finalBuffer.viewport.leftColumn should be > 0
  }
