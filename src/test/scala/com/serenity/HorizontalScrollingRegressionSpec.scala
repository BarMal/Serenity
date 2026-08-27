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

class HorizontalScrollingRegressionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Horizontal scrolling" should "keep the cursor visible during long character insertion" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("HorizontalScrollingRegression"))
    val stateManager        = StateManager.apply(logger).unsafeRunSync()

    val bufferId = stateManager.createBuffer("").unsafeRunSync()
    val state    = stateManager.getCurrentState.unsafeRunSync()
    val paneId   = state.persisted.layout.editorPanes.keys.head
    stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
    stateManager
      .updateState { current =>
        current.copy(persisted =
          current.persisted.copy(
            config = current.persisted.config.withWordWrap(false),
            buffers = current.persisted.buffers.updated(
              bufferId,
              current.persisted
                .buffers(bufferId)
                .copy(document = current.persisted.buffers(bufferId).document.copy(language = Some(LanguageId.Scala)))
            )
          )
        )
      }
      .unsafeRunSync()

    val initialState  = stateManager.getCurrentState.unsafeRunSync()
    val initialBuffer = initialState.persisted.buffers(bufferId)
    initialBuffer.viewport.leftColumn shouldBe 0
    initialBuffer.editing.cursors.head.column shouldBe 0

    val alphabet = "abcdefghijklmnopqrstuvwxyz"
    val longText = alphabet * 5

    longText.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    val finalState  = stateManager.getCurrentState.unsafeRunSync()
    val finalBuffer = finalState.persisted.buffers(bufferId)

    finalBuffer.viewport.leftColumn should be > 0
    finalBuffer.editing.cursors.head.column shouldBe longText.length
    finalBuffer.editing.cursors.head.column should be >= finalBuffer.viewport.leftColumn
  }
