package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** TDD specs for #1203: the word/character count tracks `AppState`'s buffer content as it changes, and reports the
  * current selection specifically when one is active.
  */
class WordCountStateSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default
  given LoggerFactory[IO]         = Slf4jFactory.create[IO]

  "activeBufferTextStatistics" should "update as the active buffer's content changes" in {
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)

      bufferId     <- stateManager.createBuffer("one two three")
      initialState <- stateManager.getCurrentState
      paneId = initialState.persisted.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)

      beforeEdit <- stateManager.getCurrentState
      _          <- stateManager.updateBuffer(bufferId, "one two three four five")
      afterEdit  <- stateManager.getCurrentState
    yield (beforeEdit, afterEdit)

    val (beforeEdit, afterEdit) = program.unsafeRunSync()

    beforeEdit.activeBufferTextStatistics.map(_.wordCount) shouldBe Some(3)
    afterEdit.activeBufferTextStatistics.map(_.wordCount) shouldBe Some(5)
  }

  it should "report no statistics when there is no active buffer" in {
    AppState.empty.activeBufferTextStatistics shouldBe None
  }

  "activeSelectionTextStatistics" should "reflect only the selected range, not the whole buffer" in {
    val buffer0 = Buffer.fromString(BufferId(1), "alpha beta gamma delta")
    val buffer = buffer0.copy(editing =
      buffer0.editing.copy(selection = Some(Selection(CursorPosition(0, 6), CursorPosition(0, 16)))) // "beta gamma"
    )
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0))
        ),
        focus = Focus.EditorPane(PaneId(0))
      )
    )

    state.activeSelectionTextStatistics.map(_.wordCount) shouldBe Some(2)
    state.activeBufferTextStatistics.map(_.wordCount) shouldBe Some(4)
  }

  it should "report no selection statistics when the selection is empty" in {
    val buffer0 = Buffer.fromString(BufferId(1), "alpha beta")
    val buffer = buffer0.copy(editing =
      buffer0.editing.copy(selection = Some(Selection(CursorPosition(0, 2), CursorPosition(0, 2))))
    )
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          paneOrder = List(PaneId(0))
        ),
        focus = Focus.EditorPane(PaneId(0))
      )
    )

    state.activeSelectionTextStatistics shouldBe None
  }
