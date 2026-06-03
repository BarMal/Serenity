package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class UndoRedoSpec extends AnyFlatSpec with Matchers:

  given Balance            = Balance.default
  given LoggerFactory[IO]  = Slf4jFactory.create[IO]

  behavior of "Undo"

  it should "undo a single InsertChar, restoring original content and cursor" in new UndoFixture:
    val bufferId = setupBuffer("hello")

    applyEvent(InsertChar('!'))
    getContent(bufferId) shouldBe "hello!"

    applyEvent(Undo)
    getContent(bufferId) shouldBe "hello"
    getCursor shouldBe CursorPosition(0, 5)

  it should "coalesce consecutive InsertChar events into one undo step" in new UndoFixture:
    val bufferId = setupBuffer("")

    applyEvent(InsertChar('h'))
    applyEvent(InsertChar('i'))
    applyEvent(InsertChar('!'))
    getContent(bufferId) shouldBe "hi!"

    applyEvent(Undo)
    getContent(bufferId) shouldBe ""

  it should "seal the InsertChar group when a DeleteBackward follows" in new UndoFixture:
    val bufferId = setupBuffer("")

    applyEvent(InsertChar('a'))
    applyEvent(InsertChar('b'))
    applyEvent(DeleteBackward)
    getContent(bufferId) shouldBe "a"

    applyEvent(Undo) // undo the delete
    getContent(bufferId) shouldBe "ab"

    applyEvent(Undo) // undo the "ab" group
    getContent(bufferId) shouldBe ""

  it should "undo a DeleteBackward as an atomic step" in new UndoFixture:
    val bufferId = setupBuffer("hi")

    applyEvent(DeleteBackward)
    getContent(bufferId) shouldBe "h"

    applyEvent(Undo)
    getContent(bufferId) shouldBe "hi"

  it should "undo a NewLine insertion" in new UndoFixture:
    val bufferId = setupBuffer("line")

    applyEvent(NewLine)
    getContent(bufferId) shouldBe "line\n"

    applyEvent(Undo)
    getContent(bufferId) shouldBe "line"

  it should "be a no-op when the undo stack is empty" in new UndoFixture:
    val bufferId = setupBuffer("stable")

    applyEvent(Undo)
    getContent(bufferId) shouldBe "stable"

  behavior of "Redo"

  it should "redo an undone InsertChar" in new UndoFixture:
    val bufferId = setupBuffer("")

    applyEvent(InsertChar('x'))
    applyEvent(Undo)
    getContent(bufferId) shouldBe ""

    applyEvent(Redo)
    getContent(bufferId) shouldBe "x"

  it should "redo an undone coalesced InsertChar group" in new UndoFixture:
    val bufferId = setupBuffer("")

    applyEvent(InsertChar('a'))
    applyEvent(InsertChar('b'))
    applyEvent(InsertChar('c'))
    applyEvent(Undo)
    getContent(bufferId) shouldBe ""

    applyEvent(Redo)
    getContent(bufferId) shouldBe "abc"

  it should "be a no-op when the redo stack is empty" in new UndoFixture:
    val bufferId = setupBuffer("stable")

    applyEvent(Redo)
    getContent(bufferId) shouldBe "stable"

  it should "clear the redo stack when a new edit is made after undo" in new UndoFixture:
    val bufferId = setupBuffer("")

    applyEvent(InsertChar('a'))
    applyEvent(Undo)
    applyEvent(InsertChar('b')) // new edit clears redo
    applyEvent(Redo)            // redo stack is empty — no-op

    getContent(bufferId) shouldBe "b"

  behavior of "Multi-level undo/redo"

  it should "support multiple undo levels across mixed operations" in new UndoFixture:
    val bufferId = setupBuffer("")

    applyEvent(InsertChar('a'))   // group 1
    applyEvent(InsertChar('b'))   // group 1 (coalesced)
    applyEvent(DeleteBackward)    // atomic
    applyEvent(InsertChar('c'))   // group 2
    getContent(bufferId) shouldBe "ac"

    applyEvent(Undo) // undo group 2 → "a"
    getContent(bufferId) shouldBe "a"

    applyEvent(Undo) // undo delete → "ab"
    getContent(bufferId) shouldBe "ab"

    applyEvent(Undo) // undo group 1 → ""
    getContent(bufferId) shouldBe ""

    applyEvent(Redo) // redo group 1 → "ab"
    getContent(bufferId) shouldBe "ab"

    applyEvent(Redo) // redo delete → "a"
    getContent(bufferId) shouldBe "a"

    applyEvent(Redo) // redo group 2 → "ac"
    getContent(bufferId) shouldBe "ac"

  behavior of "Cross-pane undo"

  it should "snap focus to the pane where the edit happened when undoing" in new UndoFixture:
    val bufferId1 = setupBuffer("pane one")
    val pane1     = getPaneId

    applyEvent(InsertChar('!'))

    val bufferId2 = setupAnotherBuffer("pane two")
    val pane2     = getPaneId

    // Focus is on pane2; undo should snap back to pane1
    applyEvent(Undo)

    val state = getState
    state.focus shouldBe Focus.EditorPane(pane1)
    getContent(bufferId1) shouldBe "pane one"

  trait UndoFixture:
    val stateManager: StateManager = StateManager
      .apply(LoggerFactory[IO].getLogger(using LoggerName("UndoRedoSpec")))
      .unsafeRunSync()

    private var currentPaneId: PaneId = PaneId(0)

    def setupBuffer(content: String): BufferId =
      val bufferId = stateManager.createBuffer(content).unsafeRunSync()
      val state    = stateManager.getCurrentState.unsafeRunSync()
      currentPaneId = state.layout.editorPanes.keys.head
      stateManager.setBufferForPane(currentPaneId, bufferId).unsafeRunSync()
      if content.nonEmpty then
        stateManager
          .setCursorPosition(currentPaneId, 0, content.length)
          .unsafeRunSync()
      bufferId

    def setupAnotherBuffer(content: String): BufferId =
      val bufferId = stateManager.createBuffer(content).unsafeRunSync()
      val newPaneId = stateManager.createPane(Some(bufferId)).unsafeRunSync()
      currentPaneId = newPaneId
      if content.nonEmpty then
        stateManager
          .setCursorPosition(newPaneId, 0, content.length)
          .unsafeRunSync()
      stateManager.switchToPane(newPaneId).unsafeRunSync()
      bufferId

    def getPaneId: PaneId = currentPaneId

    def applyEvent(event: Event): Unit =
      stateManager.applyEvent(event).unsafeRunSync()

    def getState: AppState =
      stateManager.getCurrentState.unsafeRunSync()

    def getContent(bufferId: BufferId): String =
      getState.buffers.get(bufferId).map(_.content.collect()).getOrElse("")

    def getCursor: CursorPosition =
      getState.activeCursorPosition.getOrElse(CursorPosition(0, 0))
