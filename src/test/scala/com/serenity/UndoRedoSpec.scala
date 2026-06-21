package com.serenity

import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class UndoRedoSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

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

    applyEvent(InsertChar('a')) // group 1
    applyEvent(InsertChar('b')) // group 1 (coalesced)
    applyEvent(DeleteBackward)  // atomic
    applyEvent(InsertChar('c')) // group 2
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

  it should "bound undo history to the configured maximum depth" in new UndoFixture:
    override def sessionPolicy: SessionManager.SessionPolicy =
      SessionManager.SessionPolicy(maxUndoDepth = 2)

    val bufferId = setupBuffer("abcd")

    applyEvent(DeleteBackward)
    applyEvent(DeleteBackward)
    applyEvent(DeleteBackward)
    getContent(bufferId) shouldBe "a"

    applyEvent(Undo)
    getContent(bufferId) shouldBe "ab"

    applyEvent(Undo)
    getContent(bufferId) shouldBe "abc"

    applyEvent(Undo)
    getContent(bufferId) shouldBe "abc"

  behavior of "Multi-cursor undo/redo"

  it should "undo and redo a grouped multi-cursor insertion with the full cursor set" in new UndoFixture:
    val bufferId = setupBuffer("abcd")
    setCursors(bufferId, List(CursorPosition(0, 1), CursorPosition(0, 3)))

    applyEvent(InsertChar('X'))
    applyEvent(InsertChar('Y'))
    getContent(bufferId) shouldBe "aXYbcXYd"
    getCursors(bufferId) shouldBe List(CursorPosition(0, 3), CursorPosition(0, 7))

    applyEvent(Undo)
    getContent(bufferId) shouldBe "abcd"
    getCursors(bufferId) shouldBe List(CursorPosition(0, 1), CursorPosition(0, 3))

    applyEvent(Redo)
    getContent(bufferId) shouldBe "aXYbcXYd"
    getCursors(bufferId) shouldBe List(CursorPosition(0, 3), CursorPosition(0, 7))

  it should "undo and redo a multi-cursor newline insertion with all cursors restored" in new UndoFixture:
    val bufferId = setupBuffer("abcd")
    setCursors(bufferId, List(CursorPosition(0, 1), CursorPosition(0, 3)))

    applyEvent(NewLine)
    getContent(bufferId) shouldBe "a\nbc\nd"
    getCursors(bufferId) shouldBe List(CursorPosition(1, 0), CursorPosition(2, 0))

    applyEvent(Undo)
    getContent(bufferId) shouldBe "abcd"
    getCursors(bufferId) shouldBe List(CursorPosition(0, 1), CursorPosition(0, 3))

    applyEvent(Redo)
    getContent(bufferId) shouldBe "a\nbc\nd"
    getCursors(bufferId) shouldBe List(CursorPosition(1, 0), CursorPosition(2, 0))

  it should "undo and redo multi-cursor paste as an atomic edit" in new UndoFixture:
    val bufferId = setupBuffer("ab")
    setCursors(bufferId, List(CursorPosition(0, 0), CursorPosition(0, 2)))
    setClipboard("Z")

    applyEvent(Paste)
    getContent(bufferId) shouldBe "ZabZ"
    getCursors(bufferId) shouldBe List(CursorPosition(0, 1), CursorPosition(0, 4))

    applyEvent(Undo)
    getContent(bufferId) shouldBe "ab"
    getCursors(bufferId) shouldBe List(CursorPosition(0, 0), CursorPosition(0, 2))

    applyEvent(Redo)
    getContent(bufferId) shouldBe "ZabZ"
    getCursors(bufferId) shouldBe List(CursorPosition(0, 1), CursorPosition(0, 4))

  it should "undo and redo multi-selection replacement with selections restored on undo" in new UndoFixture:
    val bufferId = setupBuffer("alpha beta gamma")
    val first    = Selection(CursorPosition(0, 0), CursorPosition(0, 5))
    val second   = Selection(CursorPosition(0, 11), CursorPosition(0, 16))
    setSelections(bufferId, List(first, second))

    applyEvent(InsertChar('X'))
    getContent(bufferId) shouldBe "X beta X"
    getCursors(bufferId) shouldBe List(CursorPosition(0, 1), CursorPosition(0, 8))

    applyEvent(Undo)
    getContent(bufferId) shouldBe "alpha beta gamma"
    getCursors(bufferId) shouldBe List(CursorPosition(0, 5), CursorPosition(0, 16))
    getState.buffers(bufferId).allSelections shouldBe List(first, second)

    applyEvent(Redo)
    getContent(bufferId) shouldBe "X beta X"
    getCursors(bufferId) shouldBe List(CursorPosition(0, 1), CursorPosition(0, 8))
    getState.buffers(bufferId).allSelections shouldBe Nil

  it should "restore viewport, find state, and empty-buffer state from undo snapshots" in new UndoFixture:
    val bufferId = setupBuffer("alpha\nbeta\nalpha")
    val beforeBuffer = getState
      .buffers(bufferId)
      .copy(
        cursors = List(CursorPosition(2, 0)),
        preferredColumn = Some(0),
        viewport = Viewport(topLine = 2, leftColumn = 1, visibleLines = 8, visibleColumns = 40),
        findState = Some(FindState("alpha", List(FindResult(0, 0), FindResult(2, 0)), 1)),
        isNewEmpty = true
      )

    updateBuffer(bufferId, beforeBuffer)

    applyEvent(InsertChar('!'))

    val edited = getState.buffers(bufferId)
    edited.isNewEmpty shouldBe false
    updateBuffer(
      bufferId,
      edited.copy(
        viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 24, visibleColumns = 80),
        findState = None,
        isNewEmpty = false
      )
    )

    applyEvent(Undo)

    val undone = getState.buffers(bufferId)
    undone.content.collect() shouldBe "alpha\nbeta\nalpha"
    undone.cursors shouldBe List(CursorPosition(2, 0))
    undone.preferredColumn shouldBe Some(0)
    undone.viewport shouldBe beforeBuffer.viewport
    undone.findState shouldBe Some(FindState("alpha", List(FindResult(0, 0), FindResult(2, 0)), 1))
    undone.isNewEmpty shouldBe true

  it should "undo and redo multi-cursor cut with the full cursor set" in new UndoFixture:
    val bufferId = setupBuffer("alpha\nbeta\ngamma\ndelta")
    setCursors(bufferId, List(CursorPosition(0, 1), CursorPosition(2, 2)))

    applyEvent(Cut)
    getContent(bufferId) shouldBe "beta\ndelta"
    getCursors(bufferId) shouldBe List(CursorPosition(0, 0), CursorPosition(1, 0))

    applyEvent(Undo)
    getContent(bufferId) shouldBe "alpha\nbeta\ngamma\ndelta"
    getCursors(bufferId) shouldBe List(CursorPosition(0, 1), CursorPosition(2, 2))

    applyEvent(Redo)
    getContent(bufferId) shouldBe "beta\ndelta"
    getCursors(bufferId) shouldBe List(CursorPosition(0, 0), CursorPosition(1, 0))

  behavior of "Cross-pane undo"

  it should "snap focus to the pane where the edit happened when undoing" in new UndoFixture:
    val bufferId1 = setupBuffer("pane one")
    val pane1     = getPaneId

    applyEvent(InsertChar('!'))

    setupAnotherBuffer("pane two")
    getPaneId

    // Focus is on pane2; undo should snap back to pane1
    applyEvent(Undo)

    val state = getState
    state.focus shouldBe Focus.EditorPane(pane1)
    getContent(bufferId1) shouldBe "pane one"

  trait UndoFixture:

    def sessionPolicy: SessionManager.SessionPolicy =
      SessionManager.SessionPolicy()

    val stateManager: StateManager = StateManager
      .apply(LoggerFactory[IO].getLogger(using LoggerName("UndoRedoSpec")), policy = sessionPolicy)
      .unsafeRunSync()

    private val currentPaneId = AtomicReference[PaneId](PaneId(0))

    def setupBuffer(content: String): BufferId =
      val bufferId = stateManager.createBuffer(content).unsafeRunSync()
      val state    = stateManager.getCurrentState.unsafeRunSync()
      currentPaneId.set(state.layout.editorPanes.keys.head)
      stateManager.setBufferForPane(currentPaneId.get(), bufferId).unsafeRunSync()
      if content.nonEmpty then
        stateManager
          .setCursorPosition(currentPaneId.get(), 0, content.length)
          .unsafeRunSync()
      bufferId

    def setupAnotherBuffer(content: String): BufferId =
      val bufferId  = stateManager.createBuffer(content).unsafeRunSync()
      val newPaneId = stateManager.createPane(Some(bufferId)).unsafeRunSync()
      currentPaneId.set(newPaneId)
      if content.nonEmpty then
        stateManager
          .setCursorPosition(newPaneId, 0, content.length)
          .unsafeRunSync()
      stateManager.switchToPane(newPaneId).unsafeRunSync()
      bufferId

    def getPaneId: PaneId = currentPaneId.get()

    def applyEvent(event: Event): Unit =
      stateManager.applyEvent(event).unsafeRunSync()

    def getState: AppState =
      stateManager.getCurrentState.unsafeRunSync()

    def getContent(bufferId: BufferId): String =
      getState.buffers.get(bufferId).map(_.content.collect()).getOrElse("")

    def getCursor: CursorPosition =
      getState.activeCursorPosition.getOrElse(CursorPosition(0, 0))

    def getCursors(bufferId: BufferId): List[CursorPosition] =
      getState.buffers(bufferId).cursors

    def setCursors(bufferId: BufferId, cursors: List[CursorPosition]): Unit =
      stateManager
        .updateState { state =>
          state.copy(
            buffers = state.buffers.updated(
              bufferId,
              state.buffers(bufferId).copy(cursors = cursors, selection = None, selections = Nil)
            )
          )
        }
        .unsafeRunSync()

    def setSelections(bufferId: BufferId, selections: List[Selection]): Unit =
      stateManager
        .updateState { state =>
          state.copy(
            buffers = state.buffers.updated(
              bufferId,
              state
                .buffers(bufferId)
                .copy(
                  cursors = selections.map(_.focus),
                  selection = selections.headOption,
                  selections = selections
                )
            )
          )
        }
        .unsafeRunSync()

    def setClipboard(text: String): Unit =
      stateManager.updateState(_.copy(clipboard = Some(text))).unsafeRunSync()

    def updateBuffer(bufferId: BufferId, buffer: Buffer): Unit =
      stateManager.updateState(state => state.copy(buffers = state.buffers.updated(bufferId, buffer))).unsafeRunSync()
