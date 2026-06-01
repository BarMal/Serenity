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

class CopyPasteSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  behavior of "Copy"

  it should "copy the current line text to the clipboard" in new ClipFixture:
    setupBuffer("hello world")

    applyEvent(Copy)

    getClipboard shouldBe Some("hello world")

  it should "not modify the buffer content" in new ClipFixture:
    val bufferId = setupBuffer("untouched")

    applyEvent(Copy)

    getContent(bufferId) shouldBe "untouched"

  it should "not mark the buffer dirty" in new ClipFixture:
    val bufferId = setupBuffer("clean")

    applyEvent(Copy)

    getState.buffers(bufferId).isDirty shouldBe false

  it should "copy the correct line when cursor is on a non-first line" in new ClipFixture:
    val bufferId = setupBuffer("first\nsecond\nthird")
    setCursor(1, 0) // position on "second"

    applyEvent(Copy)

    getClipboard shouldBe Some("second")

  it should "overwrite the clipboard on a second copy" in new ClipFixture:
    setupBuffer("line one")
    applyEvent(Copy)

    val bufferId2 = setupBuffer("line two")
    applyEvent(Copy)

    getClipboard shouldBe Some("line two")

  it should "copy the active selection instead of the whole line" in new ClipFixture:
    setupBuffer("Hello World Program")
    setSelection(Selection(CursorPosition(0, 6), CursorPosition(0, 11)))

    applyEvent(Copy)

    getClipboard shouldBe Some("World")

  it should "copy a multiline selection exactly" in new ClipFixture:
    setupBuffer("alpha\nbeta\ngamma")
    setSelection(Selection(CursorPosition(0, 2), CursorPosition(1, 2)))

    applyEvent(Copy)

    getClipboard shouldBe Some("pha\nbe")

  behavior of "Paste"

  it should "insert clipboard content at the cursor position" in new ClipFixture:
    val bufferId = setupBuffer("world")
    setCursor(0, 0)

    getState  // warm up
    stateManager.updateState(_.copy(clipboard = Some("hello "))).unsafeRunSync()
    applyEvent(Paste)

    getContent(bufferId) shouldBe "hello world"

  it should "be a no-op when clipboard is empty" in new ClipFixture:
    val bufferId = setupBuffer("unchanged")

    applyEvent(Paste)

    getContent(bufferId) shouldBe "unchanged"

  it should "advance the cursor past the pasted text" in new ClipFixture:
    val bufferId = setupBuffer("")
    stateManager.updateState(_.copy(clipboard = Some("hi"))).unsafeRunSync()

    applyEvent(Paste)

    getCursor.column shouldBe 2

  it should "replace the active selection when pasting" in new ClipFixture:
    val bufferId = setupBuffer("Hello World Program")
    setSelection(Selection(CursorPosition(0, 6), CursorPosition(0, 11)))
    stateManager.updateState(_.copy(clipboard = Some("Universe"))).unsafeRunSync()

    applyEvent(Paste)

    getContent(bufferId) shouldBe "Hello Universe Program"
    getCursor shouldBe CursorPosition(0, 14)
    getState.buffers(bufferId).selection shouldBe None

  it should "place the cursor at the true multiline insertion end after paste" in new ClipFixture:
    val bufferId = setupBuffer("alpha\nomega")
    setCursor(0, 5)
    stateManager.updateState(_.copy(clipboard = Some("\nbeta\ngamma"))).unsafeRunSync()

    applyEvent(Paste)

    getContent(bufferId) shouldBe "alpha\nbeta\ngamma\nomega"
    getCursor shouldBe CursorPosition(2, 5)

  behavior of "Cut"

  it should "copy the current line to the clipboard" in new ClipFixture:
    setupBuffer("cut me")

    applyEvent(Cut)

    getClipboard shouldBe Some("cut me")

  it should "remove the current line from the buffer" in new ClipFixture:
    val bufferId = setupBuffer("remove this\nkeep this")
    setCursor(0, 0)

    applyEvent(Cut)

    getContent(bufferId) shouldBe "keep this"

  it should "mark the buffer dirty" in new ClipFixture:
    val bufferId = setupBuffer("to cut")

    applyEvent(Cut)

    getState.buffers(bufferId).isDirty shouldBe true

  it should "cut the active selection instead of the whole line" in new ClipFixture:
    val bufferId = setupBuffer("Hello World Program")
    setSelection(Selection(CursorPosition(0, 6), CursorPosition(0, 11)))

    applyEvent(Cut)

    getClipboard shouldBe Some("World")
    getContent(bufferId) shouldBe "Hello  Program"
    getCursor shouldBe CursorPosition(0, 6)
    getState.buffers(bufferId).selection shouldBe None

  it should "cut a multiline selection and join the remaining text" in new ClipFixture:
    val bufferId = setupBuffer("alpha\nbeta\ngamma")
    setSelection(Selection(CursorPosition(0, 2), CursorPosition(1, 2)))

    applyEvent(Cut)

    getClipboard shouldBe Some("pha\nbe")
    getContent(bufferId) shouldBe "alta\ngamma"
    getCursor shouldBe CursorPosition(0, 2)
    getState.buffers(bufferId).selection shouldBe None

  it should "round-trip: cut then paste restores the line" in new ClipFixture:
    val bufferId = setupBuffer("original")
    setCursor(0, 0)

    applyEvent(Cut)
    getContent(bufferId) shouldBe ""

    applyEvent(Paste)
    getContent(bufferId) shouldBe "original"

  trait ClipFixture:
    val stateManager: StateManager = StateManager
      .apply(LoggerFactory[IO].getLogger(using LoggerName("CopyPasteSpec")))
      .unsafeRunSync()

    private var activePaneId: PaneId  = PaneId(0)
    private var activeBufferId: BufferId = BufferId(0)

    def setupBuffer(content: String): BufferId =
      val bufferId = stateManager.createBuffer(content).unsafeRunSync()
      val state    = stateManager.getCurrentState.unsafeRunSync()
      activePaneId   = state.layout.editorPanes.keys.head
      activeBufferId = bufferId
      stateManager.setBufferForPane(activePaneId, bufferId).unsafeRunSync()
      bufferId

    def setCursor(line: Int, col: Int): Unit =
      stateManager.setCursorPosition(activePaneId, line, col).unsafeRunSync()

    def setSelection(selection: Selection): Unit =
      stateManager.updateState { state =>
        state.copy(
          buffers = state.buffers.updated(
            activeBufferId,
            state.buffers(activeBufferId).copy(
              cursors = List(selection.start),
              selection = Some(selection)
            )
          )
        )
      }.unsafeRunSync()

    def applyEvent(event: Event): Unit =
      stateManager.applyEvent(event).unsafeRunSync()

    def getState: AppState =
      stateManager.getCurrentState.unsafeRunSync()

    def getContent(bufferId: BufferId): String =
      getState.buffers.get(bufferId).map(_.content.collect()).getOrElse("")

    def getClipboard: Option[String] =
      getState.clipboard

    def getCursor: CursorPosition =
      getState.activeCursorPosition.getOrElse(CursorPosition(0, 0))
