package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import com.serenity.command.ClipboardHistoryCommands
import com.serenity.keystroke.events.{Copy, Direction, ModalNavigate, ModalSubmit}
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.{StateManagerTestSupport, setBufferForPane, setCursorPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1962 through a running editor: the Paste from History picker and the entry picked from it. */
class PasteFromHistoryStateManagerSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def editorWith(text: String): (StateManager, BufferId) =
    val editor   = createStateManager("PasteFromHistoryStateManagerSpec")
    val bufferId = editor.createBuffer(text, None).unsafeRunSync()
    editor.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    (editor, bufferId)

  private def pickerLabels(state: AppState): List[String] =
    val pickers = state.runtime.modalStack.map(_.modal).collect { case Modal.ListPicker(picker) => picker } ++
      state.runtime.uiSurfaces.map(_.content).collect {
        case SurfaceContent.ModalWorkflow(Modal.ListPicker(picker)) => picker
      }
    pickers
      .filter(_.title == ClipboardHistoryPicker.Title)
      .flatMap(_.items.toOption.toList.flatMap(_.items.map(_.label)))

  "Paste from History" should "list the copied texts newest first and paste the one picked" in {
    val (editor, bufferId) = editorWith("first\nsecond\nthird")
    editor.setCursorPosition(PaneId(0), 0, 0).unsafeRunSync()
    editor.applyEvent(Copy).unsafeRunSync()
    editor.setCursorPosition(PaneId(0), 1, 0).unsafeRunSync()
    editor.applyEvent(Copy).unsafeRunSync()
    editor.setCursorPosition(PaneId(0), 2, 2).unsafeRunSync()

    editor.executeCommand(ClipboardHistoryCommands.choose).unsafeRunSync()
    pickerLabels(editor.getCurrentState.unsafeRunSync()) shouldBe List("second", "first")

    editor.applyEvent(ModalNavigate(Direction.Down)).unsafeRunSync()
    editor.applyEvent(ModalSubmit).unsafeRunSync()

    val state = awaitState(editor)(_.persisted.buffers(bufferId).document.content.collect() != "first\nsecond\nthird")
      .unsafeRunSync()
    state.persisted.buffers(bufferId).document.content.collect() shouldBe "first\nsecond\nfirst\nthird"
    state.runtime.clipboard shouldBe Some("second")
    pickerLabels(state) shouldBe Nil
  }
