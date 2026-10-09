package com.serenity

import java.awt.event.{InputEvent, KeyEvent}
import javax.swing.JPanel

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, HotkeyConfig}
import com.serenity.input.{FocusedInputTranslator, InputRouter, SwingInputHandler}
import com.serenity.keystroke.events.{
  DeleteBackward,
  DeleteForward,
  DeleteToLineEnd,
  DeleteToLineStart,
  DeleteWordBackward,
  DeleteWordForward,
  Event
}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId, EditorPane, Focus, PaneId}
import com.serenity.ui.layout.{CellMetrics, Layout}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The macOS delete keys as AWT delivers them, decoded through the editor keymap to editor events. */
class SwingInputHandlerDeleteKeysSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val StreamObservationTimeout = 10.seconds

  private val editorPaneId = PaneId(0)

  private val macEditorState =
    val initial = AppState.initial
    initial.copy(persisted =
      initial.persisted.copy(
        config = AppConfig.default.withHotkeyConfig(HotkeyConfig.forOs("Mac OS X")),
        layout = Layout(
          editorPanes = Map(editorPaneId -> EditorPane.withBuffer(editorPaneId, BufferId(0))),
          activeEditorPaneId = Some(editorPaneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(editorPaneId))
        ),
        focus = Focus.EditorPane(editorPaneId)
      )
    )

  private def editorEventFor(keyCode: Int, modifiers: Int): Option[Event] =
    val component = new JPanel()
    val router    = InputRouter.create[IO, Event](FocusedInputTranslator.forState(macEditorState)).unsafeRunSync()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    component.getKeyListeners.head.keyPressed(
      KeyEvent(component, KeyEvent.KEY_PRESSED, System.currentTimeMillis(), modifiers, keyCode, KeyEvent.CHAR_UNDEFINED)
    )
    handler.eventStream.take(1).compile.last.unsafeRunTimed(StreamObservationTimeout).flatten

  "SwingInputHandler Backspace" should "decode to DeleteBackward with no modifiers" in {
    editorEventFor(KeyEvent.VK_BACK_SPACE, 0) shouldBe Some(DeleteBackward)
  }

  it should "decode to DeleteBackward with Shift still held after a capital" in {
    editorEventFor(KeyEvent.VK_BACK_SPACE, InputEvent.SHIFT_DOWN_MASK) shouldBe Some(DeleteBackward)
  }

  it should "decode Cmd+Backspace to DeleteToLineStart" in {
    editorEventFor(KeyEvent.VK_BACK_SPACE, InputEvent.META_DOWN_MASK) shouldBe Some(DeleteToLineStart)
  }

  it should "decode Option+Backspace to DeleteWordBackward" in {
    editorEventFor(KeyEvent.VK_BACK_SPACE, InputEvent.ALT_DOWN_MASK) shouldBe Some(DeleteWordBackward)
  }

  "SwingInputHandler Delete" should "decode to DeleteForward with no modifiers" in {
    editorEventFor(KeyEvent.VK_DELETE, 0) shouldBe Some(DeleteForward)
  }

  it should "decode to DeleteForward with Shift held" in {
    editorEventFor(KeyEvent.VK_DELETE, InputEvent.SHIFT_DOWN_MASK) shouldBe Some(DeleteForward)
  }

  it should "decode Cmd+Delete to DeleteToLineEnd" in {
    editorEventFor(KeyEvent.VK_DELETE, InputEvent.META_DOWN_MASK) shouldBe Some(DeleteToLineEnd)
  }

  it should "decode Option+Delete to DeleteWordForward" in {
    editorEventFor(KeyEvent.VK_DELETE, InputEvent.ALT_DOWN_MASK) shouldBe Some(DeleteWordForward)
  }
