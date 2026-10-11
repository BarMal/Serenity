package com.serenity

import java.awt.event.{InputEvent, KeyEvent}
import javax.swing.JPanel

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, HotkeyConfig}
import com.serenity.input.{FocusedInputTranslator, InputRouter, SwingInputHandler}
import com.serenity.keystroke.events.{Event, RunCommand}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId, EditorPane, Focus, PaneId}
import com.serenity.ui.layout.{CellMetrics, Layout}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The comment-toggle chords as AWT delivers them: Ctrl+/ and Cmd+/ carry a punctuation key, and Shift+Alt+A is an Alt
  * chord that would otherwise type a letter.
  */
class SwingInputHandlerCommentKeysSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val StreamObservationTimeout = 10.seconds

  private val editorPaneId = PaneId(0)

  private def editorState(osName: String): AppState =
    val initial = AppState.initial
    initial.copy(persisted =
      initial.persisted.copy(
        config = AppConfig.default.withHotkeyConfig(HotkeyConfig.forOs(osName)),
        layout = Layout(
          editorPanes = Map(editorPaneId -> EditorPane.withBuffer(editorPaneId, BufferId(0))),
          activeEditorPaneId = Some(editorPaneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(editorPaneId))
        ),
        focus = Focus.EditorPane(editorPaneId)
      )
    )

  private def eventFor(osName: String, keyCode: Int, modifiers: Int): Option[Event] =
    val component = new JPanel()
    val router    = InputRouter.create[IO, Event](FocusedInputTranslator.forState(editorState(osName))).unsafeRunSync()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    component.getKeyListeners.head.keyPressed(
      KeyEvent(component, KeyEvent.KEY_PRESSED, System.currentTimeMillis(), modifiers, keyCode, KeyEvent.CHAR_UNDEFINED)
    )
    handler.eventStream.take(1).compile.last.unsafeRunTimed(StreamObservationTimeout).flatten

  "SwingInputHandler" should "decode Ctrl+/ to the line-comment command" in {
    eventFor("Linux", KeyEvent.VK_SLASH, InputEvent.CTRL_DOWN_MASK) shouldBe Some(RunCommand("toggle-line-comment"))
  }

  it should "decode Cmd+/ to the line-comment command" in {
    eventFor("Mac OS X", KeyEvent.VK_SLASH, InputEvent.META_DOWN_MASK) shouldBe Some(
      RunCommand("toggle-line-comment")
    )
  }

  it should "decode Shift+Alt+A to the block-comment command" in {
    eventFor("Linux", KeyEvent.VK_A, InputEvent.SHIFT_DOWN_MASK | InputEvent.ALT_DOWN_MASK) shouldBe Some(
      RunCommand("toggle-block-comment")
    )
  }
