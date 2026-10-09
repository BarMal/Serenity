package com.serenity

import java.awt.event.{InputEvent, KeyEvent}
import javax.swing.JPanel

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.input.{InputRouter, SwingInputHandler}
import com.serenity.keystroke.events.{
  CursorPeekModifierPressed,
  CursorPeekModifierReleased,
  CursorPeekOtherKeyPressed,
  Event,
  InsertChar
}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The raw cursor-peek key events the handler emits, and that it emits none while the feature is off (#1845). */
class SwingInputHandlerCursorPeekSpec extends AnyFlatSpec with Matchers:

  private val StreamObservationTimeout = 10.seconds

  "SwingInputHandler" should "emit a raw CursorPeekModifierPressed for a bare modifier press, regardless of any pending double-tap hotkey" in {
    val component = new JPanel()
    val router    = cursorPeekRouter()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    val listener  = component.getKeyListeners.head
    val now       = System.currentTimeMillis()

    listener.keyPressed(KeyEvent(component, KeyEvent.KEY_PRESSED, now, 0, KeyEvent.VK_META, '\u0000'))

    handler.eventStream.take(1).compile.last.unsafeRunTimed(StreamObservationTimeout).flatten shouldBe
      Some(CursorPeekModifierPressed(Modifier.Meta, now))
  }

  it should "emit a raw CursorPeekModifierReleased for a bare modifier release" in {
    val component = new JPanel()
    val router    = cursorPeekRouter()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    val listener  = component.getKeyListeners.head
    val now       = System.currentTimeMillis()

    listener.keyPressed(KeyEvent(component, KeyEvent.KEY_PRESSED, now, 0, KeyEvent.VK_META, '\u0000'))
    listener.keyReleased(KeyEvent(component, KeyEvent.KEY_RELEASED, now + 5, 0, KeyEvent.VK_META, '\u0000'))

    handler.eventStream.take(2).compile.toList.unsafeRunTimed(StreamObservationTimeout) shouldBe Some(
      List(CursorPeekModifierPressed(Modifier.Meta, now), CursorPeekModifierReleased(Modifier.Meta, now + 5))
    )
  }

  it should "emit raw modifier press/release events for any modifier, not only the cursor-peek prototype's own" in {
    val component = new JPanel()
    val router    = cursorPeekRouter()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    val listener  = component.getKeyListeners.head
    val now       = System.currentTimeMillis()

    listener.keyPressed(KeyEvent(component, KeyEvent.KEY_PRESSED, now, 0, KeyEvent.VK_CONTROL, '\u0000'))

    handler.eventStream.take(1).compile.last.unsafeRunTimed(StreamObservationTimeout).flatten shouldBe
      Some(CursorPeekModifierPressed(Modifier.Ctrl, now))
  }

  it should "emit a raw CursorPeekOtherKeyPressed for a non-modifier key press" in {
    val component = new JPanel()
    val router    = cursorPeekRouter()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    val listener  = component.getKeyListeners.head
    val now       = System.currentTimeMillis()

    listener.keyPressed(KeyEvent(component, KeyEvent.KEY_PRESSED, now, 0, KeyEvent.VK_A, 'a'))

    handler.eventStream.take(1).compile.last.unsafeRunTimed(StreamObservationTimeout).flatten shouldBe
      Some(CursorPeekOtherKeyPressed)
  }

  it should "emit no cursor-peek events while cursor peek is disabled" in {
    val component = new JPanel()
    val router    = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    val listener  = component.getKeyListeners.head
    val now       = System.currentTimeMillis()

    listener.keyPressed(KeyEvent(component, KeyEvent.KEY_PRESSED, now, 0, KeyEvent.VK_SHIFT, '\u0000'))
    listener.keyPressed(
      KeyEvent(component, KeyEvent.KEY_PRESSED, now + 1, InputEvent.SHIFT_DOWN_MASK, KeyEvent.VK_A, 'A')
    )
    listener.keyTyped(KeyEvent(component, KeyEvent.KEY_TYPED, now + 2, 0, KeyEvent.VK_UNDEFINED, 'A'))
    listener.keyReleased(KeyEvent(component, KeyEvent.KEY_RELEASED, now + 3, 0, KeyEvent.VK_SHIFT, '\u0000'))
    listener.keyTyped(KeyEvent(component, KeyEvent.KEY_TYPED, now + 4, 0, KeyEvent.VK_UNDEFINED, 'b'))

    handler.eventStream.take(2).compile.toList.unsafeRunTimed(StreamObservationTimeout) shouldBe
      Some(List(InsertChar('A'), InsertChar('b')))
  }

  it should "not surface the new raw cursor-peek events on keyStrokeInfoStream" in {
    val component = new JPanel()
    val router    = cursorPeekRouter()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    val listener  = component.getKeyListeners.head
    val now       = System.currentTimeMillis()

    listener.keyPressed(KeyEvent(component, KeyEvent.KEY_PRESSED, now, 0, KeyEvent.VK_META, '\u0000'))
    listener.keyTyped(KeyEvent(component, KeyEvent.KEY_TYPED, now + 1, 0, KeyEvent.VK_UNDEFINED, 'z'))

    handler.keyStrokeInfoStream.take(1).compile.last.unsafeRunTimed(StreamObservationTimeout).flatten shouldBe
      Some(KeyStrokeInfo(InputKey.Character, Some('z'), Set.empty))
  }

  private def cursorPeekRouter(): InputRouter[IO, Event] =
    InputRouter
      .create[IO, Event](new TextEntryTranslator)
      .flatTap(_.setCursorPeekEnabled(true))
      .unsafeRunSync()
