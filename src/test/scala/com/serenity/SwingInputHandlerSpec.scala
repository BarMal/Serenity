package com.serenity

import java.awt.event.{InputEvent, KeyEvent}
import javax.swing.JPanel

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import cats.syntax.parallel.*
import com.serenity.input.{InputRouter, SwingInputHandler}
import com.serenity.keystroke.events.{
  Event,
  InsertChar,
  MouseButton,
  MouseClick,
  MouseDrag,
  MouseMove,
  MousePress,
  MouseRenderMetrics
}
import com.serenity.keystroke.translators.{TextEntryTranslator, Translator}
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SwingInputHandlerSpec extends AnyFlatSpec with Matchers:

  private val StreamObservationTimeout = 10.seconds

  "SwingInputHandler" should "terminate its event stream when shutdown is requested while idle" in {
    val component = new JPanel()
    val router    = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))

    val program = for
      awaiting <- Deferred[IO, Unit]
      _ <- (
        awaiting.complete(()) >> handler.eventStream.compile.drain,
        awaiting.get >> handler.shutdown
      ).parMapN((_, _) => ())
    yield ()

    program.unsafeRunTimed(StreamObservationTimeout).shouldBe(defined)
  }

  it should "terminate event streams that subscribe after shutdown has already completed" in {
    val component = new JPanel()
    val router    = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))

    val program = handler.shutdown >> handler.eventStream.compile.drain

    program.unsafeRunTimed(StreamObservationTimeout).shouldBe(defined)
  }

  it should "preserve callback order across keyboard and mouse input" in {
    val component = new JPanel()
    val router    = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    val key       = component.getKeyListeners.head
    val mouse     = component.getMouseListeners.head
    val motion    = component.getMouseMotionListeners.head

    key.keyTyped(KeyEvent(component, KeyEvent.KEY_TYPED, 1L, 0, KeyEvent.VK_UNDEFINED, 'a'))
    motion.mouseMoved(
      java.awt.event.MouseEvent(component, java.awt.event.MouseEvent.MOUSE_MOVED, 2L, 0, 8, 16, 0, false)
    )
    key.keyTyped(KeyEvent(component, KeyEvent.KEY_TYPED, 3L, 0, KeyEvent.VK_UNDEFINED, 'b'))
    mouse.mouseClicked(
      java.awt.event.MouseEvent(component, java.awt.event.MouseEvent.MOUSE_CLICKED, 4L, 0, 16, 32, 1, false)
    )

    handler.eventStream.take(4).compile.toList.unsafeRunSync() shouldBe List(
      InsertChar('a'),
      MouseMove(1, 1, Some(8), Some(16), shiftDown = false),
      InsertChar('b'),
      MouseClick(
        2,
        2,
        Some(16),
        Some(32),
        clickCount = 1,
        shiftDown = false,
        button = MouseButton.Other,
        renderMetrics = Some(MouseRenderMetrics(CellMetrics(8, 16, 13), CellMetrics(8, 16, 13)))
      )
    )
  }

  it should "coalesce only superseded adjacent mouse moves and drags" in {
    val component = new JPanel()
    val router    = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    val key       = component.getKeyListeners.head
    val mouse     = component.getMouseListeners.head
    val motion    = component.getMouseMotionListeners.head

    motion.mouseMoved(
      java.awt.event.MouseEvent(component, java.awt.event.MouseEvent.MOUSE_MOVED, 1L, 0, 8, 16, 0, false)
    )
    motion.mouseMoved(
      java.awt.event.MouseEvent(component, java.awt.event.MouseEvent.MOUSE_MOVED, 2L, 0, 16, 32, 0, false)
    )
    mouse.mousePressed(
      java.awt.event.MouseEvent(component, java.awt.event.MouseEvent.MOUSE_PRESSED, 3L, 0, 24, 48, 1, false)
    )
    key.keyTyped(KeyEvent(component, KeyEvent.KEY_TYPED, 4L, 0, KeyEvent.VK_UNDEFINED, 'x'))
    motion.mouseDragged(
      java.awt.event.MouseEvent(component, java.awt.event.MouseEvent.MOUSE_DRAGGED, 5L, 0, 32, 64, 0, false)
    )
    motion.mouseDragged(
      java.awt.event.MouseEvent(component, java.awt.event.MouseEvent.MOUSE_DRAGGED, 6L, 0, 40, 80, 0, false)
    )

    handler.eventStream.take(4).compile.toList.unsafeRunSync() shouldBe List(
      MouseMove(2, 2, Some(16), Some(32), shiftDown = false),
      MousePress(3, 3, Some(24), Some(48), shiftDown = false, button = MouseButton.Other),
      InsertChar('x'),
      MouseDrag(5, 5, Some(40), Some(80), shiftDown = false, button = MouseButton.Other)
    )
  }

  it should "enqueue callbacks without waiting for event processing" in {
    val component = new JPanel()
    val program = for
      consumerStarted <- Deferred[IO, Unit]
      releaseConsumer <- Deferred[IO, Unit]
      callbackDone    <- Deferred[IO, Unit]
      router = new InputRouter[IO, Event]:
        override def eventStream(infoStream: fs2.Stream[IO, KeyStrokeInfo]): fs2.Stream[IO, Event] =
          infoStream.evalMap(_ => consumerStarted.complete(()) >> releaseConsumer.get.as(InsertChar('a')))
        override def setActiveTranslator(translator: Translator[Event]): IO[Unit] = IO.unit
        override def getActiveTranslator: IO[Translator[Event]]                   = IO.pure(new TextEntryTranslator)
      handler = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
      key     = component.getKeyListeners.head
      motion  = component.getMouseMotionListeners.head
      eventFiber <- handler.eventStream.compile.drain.start
      _ = key.keyTyped(KeyEvent(component, KeyEvent.KEY_TYPED, 1L, 0, KeyEvent.VK_UNDEFINED, 'a'))
      _ <- consumerStarted.get
      _ <- IO.blocking(
        motion.mouseMoved(
          java.awt.event.MouseEvent(component, java.awt.event.MouseEvent.MOUSE_MOVED, 2L, 0, 8, 16, 0, false)
        )
      ) >> callbackDone.complete(())
      callbackObserved <- callbackDone.get.as(true).timeoutTo(1.second, IO.pure(false))
      _                <- releaseConsumer.complete(())
      _                <- handler.shutdown
      _                <- eventFiber.joinWithNever
    yield callbackObserved

    program.unsafeRunSync() shouldBe true
  }

  it should "emit macOS printable typed characters without command modifiers" in {
    val component = new JPanel()
    val router    = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    val event = KeyEvent(
      component,
      KeyEvent.KEY_TYPED,
      System.currentTimeMillis(),
      InputEvent.SHIFT_DOWN_MASK,
      KeyEvent.VK_UNDEFINED,
      '£'
    )

    component.getKeyListeners.head.keyTyped(event)

    handler.keyStrokeInfoStream.take(1).compile.last.unsafeRunTimed(StreamObservationTimeout).flatten shouldBe
      Some(KeyStrokeInfo(InputKey.Character, Some('£'), Set.empty))
  }

  it should "emit command-modified pressed letters as meta character strokes" in {
    val component = new JPanel()
    val router    = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    val event = KeyEvent(
      component,
      KeyEvent.KEY_PRESSED,
      System.currentTimeMillis(),
      InputEvent.META_DOWN_MASK,
      KeyEvent.VK_P,
      'P'
    )

    component.getKeyListeners.head.keyPressed(event)

    handler.keyStrokeInfoStream.take(1).compile.last.unsafeRunTimed(StreamObservationTimeout).flatten shouldBe
      Some(KeyStrokeInfo(InputKey.Character, Some('p'), Set(Modifier.Meta)))
  }

  it should "emit a double-tap stroke only after the modifier is released" in {
    val component = new JPanel()
    val router    = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    val listener  = component.getKeyListeners.head
    val now       = System.currentTimeMillis()

    listener.keyPressed(KeyEvent(component, KeyEvent.KEY_PRESSED, now, 0, KeyEvent.VK_CONTROL, '\u0000'))
    listener.keyReleased(KeyEvent(component, KeyEvent.KEY_RELEASED, now, 0, KeyEvent.VK_CONTROL, '\u0000'))
    listener.keyPressed(
      KeyEvent(component, KeyEvent.KEY_PRESSED, now + 200, InputEvent.CTRL_DOWN_MASK, KeyEvent.VK_CONTROL, '\u0000')
    )

    handler.keyStrokeInfoStream.take(1).compile.last.unsafeRunTimed(StreamObservationTimeout).flatten shouldBe
      Some(KeyStrokeInfo(InputKey.Ctrl, None, Set.empty))
  }

  it should "ignore held-modifier auto-repeat presses" in {
    val component = new JPanel()
    val router    = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    val listener  = component.getKeyListeners.head
    val now       = System.currentTimeMillis()

    listener.keyPressed(KeyEvent(component, KeyEvent.KEY_PRESSED, now, 0, KeyEvent.VK_CONTROL, '\u0000'))
    listener.keyPressed(
      KeyEvent(component, KeyEvent.KEY_PRESSED, now + 100, InputEvent.CTRL_DOWN_MASK, KeyEvent.VK_CONTROL, '\u0000')
    )

    handler.keyStrokeInfoStream.take(1).compile.last.unsafeRunTimed(250.millis) shouldBe None
  }

  it should "ignore modifier taps outside the 200 millisecond double-tap window" in {
    val component = new JPanel()
    val router    = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))
    val listener  = component.getKeyListeners.head
    val now       = System.currentTimeMillis()

    listener.keyPressed(KeyEvent(component, KeyEvent.KEY_PRESSED, now, 0, KeyEvent.VK_CONTROL, '\u0000'))
    listener.keyReleased(KeyEvent(component, KeyEvent.KEY_RELEASED, now, 0, KeyEvent.VK_CONTROL, '\u0000'))
    listener.keyPressed(
      KeyEvent(component, KeyEvent.KEY_PRESSED, now + 201, InputEvent.CTRL_DOWN_MASK, KeyEvent.VK_CONTROL, '\u0000')
    )

    handler.keyStrokeInfoStream.take(1).compile.last.unsafeRunTimed(250.millis) shouldBe None
  }
