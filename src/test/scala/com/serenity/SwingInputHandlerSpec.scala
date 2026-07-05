package com.serenity

import java.awt.event.{InputEvent, KeyEvent}
import javax.swing.JPanel

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import cats.syntax.parallel.*
import com.serenity.input.{InputRouter, SwingInputHandler}
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SwingInputHandlerSpec extends AnyFlatSpec with Matchers:

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

    program.unsafeRunTimed(2.seconds).shouldBe(defined)
  }

  it should "terminate event streams that subscribe after shutdown has already completed" in {
    val component = new JPanel()
    val router    = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    val handler   = new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))

    val program = handler.shutdown >> handler.eventStream.compile.drain

    program.unsafeRunTimed(2.seconds).shouldBe(defined)
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

    handler.keyStrokeInfoStream.take(1).compile.last.unsafeRunTimed(2.seconds).flatten shouldBe
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

    handler.keyStrokeInfoStream.take(1).compile.last.unsafeRunTimed(2.seconds).flatten shouldBe
      Some(KeyStrokeInfo(InputKey.Character, Some('p'), Set(Modifier.Meta)))
  }
