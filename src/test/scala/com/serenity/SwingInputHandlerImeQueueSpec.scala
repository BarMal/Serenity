package com.serenity

import java.awt.event.{InputMethodEvent, KeyEvent}
import java.awt.font.TextHitInfo
import java.text.AttributedString
import javax.swing.JPanel

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.input.{InputRouter, SwingInputHandler}
import com.serenity.keystroke.events.{Event, ImeCommit, ImeComposing, InsertChar}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.state.models.CompositionClause
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Input-method events travel the same ordered queue as keys (#2008), so a commit can never overtake the key typed
  * before it or be overtaken by the one typed after.
  */
class SwingInputHandlerImeQueueSpec extends AnyFlatSpec with Matchers:

  private def handlerOn(component: JPanel): SwingInputHandler[IO, Event] =
    val router = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))

  private def typed(component: JPanel, char: Char): Unit =
    component.getKeyListeners.head.keyTyped(KeyEvent(component, KeyEvent.KEY_TYPED, 2L, 0, KeyEvent.VK_UNDEFINED, char))

  private def firstEvents(handler: SwingInputHandler[IO, Event], count: Int): Option[List[Event]] =
    handler.eventStream.take(count.toLong).compile.toList.unsafeRunTimed(10.seconds)

  private def inputMethodEvent(panel: JPanel, text: String, committed: Int): InputMethodEvent =
    new InputMethodEvent(
      panel,
      InputMethodEvent.INPUT_METHOD_TEXT_CHANGED,
      1L,
      new AttributedString(text).getIterator,
      committed,
      TextHitInfo.leading(0),
      null
    )

  "SwingInputHandler" should "deliver input-method events in order with the keys around them" in {
    val panel   = new JPanel
    val handler = handlerOn(panel)

    typed(panel, 'a')
    handler.submit(ImeCommit("字"))
    typed(panel, 'b')

    firstEvents(handler, 3) shouldBe Some(List(InsertChar('a'), ImeCommit("字"), InsertChar('b')))
  }

  "submitInputMethodEvent" should "queue the commit and the pre-edit of one AWT event adjacent and in that order" in {
    val panel   = new JPanel
    val handler = handlerOn(panel)

    typed(panel, 'a')
    handler.submitInputMethodEvent(inputMethodEvent(panel, "漢字かな", committed = 2))
    typed(panel, 'b')

    firstEvents(handler, 4) shouldBe Some(
      List(
        InsertChar('a'),
        ImeCommit("漢字"),
        ImeComposing("かな", List(CompositionClause(0, 2, selected = false)), Some(0)),
        InsertChar('b')
      )
    )
  }
