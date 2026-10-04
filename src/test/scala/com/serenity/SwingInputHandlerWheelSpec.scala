package com.serenity

import java.awt.event.{InputEvent, KeyEvent, MouseEvent, MouseWheelEvent}
import javax.swing.JPanel

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.input.{InputRouter, SwingInputHandler}
import com.serenity.keystroke.events.{Event, InsertChar, ScrollDown, ScrollLeft, ScrollRight}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Trackpad scrolling (issue #1796): high-resolution trackpads report a gesture as many wheel events whose integer
  * rotation is 0 and whose precise rotation is fractional; those must add up to whole notches rather than be dropped.
  */
class SwingInputHandlerWheelSpec extends AnyFlatSpec with Matchers:

  private val StreamObservationTimeout = 10.seconds
  private val ShiftHeld                = InputEvent.SHIFT_DOWN_MASK

  private def handlerOn(component: JPanel): SwingInputHandler[IO, Event] =
    val router = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))

  private def scrollWheel(
      component: JPanel,
      modifiers: Int,
      scrollType: Int,
      wheelRotation: Int,
      preciseRotation: Double
  ): Unit =
    component.getMouseWheelListeners.head.mouseWheelMoved(
      new MouseWheelEvent(
        component,
        MouseEvent.MOUSE_WHEEL,
        1L,
        modifiers,
        0,
        0,
        0,
        0,
        0,
        false,
        scrollType,
        1,
        wheelRotation,
        preciseRotation
      )
    )

  private def typeMarker(component: JPanel): Unit =
    component.getKeyListeners.head.keyTyped(KeyEvent(component, KeyEvent.KEY_TYPED, 2L, 0, KeyEvent.VK_UNDEFINED, 'a'))

  private def firstEvents(handler: SwingInputHandler[IO, Event], count: Int): Option[List[Event]] =
    handler.eventStream.take(count.toLong).compile.toList.unsafeRunTimed(StreamObservationTimeout)

  "SwingInputHandler" should "scroll one notch once fractional trackpad deltas add up to a whole notch" in {
    val component = new JPanel()
    val handler   = handlerOn(component)

    Seq.fill(4)(0.25).foreach(delta => scrollWheel(component, 0, MouseWheelEvent.WHEEL_UNIT_SCROLL, 0, delta))
    typeMarker(component)

    firstEvents(handler, 2) shouldBe Some(List(ScrollDown(3), InsertChar('a')))
  }

  it should "add up shift-held fractional trackpad deltas into a horizontal notch" in {
    val component = new JPanel()
    val handler   = handlerOn(component)

    Seq.fill(2)(0.5).foreach(delta => scrollWheel(component, ShiftHeld, MouseWheelEvent.WHEEL_UNIT_SCROLL, 0, delta))
    typeMarker(component)

    firstEvents(handler, 2) shouldBe Some(List(ScrollRight(3), InsertChar('a')))
  }

  it should "scroll a viewport's worth of lines per notch of a block scroll" in {
    val component = new JPanel()
    component.setSize(800, 480)
    val handler = handlerOn(component)

    scrollWheel(component, 0, MouseWheelEvent.WHEEL_BLOCK_SCROLL, 1, 1.0)

    firstEvents(handler, 1) shouldBe Some(List(ScrollDown(30)))
  }

  it should "scroll a viewport's worth of columns per notch of a shift-held block scroll" in {
    val component = new JPanel()
    component.setSize(800, 480)
    val handler = handlerOn(component)

    scrollWheel(component, ShiftHeld, MouseWheelEvent.WHEEL_BLOCK_SCROLL, -1, -1.0)

    firstEvents(handler, 1) shouldBe Some(List(ScrollLeft(100)))
  }
