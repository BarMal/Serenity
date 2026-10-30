package com.serenity

import javax.swing.JPanel

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.diagnostics.FrameTimings
import com.serenity.input.{InputRouter, SwingInputHandler}
import com.serenity.keystroke.events.{Event, InsertChar}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The start-up probe (#2420) types one character without a window to type it into; it must arrive as a key would. */
class SwingInputHandlerProbeSpec extends AnyFlatSpec with Matchers:

  private def handlerOn(component: JPanel, timings: FrameTimings): SwingInputHandler[IO, Event] =
    val router  = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    val metrics = () => CellMetrics(8, 16, 13)
    new SwingInputHandler[IO, Event](component, router, metrics, metrics, frameTimings = timings)

  "SwingInputHandler.submitTypedChar" should "queue the character as the key listener would" in {
    val handler = handlerOn(new JPanel(), FrameTimings())

    handler.submitTypedChar('a')

    handler.eventStream.take(1L).compile.toList.unsafeRunTimed(10.seconds) shouldBe Some(List(InsertChar('a')))
  }

  it should "be followed by the latency trace as a received key" in {
    val timings = FrameTimings()
    timings.keyLatency.setEnabled(true)
    val handler = handlerOn(new JPanel(), timings)

    handler.submitTypedChar('a')

    timings.keyLatency.pendingKeys.size shouldBe 1
  }
