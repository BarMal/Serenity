package com.serenity

import javax.swing.JPanel

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import cats.syntax.parallel.*
import com.serenity.input.{InputRouter, SwingInputHandler}
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.TextEntryTranslator
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
