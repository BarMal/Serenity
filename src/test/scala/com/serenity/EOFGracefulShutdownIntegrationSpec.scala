package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.input.InputRouter
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class EOFGracefulShutdownIntegrationSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  "EOF Integration Test" should "demonstrate complete flow from EOF keystroke to graceful shutdown" in {
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      inputRouter  <- InputRouter.create[IO, Event](new TextEntryTranslator)

      eofInfo = KeyStrokeInfo(InputKey.EOF, None, Set.empty)

      translator <- inputRouter.getActiveTranslator
      event = translator.translate(eofInfo)

      _ = event shouldBe com.serenity.keystroke.events.Quit

      _ <- stateManager.applyEvent(event)
    yield succeed

    program.unsafeRunSync()
  }

  "EOF vs Ctrl+Q" should "both trigger the same graceful shutdown behavior" in {
    val program = for
      logger        <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager1 <- StateManager.apply(logger)
      stateManager2 <- StateManager.apply(logger)
      translator = new TextEntryTranslator()

      eofEvent   = translator.translate(KeyStrokeInfo(InputKey.EOF, None, Set.empty))
      ctrlQEvent = translator.translate(KeyStrokeInfo(InputKey.Character, Some('q'), Set(Modifier.Ctrl)))

      _ = eofEvent shouldBe com.serenity.keystroke.events.Quit
      _ = ctrlQEvent shouldBe com.serenity.keystroke.events.Quit

      _ <- stateManager1.applyEvent(eofEvent)
      _ <- stateManager2.applyEvent(ctrlQEvent)
    yield succeed

    program.unsafeRunSync()
  }
