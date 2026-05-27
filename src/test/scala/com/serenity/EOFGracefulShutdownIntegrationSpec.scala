package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.googlecode.lanterna.input.KeyType
import com.googlecode.lanterna.input.KeyStroke
import com.serenity.input.InputRouter
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class EOFGracefulShutdownIntegrationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  "EOF Integration Test" should "demonstrate complete flow from EOF keystroke to graceful shutdown" in {
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      inputRouter  <- InputRouter.create[IO, Event](new TextEntryTranslator)
      
      // Simulate EOF keystroke being received (like when terminal closes)
      eofKeyStroke = new KeyStroke(KeyType.EOF)
      
      // Translate keystroke to event (this is what InputRouter does)
      translator <- inputRouter.getActiveTranslator
      event = translator.translate(eofKeyStroke)
      
      // Verify EOF translates to Quit
      _ = event shouldBe com.serenity.keystroke.events.Quit
      
      // Apply the event to state manager (this is what the main loop does)
      _ <- stateManager.applyEvent(event)
      
      // If we reach here without error, the graceful shutdown was initiated successfully
      // In the real application, this would trigger awaitQuit to unblock and exit cleanly
    yield
      succeed

    program.unsafeRunSync()
  }

  "EOF vs Ctrl+Q" should "both trigger the same graceful shutdown behavior" in {
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager1 <- StateManager.apply(logger)
      stateManager2 <- StateManager.apply(logger)
      translator = new TextEntryTranslator()
      
      // Test EOF
      eofEvent = translator.translate(new KeyStroke(KeyType.EOF))
      
      // Test Ctrl+Q  
      ctrlQEvent = translator.translate(new KeyStroke('q', true, false, false))
      
      // Both should produce Quit event
      _ = eofEvent shouldBe com.serenity.keystroke.events.Quit
      _ = ctrlQEvent shouldBe com.serenity.keystroke.events.Quit
      
      // Both should be handled the same way by StateManager
      _ <- stateManager1.applyEvent(eofEvent)
      _ <- stateManager2.applyEvent(ctrlQEvent)
      
      // Both trigger graceful shutdown - no exceptions means success
    yield
      // Both EOF and Ctrl+Q now provide the same graceful shutdown behavior
      succeed

    program.unsafeRunSync()
  }
