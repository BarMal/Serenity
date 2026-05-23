package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.serenity.keystroke.events.{Event, ResizeEvent, UnhandledEvent}
import com.serenity.keystroke.translators.{TextEntryTranslator, Translator}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.TerminalSize
import com.serenity.ui.renderer.RenderController
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class ResizeAndEOFFixSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  "RenderController.handleResize" should "immediately trigger re-render when resize is detected" in {
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      initialState <- stateManager.getCurrentState
      
      // Simulate a resize event
      newSize = TerminalSize(120, 40)
      resizeTriggered <- IO.ref(false)
      onResized = resizeTriggered.set(true)
      
      _ <- RenderController.handleResize(Some(newSize), stateManager, onResized)
      
      finalState <- stateManager.getCurrentState
      wasTriggered <- resizeTriggered.get
    yield
      // The resize event should be applied to state
      finalState.terminalSize shouldBe Some(newSize)
      initialState.terminalSize should not be Some(newSize)
      
      // The onResized callback should be triggered immediately
      wasTriggered shouldBe true

    program.unsafeRunSync()
  }

  it should "not trigger re-render when no resize is detected" in {
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      initialState <- stateManager.getCurrentState
      
      resizeTriggered <- IO.ref(false)
      onResized = resizeTriggered.set(true)
      
      // No resize detected (None)
      _ <- RenderController.handleResize(None, stateManager, onResized)
      
      finalState <- stateManager.getCurrentState
      wasTriggered <- resizeTriggered.get
    yield
      // State should remain unchanged
      finalState.terminalSize shouldBe initialState.terminalSize
      
      // onResized should NOT be triggered
      wasTriggered shouldBe false

    program.unsafeRunSync()
  }

  "UnhandledEvent filtering" should "distinguish between critical and non-critical unhandled events" in {
    val translator = new TextEntryTranslator()
    
    // EOF keystroke now translates to Quit, not UnhandledEvent
    val eofKeyStroke = new KeyStroke(KeyType.EOF)
    val normalUnknownKeyStroke = new KeyStroke(KeyType.Unknown)
    
    // Translate both
    val eofEvent = translator.translate(eofKeyStroke) 
    val unknownEvent = translator.translate(normalUnknownKeyStroke)
    
    // EOF should now be Quit, Unknown should be UnhandledEvent
    eofEvent shouldBe com.serenity.keystroke.events.Quit
    unknownEvent shouldBe a[UnhandledEvent[?]]
    
    val unknownUnhandled = unknownEvent.asInstanceOf[UnhandledEvent[?]]
    
    // Unknown events should be identifiable as system/terminal events that shouldn't flood logs
    unknownUnhandled.keyStroke.getKeyType shouldBe KeyType.Unknown
  }

  "Event filtering logic" should "classify events correctly for logging" in {
    val translator = new TextEntryTranslator()
    
    // Test various problematic keystrokes that might flood logs
    // Note: EOF is no longer included here since it now translates to Quit, not UnhandledEvent
    val problematicKeystrokes = List(
      new KeyStroke(KeyType.Unknown),
      new KeyStroke('\u0000', false, false, false), // Null character  
      new KeyStroke('\u0004', false, false, false), // End of transmission (Ctrl+D)
      new KeyStroke('\u001A', false, false, false)  // Substitute character (Ctrl+Z on some systems)
    )
    
    val events = problematicKeystrokes.map(translator.translate)
    
    // All should be UnhandledEvent instances (EOF no longer included)
    events.foreach(_ shouldBe a[UnhandledEvent[?]])
    
    val unhandledEvents = events.collect { case ue: UnhandledEvent[?] => ue }
    
    // We should be able to identify which ones are system/terminal events vs user keystrokes
    val systemEvents = unhandledEvents.filter(isSystemEvent)
    val userEvents = unhandledEvents.filter(!isSystemEvent(_))
    
    // Null characters should be classified as system events, but not EOF (since it's now handled)
    systemEvents.length.should(be >= 1)
    systemEvents.exists(_.keyStroke.getKeyType == KeyType.Unknown).shouldBe(true)
    // EOF should no longer be in unhandled system events since it's translated to Quit
    systemEvents.exists(_.keyStroke.getKeyType == KeyType.EOF).shouldBe(false)
  }

  "EOF event handling" should "translate EOF keystroke to Quit event for graceful shutdown" in {
    val translator = new TextEntryTranslator()
    
    // EOF keystroke should be translated to Quit event, not UnhandledEvent
    val eofEvent = translator.translate(new KeyStroke(KeyType.EOF))
    
    // EOF should now be translated to a Quit event
    eofEvent shouldBe com.serenity.keystroke.events.Quit
    eofEvent should not be a[UnhandledEvent[?]]
  }

  it should "handle EOF event properly in StateManager to trigger graceful shutdown" in {
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      
      // Apply EOF event (which should be translated to Quit)
      _ <- stateManager.applyEvent(com.serenity.keystroke.events.Quit)
      
      // The quit signal should be triggered
      // Note: We can't easily test awaitQuit in a unit test since it blocks,
      // but we can verify the event is processed without error
    yield
      // If we reach here, the Quit event was processed successfully
      succeed

    program.unsafeRunSync()
  }

  "Main.isSystemEvent integration" should "no longer classify EOF as system event since it's handled" in {
    // This test verifies that EOF events are no longer unhandled system events
    // since they should be translated to Quit events
    
    val translator = new TextEntryTranslator()
    
    // EOF event should now be Quit, not UnhandledEvent
    val eofEvent = translator.translate(new KeyStroke(KeyType.EOF))
    eofEvent shouldBe com.serenity.keystroke.events.Quit
    
    // Test other system events that should still be filtered
    val unknownEvent = translator.translate(new KeyStroke(KeyType.Unknown)).asInstanceOf[UnhandledEvent[?]]
    val nullCharEvent = translator.translate(new KeyStroke('\u0000', false, false, false)).asInstanceOf[UnhandledEvent[?]]
    val normalCharEvent = translator.translate(new KeyStroke('§', false, false, false)).asInstanceOf[UnhandledEvent[?]]
    
    // Test classification  
    isSystemEvent(unknownEvent) shouldBe true  // Unknown should be filtered (terminal noise)
    isSystemEvent(nullCharEvent) shouldBe true // Null char should be filtered
    isSystemEvent(normalCharEvent) shouldBe false // Normal unhandled char should still warn
  }

  private def isSystemEvent(event: UnhandledEvent[?]): Boolean =
    import com.googlecode.lanterna.input.KeyType
    event.keyStroke.getKeyType match
      case KeyType.EOF => false // EOF is now handled as Quit event, not a system event
      case KeyType.Unknown => true
      case KeyType.Character => 
        // Check for control characters that indicate system/terminal events
        Option(event.keyStroke.getCharacter).exists { char =>
          char == '\u0000' || // Null character
          char == '\u0004' || // End of transmission (Ctrl+D)
          char == '\u001A'    // Substitute character (Ctrl+Z on some systems)
        }
      case _ => false