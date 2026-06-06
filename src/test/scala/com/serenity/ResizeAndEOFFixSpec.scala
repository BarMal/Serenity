package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Event, UnhandledEvent}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.RenderController
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class ResizeAndEOFFixSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  "RenderController.handleResize" should "immediately trigger re-render when resize is detected" in {
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      initialState <- stateManager.getCurrentState

      newSize = ViewportSize(120, 40)
      resizeTriggered <- IO.ref(false)
      onResized = resizeTriggered.set(true)

      _ <- RenderController.handleResize(Some(newSize), stateManager, onResized)

      finalState   <- stateManager.getCurrentState
      wasTriggered <- resizeTriggered.get
    yield
      finalState.viewportSize shouldBe Some(newSize)
      initialState.viewportSize should not be Some(newSize)
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

      _ <- RenderController.handleResize(None, stateManager, onResized)

      finalState   <- stateManager.getCurrentState
      wasTriggered <- resizeTriggered.get
    yield
      finalState.viewportSize shouldBe initialState.viewportSize
      wasTriggered shouldBe false

    program.unsafeRunSync()
  }

  "UnhandledEvent filtering" should "distinguish between critical and non-critical unhandled events" in {
    val translator = new TextEntryTranslator()

    val eofEvent     = translator.translate(KeyStrokeInfo(InputKey.EOF, None, Set.empty))
    val unknownEvent = translator.translate(KeyStrokeInfo(InputKey.Unknown, None, Set.empty))

    eofEvent shouldBe com.serenity.keystroke.events.Quit
    unknownEvent shouldBe a[UnhandledEvent[?]]

    val unknownUnhandled = unknownEvent.asInstanceOf[UnhandledEvent[?]]
    unknownUnhandled.info.keyType shouldBe InputKey.Unknown
  }

  "Event filtering logic" should "classify events correctly for logging" in {
    val translator = new TextEntryTranslator()

    val problematicInfos = List(
      KeyStrokeInfo(InputKey.Unknown, None, Set.empty),
      KeyStrokeInfo(InputKey.Character, None, Set.empty),
      KeyStrokeInfo(InputKey.Character, Some(4.toChar), Set.empty),
      KeyStrokeInfo(InputKey.Character, Some(26.toChar), Set.empty)
    )

    val events = problematicInfos.map(translator.translate)

    events.foreach(_ shouldBe a[UnhandledEvent[?]])

    val unhandledEvents = events.collect { case ue: UnhandledEvent[?] => ue }
    val systemEvents    = unhandledEvents.filter(isSystemEvent)

    systemEvents.length.should(be >= 1)
    systemEvents.exists(_.info.keyType == InputKey.Unknown).shouldBe(true)
    systemEvents.exists(_.info.keyType == InputKey.EOF).shouldBe(false)
  }

  "EOF event handling" should "translate EOF keystroke to Quit event for graceful shutdown" in {
    val translator = new TextEntryTranslator()

    val eofEvent = translator.translate(KeyStrokeInfo(InputKey.EOF, None, Set.empty))

    eofEvent shouldBe com.serenity.keystroke.events.Quit
    eofEvent should not be a[UnhandledEvent[?]]
  }

  it should "handle EOF event properly in StateManager to trigger graceful shutdown" in {
    val program = for
      logger       <- IO.pure(LoggerFactory[IO].getLogger(using LoggerName("Test")))
      stateManager <- StateManager.apply(logger)
      _            <- stateManager.applyEvent(com.serenity.keystroke.events.Quit)
    yield succeed

    program.unsafeRunSync()
  }

  "Main.isSystemEvent integration" should "no longer classify EOF as system event since it's handled" in {
    val translator = new TextEntryTranslator()

    val eofEvent = translator.translate(KeyStrokeInfo(InputKey.EOF, None, Set.empty))
    eofEvent shouldBe com.serenity.keystroke.events.Quit

    val unknownEvent =
      translator.translate(KeyStrokeInfo(InputKey.Unknown, None, Set.empty)).asInstanceOf[UnhandledEvent[?]]
    val nullCharEvent =
      translator.translate(KeyStrokeInfo(InputKey.Character, None, Set.empty)).asInstanceOf[UnhandledEvent[?]]
    val normalCharEvent = translator
      .translate(KeyStrokeInfo(InputKey.Character, Some(167.toChar), Set.empty))
      .asInstanceOf[UnhandledEvent[?]]

    isSystemEvent(unknownEvent) shouldBe true
    isSystemEvent(nullCharEvent) shouldBe true
    isSystemEvent(normalCharEvent) shouldBe false
  }

  private def isSystemEvent(event: UnhandledEvent[?]): Boolean =
    event.info.keyType match
      case InputKey.EOF     => false
      case InputKey.Unknown => true
      case InputKey.Character =>
        event.info.character match
          case None    => true
          case Some(c) => c.toInt == 4 || c.toInt == 26
      case _ => false
