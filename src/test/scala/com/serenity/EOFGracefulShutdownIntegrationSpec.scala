package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.googlecode.lanterna.input.KeyType
import com.googlecode.lanterna.input.KeyStroke
import com.serenity.keystroke.KeyStrokeInfo
import com.googlecode.lanterna.screen.Screen
import com.googlecode.lanterna.terminal.Terminal
import com.serenity.input.{InputHandlerImpl, InputRouter, ScreenInputHandler}
import com.serenity.keystroke.events.Event
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

import java.lang.reflect.{InvocationHandler, Proxy}
import java.util.concurrent.atomic.AtomicInteger

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
      event = translator.translate(KeyStrokeInfo.fromKeyStroke(eofKeyStroke))
      
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
      eofEvent = translator.translate(KeyStrokeInfo.fromKeyStroke(new KeyStroke(KeyType.EOF)))

      // Test Ctrl+Q
      ctrlQEvent = translator.translate(KeyStrokeInfo.fromKeyStroke(new KeyStroke('q', true, false, false)))
      
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

  "Input handlers" should "treat screen closure as a quit event instead of ending the stream silently" in {
    val program = for
      inputRouter <- InputRouter.create[IO, Event](new TextEntryTranslator)
      handler = new ScreenInputHandler[IO, Event](screenReturning(null), inputRouter)
      events <- handler.eventStream.take(1).compile.toList
    yield
      events shouldBe List(com.serenity.keystroke.events.Quit)

    program.unsafeRunSync()
  }

  it should "treat terminal closure as a quit event instead of ending the stream silently" in {
    val program = for
      inputRouter <- InputRouter.create[IO, Event](new TextEntryTranslator)
      handler = new InputHandlerImpl[IO, Event](terminalReturning(null), inputRouter)
      events <- handler.eventStream.take(1).compile.toList
    yield
      events shouldBe List(com.serenity.keystroke.events.Quit)

    program.unsafeRunSync()
  }

  private def screenReturning(values: KeyStroke*): Screen =
    proxyReturning[Screen](classOf[Screen], values*)

  private def terminalReturning(values: KeyStroke*): Terminal =
    proxyReturning[Terminal](classOf[Terminal], values*)

  private def proxyReturning[A](iface: Class[A], values: KeyStroke*): A =
    val index = new AtomicInteger(0)
    val handler = new InvocationHandler:
      override def invoke(proxy: Any, method: java.lang.reflect.Method, args: Array[AnyRef] | Null): AnyRef =
        method.getName match
          case "readInput" =>
            val current = index.getAndIncrement()
            if current < values.length then values(current)
            else null
          case "hashCode" =>
            Integer.valueOf(System.identityHashCode(proxy))
          case "equals" =>
            java.lang.Boolean.valueOf(proxy.eq(Option(args).flatMap(_.headOption).orNull))
          case "toString" =>
            s"${iface.getSimpleName}Proxy"
          case _ =>
            defaultValue(method.getReturnType)

    Proxy.newProxyInstance(iface.getClassLoader, Array(iface), handler).asInstanceOf[A]

  private def defaultValue(returnType: Class[?]): AnyRef =
    if returnType == java.lang.Boolean.TYPE then java.lang.Boolean.FALSE
    else if returnType == java.lang.Integer.TYPE then Integer.valueOf(0)
    else if returnType == java.lang.Long.TYPE then java.lang.Long.valueOf(0L)
    else if returnType == java.lang.Double.TYPE then java.lang.Double.valueOf(0.0)
    else if returnType == java.lang.Float.TYPE then java.lang.Float.valueOf(0.0f)
    else if returnType == java.lang.Short.TYPE then java.lang.Short.valueOf(0.toShort)
    else if returnType == java.lang.Byte.TYPE then java.lang.Byte.valueOf(0.toByte)
    else if returnType == java.lang.Character.TYPE then java.lang.Character.valueOf('\u0000')
    else null
