package com.serenity

import java.awt.event.{InputMethodEvent, KeyEvent}
import java.awt.font.TextHitInfo
import java.text.AttributedString
import java.util.concurrent.atomic.AtomicReference
import javax.swing.JPanel

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.input.{ImeRequestsHost, InputRouter, SwingInputHandler}
import com.serenity.keystroke.events.{Event, InsertChar}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The IME diagnostic (issue #2008) is opt-in: with no sink the component is untouched, with one every
  * `InputMethodEvent` and `KEY_TYPED` is logged and typing still reaches the pipeline unchanged.
  */
class SwingInputHandlerImeDiagnosticsSpec extends AnyFlatSpec with Matchers:

  private class HostPanel extends JPanel with ImeRequestsHost:
    private val installedRef = new AtomicReference[Option[java.awt.im.InputMethodRequests]](None)
    def installed: Option[java.awt.im.InputMethodRequests]                          = installedRef.get()
    def installInputMethodRequests(requests: java.awt.im.InputMethodRequests): Unit = installedRef.set(Some(requests))
    val caret                                   = new AtomicReference[Option[java.awt.Rectangle]](None)
    def caretBounds: Option[java.awt.Rectangle] = caret.get()

  private def handlerOn(component: JPanel, sink: Option[String => Unit]): SwingInputHandler[IO, Event] =
    val router = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    new SwingInputHandler[IO, Event](
      component,
      router,
      () => CellMetrics(8, 16, 13),
      () => CellMetrics(8, 16, 13),
      imeDiagnostics = sink
    )

  private def imeEvent(component: JPanel, committed: Int): InputMethodEvent =
    new InputMethodEvent(
      component,
      InputMethodEvent.INPUT_METHOD_TEXT_CHANGED,
      3L,
      new AttributedString("ab").getIterator,
      committed,
      TextHitInfo.leading(1),
      null
    )

  "SwingInputHandler" should "listen for input-method events and answer its requests whether or not diagnostics are on" in {
    val panel = new HostPanel
    val _     = handlerOn(panel, None)
    panel.getInputMethodListeners.length shouldBe 1
    panel.installed should not be empty
  }

  it should "log an InputMethodEvent when diagnostics are on" in {
    val panel = new HostPanel
    val seen  = scala.collection.mutable.ListBuffer.empty[String]
    val _     = handlerOn(panel, Some(seen += _))
    panel.getInputMethodListeners.length shouldBe 1
    panel.getInputMethodListeners.head.inputMethodTextChanged(imeEvent(panel, committed = 1))
    seen.toList.exists(l => l.contains("IME id=INPUT_METHOD_TEXT_CHANGED") && l.contains("committed=1")) shouldBe true
  }

  it should "log caret changes too" in {
    val panel = new HostPanel
    val seen  = scala.collection.mutable.ListBuffer.empty[String]
    val _     = handlerOn(panel, Some(seen += _))
    panel.getInputMethodListeners.head.caretPositionChanged(
      new InputMethodEvent(panel, InputMethodEvent.CARET_POSITION_CHANGED, TextHitInfo.leading(0), null)
    )
    seen.toList.exists(_.contains("CARET_POSITION_CHANGED")) shouldBe true
  }

  it should "install input-method requests on a host component" in {
    val panel = new HostPanel
    val _     = handlerOn(panel, Some(_ => ()))
    panel.installed should not be empty
  }

  it should "answer the platform's request for a text location with the host's caret, so the candidate window follows it" in {
    val panel = new HostPanel
    val _     = handlerOn(panel, None)
    panel.caret.set(Some(new java.awt.Rectangle(10, 20, 2, 16)))

    panel.installed.map(_.getTextLocation(null)) shouldBe Some(new java.awt.Rectangle(10, 20, 2, 16))
  }

  it should "tolerate a component that cannot host requests" in {
    val panel = new JPanel
    val _     = handlerOn(panel, Some(_ => ()))
    panel.getInputMethodListeners.length shouldBe 1
  }

  it should "log KEY_TYPED and still deliver the character unchanged" in {
    val panel   = new HostPanel
    val seen    = scala.collection.mutable.ListBuffer.empty[String]
    val handler = handlerOn(panel, Some(seen += _))
    panel.getKeyListeners.head.keyTyped(KeyEvent(panel, KeyEvent.KEY_TYPED, 5L, 0, KeyEvent.VK_UNDEFINED, 'a'))
    seen.toList.exists(_.contains("KEY_TYPED")) shouldBe true
    val events = handler.eventStream.take(1).compile.toList.unsafeRunTimed(10.seconds)
    events.exists(_.exists { case InsertChar('a') => true; case _ => false }) shouldBe true
  }

  it should "not log KEY_TYPED when diagnostics are off" in {
    val panel   = new HostPanel
    val handler = handlerOn(panel, None)
    panel.getKeyListeners.head.keyTyped(KeyEvent(panel, KeyEvent.KEY_TYPED, 5L, 0, KeyEvent.VK_UNDEFINED, 'a'))
    handler.eventStream.take(1).compile.toList.unsafeRunTimed(10.seconds).map(_.length) shouldBe Some(1)
  }
