package com.serenity.input

import java.awt.event.{InputEvent, KeyEvent}
import javax.swing.{JPanel, KeyStroke}

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Event, Paste, Undo}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MenuActivationGuardSpec extends AnyFlatSpec with Matchers:

  private val ctrlZ = KeyStroke.getKeyStroke(KeyEvent.VK_Z, InputEvent.CTRL_DOWN_MASK)
  private val ctrlV = KeyStroke.getKeyStroke(KeyEvent.VK_V, InputEvent.CTRL_DOWN_MASK)

  private def newHandler(component: JPanel): SwingInputHandler[IO, Event] =
    val router = InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync()
    new SwingInputHandler[IO, Event](component, router, () => CellMetrics(8, 16, 13))

  private def press(component: JPanel, code: Int, char: Char, modifiers: Int): Unit =
    component.getKeyListeners.head
      .keyPressed(new KeyEvent(component, KeyEvent.KEY_PRESSED, System.currentTimeMillis, modifiers, code, char))

  private def guardOver(
    handler: SwingInputHandler[IO, Event],
    sent: ListBuffer[Event],
    now: () => Long = () => System.currentTimeMillis
  ) = new MenuActivationGuard(() => handler.lastPressed, sent += _, now)

  "SwingInputHandler" should "record the last key press it queued" in {
    val component = new JPanel()
    val handler   = newHandler(component)

    handler.lastPressed shouldBe None
    press(component, KeyEvent.VK_Z, 'z', InputEvent.CTRL_DOWN_MASK)

    handler.lastPressed.map(_._1) shouldBe Some(ctrlZ)
  }

  it should "queue an event submitted from outside the key and mouse listeners" in {
    val component = new JPanel()
    val handler   = newHandler(component)

    handler.submit(Undo)

    val batches = handler.inputBatches.take(1).compile.toList.unsafeRunTimed(10.seconds)
    batches.map(_.flatMap(_.toList)) shouldBe Some(List(PendingInput.Ready(Undo)))
  }

  "The menu activation guard" should "send nothing for an accelerator the keymap path already received" in {
    val component = new JPanel()
    val handler   = newHandler(component)
    val sent      = ListBuffer.empty[Event]

    press(component, KeyEvent.VK_Z, 'z', InputEvent.CTRL_DOWN_MASK)
    guardOver(handler, sent).activate(Undo, Some(ctrlZ), viaKeyEvent = true)
    guardOver(handler, sent).activate(Undo, Some(ctrlZ), viaKeyEvent = false)

    sent.toList shouldBe Nil
  }

  it should "send exactly once for a click when no key was pressed" in {
    val component = new JPanel()
    val handler   = newHandler(component)
    val sent      = ListBuffer.empty[Event]

    guardOver(handler, sent).activate(Undo, Some(ctrlZ), viaKeyEvent = false)

    sent.toList shouldBe List(Undo)
  }

  it should "send exactly once for a click on an item with no accelerator" in {
    val component = new JPanel()
    val handler   = newHandler(component)
    val sent      = ListBuffer.empty[Event]

    press(component, KeyEvent.VK_Z, 'z', InputEvent.CTRL_DOWN_MASK)
    guardOver(handler, sent).activate(Paste, None, viaKeyEvent = false)

    sent.toList shouldBe List(Paste)
  }

  it should "send when the last key pressed was a different stroke" in {
    val component = new JPanel()
    val handler   = newHandler(component)
    val sent      = ListBuffer.empty[Event]

    press(component, KeyEvent.VK_Z, 'z', InputEvent.CTRL_DOWN_MASK)
    guardOver(handler, sent).activate(Paste, Some(ctrlV), viaKeyEvent = false)

    sent.toList shouldBe List(Paste)
  }

  it should "send a click on the same accelerator once the key press is old" in {
    val component = new JPanel()
    val handler   = newHandler(component)
    val sent      = ListBuffer.empty[Event]
    val later     = () => System.currentTimeMillis + MenuActivationGuard.KeyPressWindowMillis + 1

    press(component, KeyEvent.VK_Z, 'z', InputEvent.CTRL_DOWN_MASK)
    guardOver(handler, sent, later).activate(Undo, Some(ctrlZ), viaKeyEvent = false)

    sent.toList shouldBe List(Undo)
  }
