package com.serenity.ui.terminal

import java.awt.event.{InputEvent, KeyEvent}
import javax.swing.{JPanel, KeyStroke}

import com.serenity.config.HotkeyTrigger
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MenuKeyStrokesSpec extends AnyFlatSpec with Matchers:

  private def strokeOf(binding: String): Option[KeyStroke] =
    HotkeyTrigger.parse(binding).flatMap(MenuKeyStrokes.of)

  "A trigger" should "become the stroke Swing names for the same keys" in {
    strokeOf("ctrl+z") shouldBe Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, InputEvent.CTRL_DOWN_MASK))
    strokeOf("meta+shift+z") shouldBe
      Some(KeyStroke.getKeyStroke(KeyEvent.VK_Z, InputEvent.META_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK))
    strokeOf("ctrl+alt+u") shouldBe
      Some(KeyStroke.getKeyStroke(KeyEvent.VK_U, InputEvent.CTRL_DOWN_MASK | InputEvent.ALT_DOWN_MASK))
  }

  it should "equal what KeyStroke.getKeyStrokeForEvent reports for a real key press" in {
    val press = new KeyEvent(new JPanel, KeyEvent.KEY_PRESSED, 0L, InputEvent.CTRL_DOWN_MASK, KeyEvent.VK_S, 's')

    strokeOf("ctrl+s") shouldBe Some(KeyStroke.getKeyStrokeForEvent(press))
  }

  it should "map named keys" in {
    strokeOf("f1") shouldBe Some(KeyStroke.getKeyStroke(KeyEvent.VK_F1, 0))
    strokeOf("ctrl+pageup") shouldBe Some(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP, InputEvent.CTRL_DOWN_MASK))
    strokeOf("alt+left") shouldBe Some(KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, InputEvent.ALT_DOWN_MASK))
  }

  it should "have no stroke for F10, which the Metal look and feel binds to the menu bar" in {
    strokeOf("f10") shouldBe None
  }

  it should "have no stroke for a double-tap modifier chord" in {
    strokeOf("ctrl+ctrl") shouldBe None
  }
