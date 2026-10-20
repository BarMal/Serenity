package com.serenity.ui.terminal

import java.awt.event.{InputEvent, KeyEvent}
import javax.swing.JPanel

import com.serenity.ui.terminal.AltTap.Input
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AltTapSpec extends AnyFlatSpec with Matchers:

  private def taps(inputs: Input*): Int =
    inputs
      .foldLeft((AltTap.State.Idle, 0)):
        case ((state, count), input) =>
          val (next, tapped) = AltTap.step(state, input)
          (next, if tapped then count + 1 else count)
      ._2

  "Alt pressed and released alone" should "be a tap" in {
    taps(Input.AltDown(alone = true), Input.AltUp) shouldBe 1
  }

  it should "stay one tap while the held key auto-repeats its press" in {
    taps(Input.AltDown(alone = true), Input.AltDown(alone = true), Input.AltDown(alone = true), Input.AltUp) shouldBe 1
  }

  it should "be a tap each time it is repeated" in {
    val once = Seq(Input.AltDown(alone = true), Input.AltUp)
    taps((once ++ once)*) shouldBe 2
  }

  "Alt used as a modifier" should "not be a tap when another key goes down while it is held" in {
    taps(Input.AltDown(alone = true), Input.OtherDown, Input.AltUp) shouldBe 0
  }

  it should "not be a tap when Alt came down with another modifier already held" in {
    taps(Input.AltDown(alone = false), Input.AltUp) shouldBe 0
  }

  it should "not leave a tap armed for the next lone release" in {
    taps(Input.AltDown(alone = true), Input.OtherDown, Input.AltUp, Input.AltUp) shouldBe 0
  }

  "A release of Alt with no press seen" should "not be a tap" in {
    taps(Input.AltUp) shouldBe 0
  }

  private val source = new JPanel

  private def key(id: Int, code: Int, modifiersEx: Int = 0): KeyEvent =
    val character = if id == KeyEvent.KEY_TYPED then 'f' else KeyEvent.CHAR_UNDEFINED
    new KeyEvent(source, id, 0L, modifiersEx, code, character)

  "A key event" should "be an Alt press that is alone when no other modifier is down" in {
    AltTap.inputOf(key(KeyEvent.KEY_PRESSED, KeyEvent.VK_ALT, InputEvent.ALT_DOWN_MASK)) shouldBe
      Some(Input.AltDown(alone = true))
  }

  it should "be an Alt press that is not alone under Ctrl, Shift or AltGr" in {
    for mask <- List(InputEvent.CTRL_DOWN_MASK, InputEvent.SHIFT_DOWN_MASK, InputEvent.ALT_GRAPH_DOWN_MASK) do
      AltTap.inputOf(key(KeyEvent.KEY_PRESSED, KeyEvent.VK_ALT, InputEvent.ALT_DOWN_MASK | mask)) shouldBe
        Some(Input.AltDown(alone = false))
  }

  it should "be an Alt release" in {
    AltTap.inputOf(key(KeyEvent.KEY_RELEASED, KeyEvent.VK_ALT)) shouldBe Some(Input.AltUp)
  }

  it should "count any other key press, AltGr and modifiers included, as another key" in {
    for code <- List(KeyEvent.VK_F, KeyEvent.VK_ESCAPE, KeyEvent.VK_ALT_GRAPH, KeyEvent.VK_CONTROL) do
      AltTap.inputOf(key(KeyEvent.KEY_PRESSED, code, InputEvent.ALT_DOWN_MASK)) shouldBe Some(Input.OtherDown)
  }

  it should "mean nothing when it is another key's release or a typed character" in {
    AltTap.inputOf(key(KeyEvent.KEY_RELEASED, KeyEvent.VK_F)) shouldBe None
    AltTap.inputOf(key(KeyEvent.KEY_TYPED, KeyEvent.VK_UNDEFINED)) shouldBe None
  }

  private val alt   = InputEvent.ALT_DOWN_MASK
  private val shift = InputEvent.SHIFT_DOWN_MASK

  private def tapsIn(events: KeyEvent*): Int =
    taps(events.flatMap(AltTap.inputOf)*)

  "A summon chord" should "not be an Alt tap when Alt goes down first" in {
    tapsIn(
      key(KeyEvent.KEY_PRESSED, KeyEvent.VK_ALT, alt),
      key(KeyEvent.KEY_PRESSED, KeyEvent.VK_SHIFT, alt | shift),
      key(KeyEvent.KEY_PRESSED, KeyEvent.VK_L, alt | shift),
      key(KeyEvent.KEY_RELEASED, KeyEvent.VK_L, alt | shift),
      key(KeyEvent.KEY_RELEASED, KeyEvent.VK_SHIFT, alt),
      key(KeyEvent.KEY_RELEASED, KeyEvent.VK_ALT)
    ) shouldBe 0
  }

  it should "not be an Alt tap when Shift goes down first" in {
    tapsIn(
      key(KeyEvent.KEY_PRESSED, KeyEvent.VK_SHIFT, shift),
      key(KeyEvent.KEY_PRESSED, KeyEvent.VK_ALT, alt | shift),
      key(KeyEvent.KEY_PRESSED, KeyEvent.VK_S, alt | shift),
      key(KeyEvent.KEY_RELEASED, KeyEvent.VK_S, alt | shift),
      key(KeyEvent.KEY_RELEASED, KeyEvent.VK_ALT, shift),
      key(KeyEvent.KEY_RELEASED, KeyEvent.VK_SHIFT)
    ) shouldBe 0
  }

  it should "not be an Alt tap when the letter is released and pressed again by auto-repeat, or Alt alone is the base" in {
    tapsIn(
      key(KeyEvent.KEY_PRESSED, KeyEvent.VK_ALT, alt),
      key(KeyEvent.KEY_PRESSED, KeyEvent.VK_L, alt),
      key(KeyEvent.KEY_RELEASED, KeyEvent.VK_L, alt),
      key(KeyEvent.KEY_PRESSED, KeyEvent.VK_L, alt),
      key(KeyEvent.KEY_RELEASED, KeyEvent.VK_L, alt),
      key(KeyEvent.KEY_RELEASED, KeyEvent.VK_ALT)
    ) shouldBe 0
  }

  it should "leave a plain Alt tap after the chord a tap" in {
    tapsIn(
      key(KeyEvent.KEY_PRESSED, KeyEvent.VK_ALT, alt),
      key(KeyEvent.KEY_PRESSED, KeyEvent.VK_L, alt),
      key(KeyEvent.KEY_RELEASED, KeyEvent.VK_L, alt),
      key(KeyEvent.KEY_RELEASED, KeyEvent.VK_ALT),
      key(KeyEvent.KEY_PRESSED, KeyEvent.VK_ALT, alt),
      key(KeyEvent.KEY_RELEASED, KeyEvent.VK_ALT)
    ) shouldBe 1
  }
