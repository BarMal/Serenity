package com.serenity

import java.awt.event.{FocusEvent, InputEvent, KeyEvent}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.swing.JPanel

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{ChromeElement, SummonConfig}
import com.serenity.input.{InputRouter, SummonReleaseTimer, SwingInputHandler}
import com.serenity.keystroke.Modifier
import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The held-chord quasimode as the window sees it: AWT key events in, summon events out, and the keys the chord uses
  * kept from typing or triggering hotkeys.
  */
class SwingInputHandlerSummonSpec extends AnyFlatSpec with Matchers:

  private val timeout = 10.seconds
  private val chord   = InputEvent.ALT_DOWN_MASK | InputEvent.SHIFT_DOWN_MASK
  private val Start   = 1_000L

  /** The timer that settles a release, run by the test instead of by the clock: a real one lapses whenever the machine
    * lets it, which can land between a repeat's release and its press.
    */
  final private class ManualReleaseTimer(onLapse: () => Unit) extends SummonReleaseTimer:
    private val armed   = new AtomicBoolean(false)
    def restart(): Unit = armed.set(true)
    def lapse(): Unit   = if armed.getAndSet(false) then onLapse()

  final private class Harness(config: SummonConfig = SummonConfig()):
    val component = new JPanel()

    val router = InputRouter
      .create[IO, Event](new TextEntryTranslator)
      .flatTap(_.setSummonConfig(config))
      .unsafeRunSync()

    private val releaseTimer = new AtomicReference[Option[ManualReleaseTimer]](None)

    val handler = new SwingInputHandler[IO, Event](
      component,
      router,
      () => CellMetrics(8, 16, 13),
      () => CellMetrics(8, 16, 13),
      summonReleaseTimer = onLapse =>
        val timer = new ManualReleaseTimer(onLapse)
        releaseTimer.set(Some(timer))
        timer
    )

    private val listener = component.getKeyListeners.head

    def press(code: Int, char: Char, mods: Int = chord, at: Long = Start): Harness =
      listener.keyPressed(KeyEvent(component, KeyEvent.KEY_PRESSED, at, mods, code, char)); this

    def release(code: Int, mods: Int = chord, at: Long = Start): Harness =
      listener.keyReleased(KeyEvent(component, KeyEvent.KEY_RELEASED, at, mods, code, KeyEvent.CHAR_UNDEFINED)); this

    def typed(char: Char, mods: Int = 0): Harness =
      listener.keyTyped(KeyEvent(component, KeyEvent.KEY_TYPED, Start, mods, KeyEvent.VK_UNDEFINED, char)); this

    def lapseReleaseTimer(): Harness =
      releaseTimer.get.foreach(_.lapse()); this

    def loseFocus(): Harness =
      component.getFocusListeners.foreach(_.focusLost(new FocusEvent(component, FocusEvent.FOCUS_LOST))); this

    def events(count: Int, within: FiniteDuration = timeout): Option[List[Event]] =
      handler.eventStream.take(count.toLong).compile.toList.unsafeRunTimed(within)

    def sentinel(): Harness = typed('~')

  private val lineNumbers = ChromeSummonPressed(ChromeElement.LineNumbers)
  private val released    = ChromeSummonReleased(ChromeElement.LineNumbers)

  "Pressing the chord" should "summon the element, and neither type its letter nor reach a hotkey" in {
    val harness = new Harness().press(KeyEvent.VK_L, 'L').typed('L').sentinel()

    harness.events(2) shouldBe Some(List(lineNumbers, InsertChar('~')))
  }

  it should "summon each configured element" in {
    val harness = new Harness()
    harness.press(KeyEvent.VK_S, 'S').press(KeyEvent.VK_T, 'T').press(KeyEvent.VK_B, 'B').press(KeyEvent.VK_H, 'H')

    harness.events(4) shouldBe Some(
      List(
        ChromeSummonPressed(ChromeElement.StatusBar),
        ChromeSummonPressed(ChromeElement.TabStrip),
        ChromeSummonPressed(ChromeElement.Scrollbar),
        ChromeSummonPressed(ChromeElement.PaneHeaders)
      )
    )
  }

  it should "summon nothing under other modifiers, and leave the key to the keyboard" in {
    val harness = new Harness().press(KeyEvent.VK_L, 'l', InputEvent.ALT_DOWN_MASK).typed('l').sentinel()

    harness.events(2).getOrElse(Nil).collect { case summon: ChromeSummonPressed => summon } shouldBe Nil
  }

  it should "leave an unbound chord letter to the keyboard" in {
    val harness = new Harness().press(KeyEvent.VK_Q, 'Q').typed('Q').sentinel()

    harness.events(2).getOrElse(Nil) should contain(InsertChar('Q'))
  }

  "Releasing the letter" should "end the summon once the auto-repeat window has passed" in {
    val harness = new Harness().press(KeyEvent.VK_L, 'L').release(KeyEvent.VK_L, at = Start + 500).lapseReleaseTimer()

    harness.events(2) shouldBe Some(List(lineNumbers, released))
  }

  "X11 auto-repeat" should "not end the summon, however many release and press pairs it sends" in {
    val harness = new Harness().press(KeyEvent.VK_L, 'L')
    for tick <- 1 to 5 do
      val when = Start + tick * 33L
      harness.release(KeyEvent.VK_L, at = when).press(KeyEvent.VK_L, 'L', at = when).typed('L')

    harness.lapseReleaseTimer().sentinel()
    harness.events(2) shouldBe Some(List(lineNumbers, InsertChar('~')))
    harness.release(KeyEvent.VK_L, at = Start + 300).lapseReleaseTimer()
    harness.events(1) shouldBe Some(List(released))
  }

  "Releasing a base modifier" should "end the summon at once" in {
    val harness = new Harness().press(KeyEvent.VK_L, 'L').release(KeyEvent.VK_ALT, InputEvent.SHIFT_DOWN_MASK)

    harness.events(2) shouldBe Some(List(lineNumbers, released))
  }

  "Losing window focus" should "end the summon, since the release will never arrive" in {
    val harness = new Harness().press(KeyEvent.VK_L, 'L').loseFocus()

    harness.events(2) shouldBe Some(List(lineNumbers, released))
  }

  "The lock key during the hold" should "lock, and not type a space" in {
    val harness = new Harness().press(KeyEvent.VK_L, 'L').press(KeyEvent.VK_SPACE, ' ').typed(' ').sentinel()

    harness.events(3) shouldBe Some(List(lineNumbers, ChromeSummonLocked, InsertChar('~')))
  }

  it should "type a space when no chord is held" in {
    val harness = new Harness().press(KeyEvent.VK_SPACE, ' ', 0).typed(' ')

    harness.events(1) shouldBe Some(List(InsertChar(' ')))
  }

  it should "type a space once the chord has ended" in {
    val harness = new Harness()
      .press(KeyEvent.VK_L, 'L')
      .release(KeyEvent.VK_ALT, InputEvent.SHIFT_DOWN_MASK)
      .press(KeyEvent.VK_SPACE, ' ', 0)
      .typed(' ')

    harness.events(3) shouldBe Some(List(lineNumbers, released, InsertChar(' ')))
  }

  "Normal typing during the hold" should "still reach the editor" in {
    val harness = new Harness().press(KeyEvent.VK_L, 'L').press(KeyEvent.VK_X, 'x', 0).typed('x')

    harness.events(2) shouldBe Some(List(lineNumbers, InsertChar('x')))
  }

  "A configured chord" should "use the configured modifiers, letter and lock key" in {
    val config = SummonConfig(modifiers = Set(Modifier.Ctrl, Modifier.Alt))
      .withLetter(ChromeElement.Scrollbar, Some('x'))
      .copy(lock = SummonConfig.parseLock("enter").getOrElse(fail("enter parses")))
    val mods    = InputEvent.CTRL_DOWN_MASK | InputEvent.ALT_DOWN_MASK
    val harness = new Harness(config).press(KeyEvent.VK_X, 'x', mods).press(KeyEvent.VK_ENTER, '\n', mods)

    harness.events(2) shouldBe Some(List(ChromeSummonPressed(ChromeElement.Scrollbar), ChromeSummonLocked))
  }

  "Ordinary typing" should "emit no summon events" in {
    val harness = new Harness().press(KeyEvent.VK_A, 'a', 0).typed('a')

    harness.events(1) shouldBe Some(List(InsertChar('a')))
  }
