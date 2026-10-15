package com.serenity.input

import com.serenity.config.{ChromeElement, SummonConfig}
import com.serenity.input.SummonChordTracker.*
import com.serenity.keystroke.{InputKey, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SummonChordTrackerSpec extends AnyFlatSpec with Matchers:

  private val config   = SummonConfig()
  private val chord    = Set(Modifier.Alt, Modifier.Shift)
  private val keyL     = RawKey(76, Symbol.Glyph('l'))
  private val keyS     = RawKey(83, Symbol.Glyph('s'))
  private val keyA     = RawKey(65, Symbol.Glyph('a'))
  private val space    = RawKey(32, Symbol.Glyph(' '))
  private val altKey   = RawKey(18, Symbol.Mod(Modifier.Alt))
  private val shiftKey = RawKey(16, Symbol.Mod(Modifier.Shift))
  private val gap      = SummonChordTracker.RepeatGapMillis

  /** Runs steps in order, collecting what each one signalled and whether it consumed the key. */
  final private case class Run(state: State, signals: List[Signal], consumed: List[Boolean])

  private def run(steps: (State => Result)*): Run =
    steps.foldLeft(Run(State.empty, Nil, Nil)) { (acc, step) =>
      val result = step(acc.state)
      Run(result.state, acc.signals ++ result.signals, acc.consumed :+ result.consumed)
    }

  private def press(key: RawKey, at: Long, modifiers: Set[Modifier] = chord): State => Result =
    SummonChordTracker.pressed(_, config, key, modifiers, at)
  private def release(key: RawKey, at: Long): State => Result = SummonChordTracker.released(_, config, key, at)
  private val flush: State => Result                          = SummonChordTracker.flushed
  private val reset: State => Result                          = SummonChordTracker.reset

  "Pressing a chord's letter" should "begin that element and consume the key" in {
    val result = run(press(keyL, 0))

    result.signals shouldBe List(Signal.Held(ChromeElement.LineNumbers))
    result.consumed shouldBe List(true)
  }

  it should "not begin anything under other modifiers" in {
    run(press(keyL, 0, Set(Modifier.Alt))).signals shouldBe Nil
    run(press(keyL, 0, Set.empty)).consumed shouldBe List(false)
    run(press(keyL, 0, Set(Modifier.Ctrl, Modifier.Alt, Modifier.Shift))).consumed shouldBe List(false)
  }

  it should "leave an unbound letter to the rest of the keyboard" in {
    val result = run(press(keyA, 0))

    result.signals shouldBe Nil
    result.consumed shouldBe List(false)
  }

  it should "begin each element once while both are held" in {
    run(press(keyL, 0), press(keyS, 50)).signals shouldBe
      List(Signal.Held(ChromeElement.LineNumbers), Signal.Held(ChromeElement.StatusBar))
  }

  "Releasing the letter" should "end the element once the repeat window has passed with no press" in {
    val result = run(press(keyL, 0), release(keyL, 500), flush)

    result.signals shouldBe List(Signal.Held(ChromeElement.LineNumbers), Signal.Released(ChromeElement.LineNumbers))
  }

  it should "not end the element until the window has been waited out" in {
    run(press(keyL, 0), release(keyL, 500)).signals shouldBe List(Signal.Held(ChromeElement.LineNumbers))
  }

  it should "end the element when a different key goes down after the window" in {
    val result = run(press(keyL, 0), release(keyL, 500), press(keyA, 500 + gap + 1))

    result.signals shouldBe List(Signal.Held(ChromeElement.LineNumbers), Signal.Released(ChromeElement.LineNumbers))
  }

  "X11 auto-repeat, a release and a press of the same key together," should "not end the element" in {
    val result = run(
      press(keyL, 0),
      release(keyL, 500),
      press(keyL, 500),
      release(keyL, 530),
      press(keyL, 530),
      flush
    )

    result.signals shouldBe List(Signal.Held(ChromeElement.LineNumbers))
  }

  it should "consume the repeated press, so its key neither types nor reaches a hotkey" in {
    val result = run(press(keyL, 0), release(keyL, 500), press(keyL, 500))

    result.consumed shouldBe List(true, false, true)
  }

  it should "still end the element at the real release after the repeats" in {
    val result = run(press(keyL, 0), release(keyL, 500), press(keyL, 500), release(keyL, 530), flush)

    result.signals shouldBe List(Signal.Held(ChromeElement.LineNumbers), Signal.Released(ChromeElement.LineNumbers))
  }

  it should "count a press exactly at the edge of the window as a repeat, and one just past it as a new press" in {
    val atEdge = run(press(keyL, 0), release(keyL, 100), press(keyL, 100 + gap), flush)
    atEdge.signals shouldBe List(Signal.Held(ChromeElement.LineNumbers))

    val pastEdge = run(press(keyL, 0), release(keyL, 100), press(keyL, 100 + gap + 1))
    pastEdge.signals shouldBe List(
      Signal.Held(ChromeElement.LineNumbers),
      Signal.Released(ChromeElement.LineNumbers),
      Signal.Held(ChromeElement.LineNumbers)
    )
  }

  it should "not mistake a release followed by a press of a different key for a repeat" in {
    val result = run(press(keyL, 0), release(keyL, 100), press(keyS, 100))

    result.signals shouldBe List(
      Signal.Held(ChromeElement.LineNumbers),
      Signal.Released(ChromeElement.LineNumbers),
      Signal.Held(ChromeElement.StatusBar)
    )
  }

  "A held key that auto-repeats with presses alone, as on Windows and macOS," should "begin the element once" in {
    val result = run(press(keyL, 0), press(keyL, 30), press(keyL, 60))

    result.signals shouldBe List(Signal.Held(ChromeElement.LineNumbers))
    result.consumed shouldBe List(true, true, true)
  }

  "Releasing a base modifier while the letter is held" should "end every held element at once" in {
    val result = run(press(keyL, 0), press(keyS, 10), release(altKey, 200))

    result.signals shouldBe List(
      Signal.Held(ChromeElement.LineNumbers),
      Signal.Held(ChromeElement.StatusBar),
      Signal.Released(ChromeElement.LineNumbers),
      Signal.Released(ChromeElement.StatusBar)
    )
  }

  it should "leave a later release of the letter with nothing to end" in {
    val result = run(press(keyL, 0), release(shiftKey, 200), release(keyL, 210), flush)

    result.signals.count(_.isInstanceOf[Signal.Released]) shouldBe 1
  }

  "Releasing a modifier nobody is chording with" should "signal nothing" in {
    run(release(altKey, 0), flush).signals shouldBe Nil
    run(press(keyL, 0), release(RawKey(17, Symbol.Mod(Modifier.Ctrl)), 5), flush).signals shouldBe
      List(Signal.Held(ChromeElement.LineNumbers))
  }

  "The lock key" should "lock while a chord is held, and be consumed" in {
    val result = run(press(keyL, 0), press(space, 40))

    result.signals shouldBe List(Signal.Held(ChromeElement.LineNumbers), Signal.Locked)
    result.consumed shouldBe List(true, true)
  }

  it should "lock again on each press, which the reducer treats as the same lock" in {
    run(press(keyL, 0), press(space, 40), press(space, 80)).signals.count(_ == Signal.Locked) shouldBe 2
  }

  it should "type normally when no chord is held" in {
    val result = run(press(space, 0, Set.empty))

    result.signals shouldBe Nil
    result.consumed shouldBe List(false)
  }

  it should "type normally once the chord has ended" in {
    val result = run(press(keyL, 0), release(altKey, 100), press(space, 120, Set(Modifier.Shift)))

    result.consumed.last shouldBe false
    result.signals.contains(Signal.Locked) shouldBe false
  }

  it should "type normally under modifiers other than the chord's" in {
    run(press(keyL, 0), press(space, 20, Set(Modifier.Ctrl))).consumed.last shouldBe false
  }

  it should "follow a configured key, such as Enter" in {
    val enterConfig = config.copy(lock = SummonConfig.parseLock("enter").getOrElse(fail("enter parses")))
    val enter       = RawKey(10, Symbol.Named(InputKey.Enter))
    val first       = SummonChordTracker.pressed(State.empty, enterConfig, keyL, chord, 0)
    val second      = SummonChordTracker.pressed(first.state, enterConfig, enter, chord, 10)

    second.signals shouldBe List(Signal.Locked)
    second.consumed shouldBe true
    SummonChordTracker.pressed(first.state, enterConfig, space, chord, 10).consumed shouldBe false
  }

  "A consumed key" should "swallow the character the OS types for it, once" in {
    val begun = SummonChordTracker.pressed(State.empty, config, keyL, chord, 0)

    val (afterFirst, firstSwallowed) = SummonChordTracker.typed(begun.state)
    val (_, secondSwallowed)         = SummonChordTracker.typed(afterFirst)

    firstSwallowed shouldBe true
    secondSwallowed shouldBe false
  }

  it should "swallow the character typed for a lock press too" in {
    val locked = run(press(keyL, 0), press(space, 10))

    SummonChordTracker.typed(locked.state)._2 shouldBe true
  }

  "A key that is not consumed" should "leave its typed character alone, even with a chord held" in {
    val result = run(press(keyL, 0), press(keyA, 20))

    SummonChordTracker.typed(result.state)._2 shouldBe false
  }

  it should "leave a typed character alone after a release" in {
    val result = run(press(keyL, 0), release(keyL, 20))

    SummonChordTracker.typed(result.state)._2 shouldBe false
  }

  "Resetting, as when the window loses focus," should "end everything held and forget the rest" in {
    val result = run(press(keyL, 0), press(keyS, 5), release(keyS, 20), reset)

    result.signals.collect { case Signal.Released(element) => element }.toSet shouldBe
      Set(ChromeElement.LineNumbers, ChromeElement.StatusBar)
    result.state shouldBe State.empty
  }

  "Flushing with nothing pending" should "signal nothing" in {
    run(press(keyL, 0), flush).signals shouldBe List(Signal.Held(ChromeElement.LineNumbers))
  }
