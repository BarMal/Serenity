package com.serenity.input

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{ChromeElement, SummonConfig}
import com.serenity.keystroke.events.{ChromeSummonToggled, Event, InsertChar}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A terminal reports no key release, so its summon chord is one keystroke that toggles. */
class InputRouterSummonSpec extends AnyFlatSpec with Matchers:

  private def route(config: SummonConfig, strokes: KeyStrokeInfo*): List[Event] =
    (for
      router <- InputRouter.create[IO, Event](new TextEntryTranslator)
      _      <- router.setSummonConfig(config)
      events <- router.eventStream(Stream.emits(strokes.toList)).compile.toList
    yield events).unsafeRunSync()

  private def stroke(char: Char, modifiers: Modifier*) = KeyStrokeInfo(InputKey.Character, Some(char), modifiers.toSet)

  "A terminal's summon chord" should "toggle the element, whichever way the terminal spells Shift" in {
    route(SummonConfig(), stroke('l', Modifier.Alt, Modifier.Shift), stroke('S', Modifier.Alt)) shouldBe
      List(ChromeSummonToggled(ChromeElement.LineNumbers), ChromeSummonToggled(ChromeElement.StatusBar))
  }

  it should "follow the configured modifiers and letters" in {
    val config =
      SummonConfig(modifiers = Set(Modifier.Ctrl, Modifier.Alt)).withLetter(ChromeElement.Scrollbar, Some('x'))

    route(config, stroke('x', Modifier.Ctrl, Modifier.Alt)) shouldBe List(ChromeSummonToggled(ChromeElement.Scrollbar))
    route(config, stroke('b', Modifier.Ctrl, Modifier.Alt)) should not contain ChromeSummonToggled(
      ChromeElement.Scrollbar
    )
    route(config, stroke('x', Modifier.Alt, Modifier.Shift)) should not contain ChromeSummonToggled(
      ChromeElement.Scrollbar
    )
  }

  it should "leave every other keystroke to the translator" in {
    route(SummonConfig(), stroke('l')) shouldBe List(InsertChar('l'))
    route(SummonConfig(), stroke('l', Modifier.Alt)).collect {
      case toggled: ChromeSummonToggled => toggled
    } shouldBe Nil
  }

  "The router's summon config" should "start as the default" in {
    InputRouter.create[IO, Event](new TextEntryTranslator).unsafeRunSync().summonConfig shouldBe SummonConfig()
  }
