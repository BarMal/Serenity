package com.serenity

import com.serenity.keystroke.events.{RunnerDismiss, RunnerRecordBinding}
import com.serenity.keystroke.translators.HotkeyRecordingTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HotkeyRecordingTranslatorSpec extends AnyFlatSpec with Matchers:

  "HotkeyRecordingTranslator" should "dismiss recording on Escape without sampling the clock" in {
    val translator = new HotkeyRecordingTranslator(() => fail("clock should not be sampled for Escape"))

    translator.translate(KeyStrokeInfo(InputKey.Escape, None, Set.empty)) shouldBe RunnerDismiss
  }

  it should "record any other stroke, stamped with the injected clock" in {
    val translator = new HotkeyRecordingTranslator(() => 4_200L)
    val stroke     = KeyStrokeInfo(InputKey.Character, Some('k'), Set(Modifier.Ctrl))

    translator.translate(stroke) shouldBe RunnerRecordBinding(stroke, 4_200L)
  }

  it should "record a bare modifier stroke without deciding tap count itself" in {
    val translator = new HotkeyRecordingTranslator(() => 9_000L)
    val stroke     = KeyStrokeInfo(InputKey.Ctrl, None, Set.empty)

    translator.translate(stroke) shouldBe RunnerRecordBinding(stroke, 9_000L)
  }

  // Each construction takes `now` as an injected function rather than a captured value (see the source doc comment),
  // so the clock must be re-sampled on every translated stroke rather than stamped once at construction.
  it should "sample the clock fresh on every translated stroke" in {
    val clock      = Iterator.from(1)
    val translator = new HotkeyRecordingTranslator(() => clock.next().toLong)
    val stroke     = KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty)

    translator.translate(stroke) shouldBe RunnerRecordBinding(stroke, 1L)
    translator.translate(stroke) shouldBe RunnerRecordBinding(stroke, 2L)
  }
