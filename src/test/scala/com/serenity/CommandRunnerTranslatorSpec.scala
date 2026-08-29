package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.CommandRunnerTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRunnerTranslatorSpec extends AnyFlatSpec with Matchers:

  private val translator = new CommandRunnerTranslator()

  "CommandRunnerTranslator" should "insert an unmodified character" in {
    translator.translate(KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty)) shouldBe RunnerInsertChar('a')
  }

  it should "insert a shift-only character" in {
    translator.translate(
      KeyStrokeInfo(InputKey.Character, Some('A'), Set(Modifier.Shift))
    ) shouldBe RunnerInsertChar('A')
  }

  it should "not insert a ctrl-modified character" in {
    translator.translate(
      KeyStrokeInfo(InputKey.Character, Some('a'), Set(Modifier.Ctrl))
    ) should not be a[RunnerInsertChar]
  }

  it should "not insert an alt-modified character" in {
    translator.translate(
      KeyStrokeInfo(InputKey.Character, Some('a'), Set(Modifier.Alt))
    ) should not be a[RunnerInsertChar]
  }
