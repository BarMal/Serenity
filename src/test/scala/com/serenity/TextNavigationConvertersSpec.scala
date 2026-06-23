package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.TextNavigationConverters
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TextNavigationConvertersSpec extends AnyFlatSpec with Matchers:

  "TextNavigationConverters" should "map shift-arrow keys to extend-selection events" in {
    val converter = TextNavigationConverters.navigationConverter
    val shift     = Set(Modifier.Shift)

    converter(KeyStrokeInfo(InputKey.ArrowLeft, None, shift)) shouldBe ExtendSelectionLeft
    converter(KeyStrokeInfo(InputKey.ArrowRight, None, shift)) shouldBe ExtendSelectionRight
    converter(KeyStrokeInfo(InputKey.ArrowUp, None, shift)) shouldBe ExtendSelectionUp
    converter(KeyStrokeInfo(InputKey.ArrowDown, None, shift)) shouldBe ExtendSelectionDown
  }
