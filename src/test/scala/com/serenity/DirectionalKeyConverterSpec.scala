package com.serenity

import com.serenity.keystroke.events.Direction
import com.serenity.keystroke.translators.DirectionalKeyConverter
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DirectionalKeyConverterSpec extends AnyFlatSpec with Matchers:

  "DirectionalKeyConverter" should "map arrow keys to directions through the provided wrapper" in {
    val converter = DirectionalKeyConverter.arrowKeys(identity)

    converter(KeyStrokeInfo(InputKey.ArrowUp, None, Set.empty)) shouldBe Direction.Up
    converter(KeyStrokeInfo(InputKey.ArrowDown, None, Set.empty)) shouldBe Direction.Down
    converter(KeyStrokeInfo(InputKey.ArrowLeft, None, Set.empty)) shouldBe Direction.Left
    converter(KeyStrokeInfo(InputKey.ArrowRight, None, Set.empty)) shouldBe Direction.Right
  }
