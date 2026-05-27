package com.serenity

import com.googlecode.lanterna.input.KeyType
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Direction
import com.serenity.keystroke.translators.DirectionalKeyConverter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DirectionalKeyConverterSpec extends AnyFlatSpec with Matchers:

  "DirectionalKeyConverter" should "map arrow keys to directions through the provided wrapper" in {
    val converter = DirectionalKeyConverter.arrowKeys(identity)

    converter(KeyStrokeInfo(KeyType.ArrowUp, None, Set.empty)) shouldBe Direction.Up
    converter(KeyStrokeInfo(KeyType.ArrowDown, None, Set.empty)) shouldBe Direction.Down
    converter(KeyStrokeInfo(KeyType.ArrowLeft, None, Set.empty)) shouldBe Direction.Left
    converter(KeyStrokeInfo(KeyType.ArrowRight, None, Set.empty)) shouldBe Direction.Right
  }
