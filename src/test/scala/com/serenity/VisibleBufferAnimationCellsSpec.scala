package com.serenity

import java.awt.Color

import com.serenity.animation.CharacterKey
import com.serenity.rope.Balance
import com.serenity.state.manager.VisibleBufferAnimationCells
import com.serenity.state.models.{Buffer, BufferId, Viewport}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VisibleBufferAnimationCellsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "VisibleBufferAnimationCells" should "key unwrapped scrolled text by visible buffer columns" in {
    val buffer = Buffer
      .fromString(BufferId(1), "abcdefghijklmnopqrstuvwxyz")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 10, visibleLines = 1, visibleColumns = 5))

    val cells = VisibleBufferAnimationCells.fromBuffer(
      buffer,
      wordWrapEnabled = false,
      startColor = Color.BLACK,
      endColor = Color.WHITE
    )

    cells.keys.toSet.shouldBe((10 until 15).map(CharacterKey(_, 0)).toSet)
    cells(CharacterKey(10, 0)).char.shouldBe('k')
    cells(CharacterKey(14, 0)).char.shouldBe('o')
  }

  it should "skip unwrapped lines that are entirely left of the viewport" in {
    val buffer = Buffer
      .fromString(BufferId(1), "short")
      .copy(viewport = Viewport(topLine = 0, leftColumn = 10, visibleLines = 1, visibleColumns = 5))

    val cells = VisibleBufferAnimationCells.fromBuffer(
      buffer,
      wordWrapEnabled = false,
      startColor = Color.BLACK,
      endColor = Color.WHITE
    )

    cells.shouldBe(empty)
  }
