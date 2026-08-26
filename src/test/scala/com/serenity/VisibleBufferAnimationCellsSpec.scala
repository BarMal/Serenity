package com.serenity

import java.awt.Color

import com.serenity.rope.{Balance, Rope}
import com.serenity.state.manager.VisibleBufferAnimationCells
import com.serenity.state.models.{Buffer, BufferId, Document, Viewport}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class VisibleBufferAnimationCellsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "VisibleBufferAnimationCells" should "collect all visible cells below the animation cap" in {
    val buffer = Buffer(BufferId(1), Document(Rope("abcdef"))).copy(
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 1, visibleColumns = 6)
    )

    val cells = VisibleBufferAnimationCells.fromBuffer(
      buffer,
      wordWrapEnabled = false,
      startColor = Color.BLACK,
      endColor = Color.WHITE,
      maxAnimatedCells = 10
    )

    cells.size shouldBe 6
  }

  it should "cap large visible buffer animations before per-cell fade lists are allocated" in {
    val content = (1 to 100).map(_ => "x" * 100).mkString("\n")
    val buffer = Buffer(BufferId(1), Document(Rope(content))).copy(
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 100, visibleColumns = 100)
    )

    val cells = VisibleBufferAnimationCells.fromBuffer(
      buffer,
      wordWrapEnabled = false,
      startColor = Color.BLACK,
      endColor = Color.WHITE,
      maxAnimatedCells = 2_000
    )

    cells.size shouldBe 2_000
    cells.keys.map(_.line).maxOption shouldBe Some(19)
  }

end VisibleBufferAnimationCellsSpec
