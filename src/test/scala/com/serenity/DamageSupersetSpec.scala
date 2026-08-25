package com.serenity

import com.serenity.state.models.{BufferId, Damage, TextCaretStop, TextVisualLine}
import com.serenity.ui.layout.{DirtyLineDiff, TextLayoutSnapshot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #997 lands `Damage` unused: nothing in the render path emits it yet, so this pins the contract a future producer
  * (#999) must satisfy rather than exercising a real one. For each hand-built edit, `DirtyLineDiff` computes the rows a
  * frame diff already trusts are dirty, and a hand-built `Damage` value -- standing in for what a producer would report
  * for the same edit -- must coarsen to a row set that covers it. `DirtyLineDiff`'s own bias is "report too much rather
  * than too little"; `Damage` inherits that bias here.
  */
class DamageSupersetSpec extends AnyFlatSpec with Matchers:

  private val bufferId = BufferId(0)

  private def visualLine(bufferLine: Int, text: String, startColumn: Int = 0): TextVisualLine =
    TextVisualLine(
      bufferLine = bufferLine,
      startColumn = startColumn,
      endColumn = startColumn + text.length,
      text = text,
      widthPx = text.length * 8.0f,
      caretStops = (0 to text.length).toVector.map(offset => TextCaretStop(startColumn + offset, offset * 8.0f))
    )

  private def snapshot(lines: Vector[TextVisualLine]): TextLayoutSnapshot =
    TextLayoutSnapshot(visualLines = lines, panelWidthPx = 320, lineHeightPx = 16, ascentPx = 13)

  private def assertCovers(dirty: Set[Int], damage: Damage): Unit =
    withClue(s"dirty=$dirty coarsened=${Damage.coarsenToRows(bufferId, damage)} damage=$damage") {
      dirty.subsetOf(Damage.coarsenToRows(bufferId, damage)) shouldBe true
    }

  "a hand-built BufferRows report" should "cover the row DirtyLineDiff finds dirty for a single-character edit" in {
    val before = snapshot(Vector(visualLine(0, "alpha"), visualLine(1, "beta"), visualLine(2, "gamma")))
    val after  = snapshot(Vector(visualLine(0, "alphb"), visualLine(1, "beta"), visualLine(2, "gamma")))

    val dirty  = DirtyLineDiff.dirtyRows(Some(before), after)
    val damage = Damage.BufferRows(bufferId, Set(0))

    dirty shouldBe Set(0)
    assertCovers(dirty, damage)
  }

  it should "cover every row DirtyLineDiff finds dirty across a multi-row edit" in {
    val before = snapshot(Vector(visualLine(0, "alpha"), visualLine(1, "beta"), visualLine(2, "gamma")))
    val after  = snapshot(Vector(visualLine(0, "alphb"), visualLine(1, "betb"), visualLine(2, "gamma")))

    val dirty  = DirtyLineDiff.dirtyRows(Some(before), after)
    val damage = Damage.BufferRows(bufferId, Set(0, 1))

    dirty shouldBe Set(0, 1)
    assertCovers(dirty, damage)
  }

  it should "cover the dirty row when there is no previous frame, provided the report is Everything" in {
    val after = snapshot(Vector(visualLine(0, "alpha"), visualLine(1, "beta")))

    val dirty = DirtyLineDiff.dirtyRows(None, after)

    dirty shouldBe Set(0, 1)
    Damage.isEverything(Damage.Everything) shouldBe true
  }

  "a hand-built BufferCells report" should "coarsen to cover the edited row, same as a BufferRows report would" in {
    val before = snapshot(Vector(visualLine(0, "alpha")))
    val after  = snapshot(Vector(visualLine(0, "alphb")))

    val dirty  = DirtyLineDiff.dirtyRows(Some(before), after)
    val damage = Damage.BufferCells(bufferId, row = 0, fromColumn = 4, toColumn = Some(5))

    dirty shouldBe Set(0)
    assertCovers(dirty, damage)
  }

  it should "not cover a row it never reported, since coarsening never invents damage" in {
    val before = snapshot(Vector(visualLine(0, "alpha"), visualLine(1, "beta")))
    val after  = snapshot(Vector(visualLine(0, "alpha"), visualLine(1, "betb")))

    val dirty         = DirtyLineDiff.dirtyRows(Some(before), after)
    val underreported = Damage.BufferRows(bufferId, Set(0))

    dirty shouldBe Set(1)
    dirty.subsetOf(Damage.coarsenToRows(bufferId, underreported)) shouldBe false
  }
