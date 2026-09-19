package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.LayoutRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Selection grow/settle (issue #1085 phase 3): [[SelectionGeometry.rectsForSelection]] is the per-visual-line column
  * extent `CursorViewport`'s seed hook diffs before/after `Selection`s with. Mirrors `CursorGlideGeometrySpec`'s own
  * structure -- it reuses the same word-wrap measurement `CursorGlideGeometry` does.
  */
class SelectionGeometrySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(1)

  private def bufferWith(
    content: String,
    viewport: Viewport = Viewport(visibleLines = 10, visibleColumns = 80)
  ): Buffer =
    Buffer.fromString(bufferId, content).copy(viewport = viewport)

  "rectsForSelection" should "produce a single-line extent for a selection within one buffer line" in {
    val buffer    = bufferWith("hello world")
    val selection = Selection(anchor = CursorPosition(0, 2), focus = CursorPosition(0, 5))

    val rects = SelectionGeometry.rectsForSelection(buffer, AppConfig.default, selection)

    rects shouldBe Map(SelectionLineKey(0, 0) -> LayoutRect(x = 2, y = 0, width = 3, height = 1))
  }

  it should "produce one extent per buffer line spanned, clipped to the selection's own start/end column" in {
    val buffer    = bufferWith("one\ntwo\nthree")
    val selection = Selection(anchor = CursorPosition(0, 1), focus = CursorPosition(2, 2))

    val rects = SelectionGeometry.rectsForSelection(buffer, AppConfig.default, selection)

    rects(SelectionLineKey(0, 0)) shouldBe LayoutRect(1, 0, 2, 1)
    rects(SelectionLineKey(1, 0)) shouldBe LayoutRect(0, 0, 3, 1)
    rects(SelectionLineKey(2, 0)) shouldBe LayoutRect(0, 0, 2, 1)
  }

  it should "resolve the same regardless of which end of the selection is the anchor" in {
    val buffer = bufferWith("one\ntwo")

    val forward  = Selection(anchor = CursorPosition(0, 1), focus = CursorPosition(1, 2))
    val backward = Selection(anchor = CursorPosition(1, 2), focus = CursorPosition(0, 1))

    SelectionGeometry.rectsForSelection(buffer, AppConfig.default, forward) shouldBe
      SelectionGeometry.rectsForSelection(buffer, AppConfig.default, backward)
  }

  it should "produce an empty map for an empty (zero-width) selection" in {
    val buffer    = bufferWith("hello world")
    val selection = Selection(anchor = CursorPosition(0, 3), focus = CursorPosition(0, 3))

    SelectionGeometry.rectsForSelection(buffer, AppConfig.default, selection) shouldBe empty
  }

  it should "split a word-wrapped line's selection across its own visual rows" in {
    val longLine  = "a" * 60
    val buffer    = bufferWith(longLine, viewport = Viewport(visibleLines = 20, visibleColumns = 20))
    val config    = AppConfig.default
    val selection = Selection(anchor = CursorPosition(0, 0), focus = CursorPosition(0, longLine.length))

    val rects = SelectionGeometry.rectsForSelection(buffer, config, selection)

    rects.keys.map(_.bufferLine).toSet shouldBe Set(0)
    rects.size should be > 1
  }
