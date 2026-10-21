package com.serenity.ui.layout

import com.serenity.config.{AppConfig, AppMode}
import com.serenity.rope.Balance
import com.serenity.state.models.AppState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The prose font's measured widths belong to the state that asked for them. */
class ProseColumnOwnershipSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def guiProseState(): AppState =
    AppState.initial(AppConfig.default.withAppMode(AppMode.Prose).withProseMeasure(Some(66)))

  "The prose column's glyph widths" should "be measured once per state, however often the column is asked for" in {
    val (a, b) = (guiProseState(), guiProseState())

    val first = ProseColumn.widthCells(a)
    ProseColumn.widthCells(a) shouldBe first

    a.runtime.proseGlyphWidths.measurements shouldBe 1L
    b.runtime.proseGlyphWidths.measurements shouldBe 0L
  }

  it should "be shared by the copies of one state" in {
    val a      = guiProseState()
    val edited = a.copy(runtime = a.runtime.copy(safeMode = true))

    ProseColumn.widthCells(a)

    (edited.runtime.proseGlyphWidths eq a.runtime.proseGlyphWidths) shouldBe true
    ProseColumn.widthCells(edited)
    edited.runtime.proseGlyphWidths.measurements shouldBe 1L
  }
