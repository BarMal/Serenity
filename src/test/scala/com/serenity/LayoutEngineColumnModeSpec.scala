package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Column-based document layout (issue #1338, Phase 1): "as many columns as fit" at a configured target width (e-reader
  * style), not a fixed user-picked count. Split out of LayoutEngineSpec to keep that file under the architecture
  * ratchet's file-length target (following LayoutEngineTabBarSpec's precedent).
  */
class LayoutEngineColumnModeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  behavior of "LayoutEngine.columnCount"

  it should "fit as many columns as the target width and gap allow" in {
    // (200 + 2) / (80 + 2) = 2.46 -> 2 columns.
    LayoutEngine.columnCount(contentWidthCells = 200, columnTargetWidthCells = 80, columnGap = 2) shouldBe 2
  }

  it should "never return fewer than one column, even when the content is narrower than the target width" in {
    LayoutEngine.columnCount(contentWidthCells = 40, columnTargetWidthCells = 80, columnGap = 2) shouldBe 1
  }

  it should "fit exactly one more column once the content width crosses the next threshold" in {
    LayoutEngine.columnCount(contentWidthCells = 162, columnTargetWidthCells = 80, columnGap = 2) shouldBe 2
    LayoutEngine.columnCount(contentWidthCells = 161, columnTargetWidthCells = 80, columnGap = 2) shouldBe 1
  }

  behavior of "LayoutEngine.columnWidthCells"

  it should "divide the remaining width evenly across the fitted columns after subtracting the gaps between them" in {
    // 2 columns fit in 200; (200 - 1*2) / 2 = 99 cells each.
    LayoutEngine.columnWidthCells(contentWidthCells = 200, columnTargetWidthCells = 80, columnGap = 2) shouldBe 99
  }

  it should "use the full content width for a single column" in {
    LayoutEngine.columnWidthCells(contentWidthCells = 40, columnTargetWidthCells = 80, columnGap = 2) shouldBe 40
  }

  behavior of "LayoutEngine.updateBufferViewportDimensions with column mode"

  private def bufferWithViewport: Buffer =
    Buffer.fromString(bufferId, "line one\nline two\nline three")

  it should "derive visibleColumns from a single column's width, not the full content rect, when column mode and word wrap are both on" in {
    val panelRect = LayoutRect(0, 0, 200, 30)

    val viewport = LayoutEngine.updateBufferViewportDimensions(
      bufferWithViewport,
      panelRect,
      wordWrapEnabled = true,
      columnModeEnabled = true,
      columnTargetWidthCells = 80,
      columnGap = 2
    )

    viewport.visibleColumns shouldBe 99
  }

  it should "fall back to the full content rect width when word wrap is off, even if column mode is on" in {
    val panelRect = LayoutRect(0, 0, 200, 30)

    val viewport = LayoutEngine.updateBufferViewportDimensions(
      bufferWithViewport,
      panelRect,
      wordWrapEnabled = false,
      columnModeEnabled = true,
      columnTargetWidthCells = 80,
      columnGap = 2
    )

    viewport.visibleColumns shouldBe 200
  }

  it should "fall back to the full content rect width when column mode is off" in {
    val panelRect = LayoutRect(0, 0, 200, 30)

    val viewport = LayoutEngine.updateBufferViewportDimensions(
      bufferWithViewport,
      panelRect,
      wordWrapEnabled = true,
      columnModeEnabled = false,
      columnTargetWidthCells = 80,
      columnGap = 2
    )

    viewport.visibleColumns shouldBe 200
  }

  it should "keep the existing three-argument overload's behaviour unchanged (column mode off)" in {
    val panelRect = LayoutRect(0, 0, 200, 30)

    val viewport = LayoutEngine.updateBufferViewportDimensions(bufferWithViewport, panelRect, wordWrapEnabled = true)

    viewport.visibleColumns shouldBe 200
  }
