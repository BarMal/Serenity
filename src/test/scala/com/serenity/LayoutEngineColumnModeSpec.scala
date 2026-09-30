package com.serenity

import com.serenity.config.AppConfig
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

  // Multi-column e-reader layout (issue #1338, Phase 2 / slice 4): count-driven resolution. `resolvedColumnCount` is
  // the single seam every caller routes through -- Auto (`columnCount = None`) reproduces the width-driven count;
  // `Some(n)` pins n, clamped to what the pane can hold (the anti-absurdity `MinColumnTextWidthCells` floor).
  behavior of "LayoutEngine.maxColumnsThatFit"

  it should "count how many minimum-width columns (plus the gap between them) fit, never fewer than one" in {
    // (200 + 2) / (8 + 2) = 20 columns of the 8-cell anti-absurdity floor.
    LayoutEngine.maxColumnsThatFit(contentWidthCells = 200, gap = 2) shouldBe 20
    // Narrower than a single floor column still yields one.
    LayoutEngine.maxColumnsThatFit(contentWidthCells = 4, gap = 2) shouldBe 1
  }

  behavior of "LayoutEngine.resolvedColumnCount"

  it should "reproduce the width-driven count in Auto mode (columnCount = None)" in {
    val surfaceConfig = AppConfig.default.withColumnTargetWidth(80).withColumnGap(2).surfaceConfig
    LayoutEngine.resolvedColumnCount(contentWidthCells = 200, surfaceConfig) shouldBe
      LayoutEngine.columnCount(contentWidthCells = 200, columnTargetWidthCells = 80, columnGap = 2)
  }

  it should "pin exactly the configured count when set and it fits" in {
    val surfaceConfig = AppConfig.default.withColumnGap(2).withColumnCount(Some(3)).surfaceConfig
    LayoutEngine.resolvedColumnCount(contentWidthCells = 200, surfaceConfig) shouldBe 3
  }

  it should "clamp an absurd configured count down to what the pane can hold" in {
    val surfaceConfig = AppConfig.default.withColumnGap(2).withColumnCount(Some(1000)).surfaceConfig
    LayoutEngine.resolvedColumnCount(contentWidthCells = 200, surfaceConfig) shouldBe
      LayoutEngine.maxColumnsThatFit(contentWidthCells = 200, gap = 2)
  }

  it should "clamp a configured count of zero or less up to one" in {
    val surfaceConfig = AppConfig.default.withColumnGap(2).withColumnCount(Some(0)).surfaceConfig
    LayoutEngine.resolvedColumnCount(contentWidthCells = 200, surfaceConfig) shouldBe 1
  }

  it should "yield a single column when the pane width is unknown (zero content width)" in {
    val autoConfig  = AppConfig.default.withColumnCount(None).surfaceConfig
    val countConfig = AppConfig.default.withColumnCount(Some(3)).surfaceConfig
    LayoutEngine.resolvedColumnCount(contentWidthCells = 0, autoConfig) shouldBe 1
    LayoutEngine.resolvedColumnCount(contentWidthCells = 0, countConfig) shouldBe 1
  }

  behavior of "LayoutEngine.columnWidthCellsForCount"

  it should "divide the content width evenly across a fixed count after subtracting the gaps" in {
    // 3 columns in 200 with gap 2: (200 - 2*2) / 3 = 65 cells each.
    LayoutEngine.columnWidthCellsForCount(contentWidthCells = 200, count = 3, gap = 2) shouldBe 65
  }

  it should "never fall below one cell, even for an over-large count" in {
    LayoutEngine.columnWidthCellsForCount(contentWidthCells = 10, count = 20, gap = 2) shouldBe 1
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

  behavior of "LayoutEngine per-column gutter reservation (issue #1338, Phase 2 / slice 2)"

  private val viewportSize = ViewportSize(160, 40)

  private def columnModeState(lineNumbers: Boolean): AppState =
    val buffer = Buffer.fromString(bufferId, (0 until 200).map(i => s"line-$i").mkString("\n"))
    val base   = AppState.initial
    base.copy(persisted =
      base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = base.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), PaneId(0))))
        ),
        focus = Focus.EditorPane(PaneId(0)),
        config = AppConfig.default.withoutStatusLine
          .withLineNumbers(lineNumbers)
          .withWordWrap(true)
          .withColumnMode(true)
          .withColumnTargetWidth(40)
          .withColumnGap(2)
      )
    )

  it should "reserve no single pane-level line-number rail in column mode -- the per-column rails replace it" in {
    val layout = LayoutEngine.calculateLayoutWithUI(columnModeState(lineNumbers = true), viewportSize)

    layout.lineNumberRect shouldBe None
    layout.rightLineNumberRect shouldBe None
  }

  it should "give the editor panel the full workspace width in column mode with line numbers on (no pane-level gutter shift)" in {
    val withNumbers    = LayoutEngine.calculateLayoutWithUI(columnModeState(lineNumbers = true), viewportSize)
    val withoutNumbers = LayoutEngine.calculateLayoutWithUI(columnModeState(lineNumbers = false), viewportSize)

    withNumbers.editorPanelRect shouldBe withoutNumbers.editorPanelRect
  }

  behavior of "LayoutEngine.perColumnGutterWidth"

  it should "match the single-column counter width when line numbers and column mode are both on" in {
    val state = columnModeState(lineNumbers = true)

    LayoutEngine.perColumnGutterWidth(state) shouldBe LayoutEngine.lineNumberCounterWidth(state)
    LayoutEngine.perColumnGutterWidth(state) should be > 0
  }

  it should "be zero when line numbers are off, even in column mode" in {
    LayoutEngine.perColumnGutterWidth(columnModeState(lineNumbers = false)) shouldBe 0
  }

  it should "be zero when column mode is off, even with line numbers on" in {
    val state     = columnModeState(lineNumbers = true)
    val columnOff = state.copy(persisted = state.persisted.copy(config = state.persisted.config.withColumnMode(false)))

    LayoutEngine.perColumnGutterWidth(columnOff) shouldBe 0
  }
