package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, Viewport}
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Column-based document layout (issue #1338, Phase 1): partitions the same wrapped visual-line stream
  * `TextLayoutSnapshot.fromBuffer` already produces into `visibleLines`-sized groups -- one per column -- reusing the
  * existing wrap logic unchanged. Split out of `TextLayoutSnapshotSpec` to keep that file under the architecture
  * ratchet's file-length target (following `LayoutEngineTabBarSpec`'s precedent).
  */
class TextLayoutSnapshotColumnChunkingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val font     = com.serenity.ui.fonts.FontLoader.previewCodeFont(com.serenity.ui.fonts.FontLoader.FontConfig())

  // A wide column and short lines keep every logical line to exactly one visual row, so chunk boundaries land on
  // simple, predictable line numbers.
  private def bufferOfLines(count: Int, visibleLines: Int, topLine: Int = 0, topVisualLine: Int = 0): Buffer =
    val content = (0 until count).map(i => s"line $i").mkString("\n")
    Buffer
      .fromString(bufferId, content)
      .copy(viewport =
        Viewport(
          topLine = topLine,
          leftColumn = 0,
          visibleColumns = 200,
          visibleLines = visibleLines,
          topVisualLine = topVisualLine
        )
      )

  behavior of "TextLayoutSnapshot.columnChunksForBuffer"

  it should "partition the wrapped visual-line stream into visibleLines-sized groups, one per column" in {
    val buffer = bufferOfLines(count = 30, visibleLines = 8)

    val chunks = TextLayoutSnapshot.columnChunksForBuffer(
      buffer,
      columnWidthPx = 2000,
      font = font,
      cellMetricsOverride = Some(CellMetrics.cellUnit),
      forceCellLayout = true,
      columnCount = 3
    )

    chunks.length shouldBe 3
    chunks.map(_.length) shouldBe List(8, 8, 8)
    chunks(0).map(_.bufferLine) shouldBe (0 until 8).toList
    chunks(1).map(_.bufferLine) shouldBe (8 until 16).toList
    chunks(2).map(_.bufferLine) shouldBe (16 until 24).toList
  }

  it should "start from the viewport's own topLine/topVisualLine, not always the document start" in {
    val buffer = bufferOfLines(count = 30, visibleLines = 8, topLine = 16, topVisualLine = 0)

    val chunks = TextLayoutSnapshot.columnChunksForBuffer(
      buffer,
      columnWidthPx = 2000,
      font = font,
      cellMetricsOverride = Some(CellMetrics.cellUnit),
      forceCellLayout = true,
      columnCount = 2
    )

    chunks(0).map(_.bufferLine) shouldBe (16 until 24).toList
    chunks(1).map(_.bufferLine) shouldBe (24 until 30).toList
  }

  it should "produce a shorter final chunk when the document runs out of lines before filling it" in {
    val buffer = bufferOfLines(count = 20, visibleLines = 8)

    val chunks = TextLayoutSnapshot.columnChunksForBuffer(
      buffer,
      columnWidthPx = 2000,
      font = font,
      cellMetricsOverride = Some(CellMetrics.cellUnit),
      forceCellLayout = true,
      columnCount = 3
    )

    chunks.map(_.length) shouldBe List(8, 8, 4)
  }

  it should "default to a single column matching the existing single-window slice" in {
    val buffer = bufferOfLines(count = 30, visibleLines = 8)

    val chunks = TextLayoutSnapshot.columnChunksForBuffer(
      buffer,
      columnWidthPx = 2000,
      font = font,
      cellMetricsOverride = Some(CellMetrics.cellUnit),
      forceCellLayout = true
    )

    chunks.length shouldBe 1
    chunks.head.map(_.bufferLine) shouldBe (0 until 8).toList
  }

  behavior of "TextLayoutSnapshot.fromBufferColumn"

  it should "produce a snapshot showing only the active column's chunk of visual lines" in {
    val buffer = bufferOfLines(count = 30, visibleLines = 8)

    val snapshot = TextLayoutSnapshot.fromBufferColumn(
      buffer,
      columnWidthPx = 2000,
      font = font,
      cellMetricsOverride = Some(CellMetrics.cellUnit),
      forceCellLayout = true
    )

    snapshot.visualLines.map(_.bufferLine) shouldBe (0 until 8).toList
    snapshot.panelWidthPx shouldBe 2000
  }

  it should "start from the viewport's own topLine/topVisualLine, matching columnChunksForBuffer's first chunk" in {
    val buffer = bufferOfLines(count = 30, visibleLines = 8, topLine = 16, topVisualLine = 0)

    val snapshot = TextLayoutSnapshot.fromBufferColumn(
      buffer,
      columnWidthPx = 2000,
      font = font,
      cellMetricsOverride = Some(CellMetrics.cellUnit),
      forceCellLayout = true
    )

    snapshot.visualLines.map(_.bufferLine) shouldBe (16 until 24).toList
  }

  it should "produce a shorter final chunk when the document runs out of lines" in {
    val buffer = bufferOfLines(count = 20, visibleLines = 8, topLine = 16)

    val snapshot = TextLayoutSnapshot.fromBufferColumn(
      buffer,
      columnWidthPx = 2000,
      font = font,
      cellMetricsOverride = Some(CellMetrics.cellUnit),
      forceCellLayout = true
    )

    snapshot.visualLines.map(_.bufferLine) shouldBe (16 until 20).toList
  }
