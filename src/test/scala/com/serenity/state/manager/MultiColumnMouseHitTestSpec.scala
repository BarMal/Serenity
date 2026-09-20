package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.MouseClick
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Multi-column e-reader layout (issue #1338, Phase 2 / slice 5): mouse hit-testing must land a click in the column its
  * pixel x actually falls into and resolve the buffer position that column visibly shows -- not column 0's. Runs in TUI
  * mode so the shared scene is built on `CellMetrics.cellUnit` (1px == 1 cell), making the click coordinates line up
  * exactly with the placements' cell offsets and each column snapshot's own cell-unit caret stops.
  */
class MultiColumnMouseHitTestSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId       = PaneId(0)
  private val bufferId     = BufferId(1)
  private val viewportSize = ViewportSize(200, 24)

  private def columnConfig: AppConfig =
    AppConfig.default.withoutStatusLine
      .withLineNumbers(false)
      .withWordWrap(true)
      .withColumnMode(true)
      .withColumnTargetWidth(20)
      .withColumnGap(2)

  private def stateWith(config: AppConfig): AppState =
    // Abundant short numbered lines: one visual row each, so every fitted column fills and its first buffer line is
    // predictable. Distinct per line so a resolved cursor unambiguously names which column it came from.
    val content = (0 until 2000).map(i => f"L$i%04d").mkString("\n")
    val buffer  = Buffer.fromString(bufferId, content)
    val base    = AppState.initial
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = config
      ),
      runtime = base.runtime.copy(isTuiMode = true, viewportSize = Some(viewportSize))
    )

  private def targeting(state: AppState): EditorMouseTargeting =
    val stateRef = Ref.unsafe[IO, AppState](state)
    val cacheRef = Ref.unsafe[IO, Option[MouseTargetCache]](None)
    new EditorMouseTargeting(EditorMouseTargetingPort(stateRef, cacheRef))

  private def sceneOf(state: AppState) =
    AuthoritativeUiScene.forState(state, viewportSize)

  "resolveMouseTarget, in column mode" should "land a click in column 0 at that column's shown position" in {
    val state      = stateWith(columnConfig)
    val scene      = sceneOf(state)
    val placements = scene.columnSnapshotsFor(paneId)
    assert(placements.length > 1, s"test setup expected a multi-column page, got ${placements.length} columns")
    val contentRect = scene.paneLayouts(paneId).contentRect

    val column0 = placements.head
    // Row 1 of the content area, a few cells into column 0's own band.
    val visualRow = 1
    val localX    = 3
    val clickRow  = contentRect.y + visualRow
    val clickCol  = contentRect.x + column0.xOffsetCells + localX
    val expected  = column0.snapshot.cursorForVisualRowAndXPx(visualRow, localX.toFloat)

    val resolved = targeting(state).resolveMouseTarget(MouseClick(clickCol, clickRow), state).unsafeRunSync()

    resolved.map { case (pid, buf, cursor) => (pid, buf.id, cursor) } shouldBe
      expected.map(cursor => (paneId, bufferId, cursor))
  }

  it should "land a click in column 1 at the position column 1 shows, not column 0's" in {
    val state      = stateWith(columnConfig)
    val scene      = sceneOf(state)
    val placements = scene.columnSnapshotsFor(paneId)
    assert(placements.length > 1, s"test setup expected a multi-column page, got ${placements.length} columns")
    val contentRect = scene.paneLayouts(paneId).contentRect

    val column1   = placements(1)
    val column0   = placements.head
    val visualRow = 0
    val localX    = 2
    val clickRow  = contentRect.y + visualRow
    val clickCol  = contentRect.x + column1.xOffsetCells + localX

    val expected = column1.snapshot.cursorForVisualRowAndXPx(visualRow, localX.toFloat)
    // Column 1's first buffer line is strictly past column 0's -- the whole point of the fix.
    val column0Position = column0.snapshot.cursorForVisualRowAndXPx(visualRow, localX.toFloat)

    val resolved = targeting(state).resolveMouseTarget(MouseClick(clickCol, clickRow), state).unsafeRunSync()

    resolved.map { case (_, _, cursor) => cursor } shouldBe expected
    resolved.map(_._3.line) should not be column0Position.map(_.line)
  }

  it should "land a click in column 2 at the position column 2 shows" in {
    val state      = stateWith(columnConfig)
    val scene      = sceneOf(state)
    val placements = scene.columnSnapshotsFor(paneId)
    assert(placements.length > 2, s"test setup expected at least 3 columns, got ${placements.length}")
    val contentRect = scene.paneLayouts(paneId).contentRect

    val column2   = placements(2)
    val visualRow = 3
    val localX    = 1
    val clickRow  = contentRect.y + visualRow
    val clickCol  = contentRect.x + column2.xOffsetCells + localX

    val expected = column2.snapshot.cursorForVisualRowAndXPx(visualRow, localX.toFloat)

    val resolved = targeting(state).resolveMouseTarget(MouseClick(clickCol, clickRow), state).unsafeRunSync()

    resolved.map { case (_, _, cursor) => cursor } shouldBe expected
  }

  it should "account for each column's line-number rail when line numbers are on (slice 2 + 5)" in {
    // With line numbers on, slice 2 reserves a per-column rail: the text starts `gutterWidthCells` past the band's
    // left edge. A click `localX` into the TEXT must resolve to that text position, not `gutterWidthCells + localX`.
    val state      = stateWith(columnConfig.withLineNumbers(true))
    val scene      = sceneOf(state)
    val placements = scene.columnSnapshotsFor(paneId)
    assert(placements.length > 1, s"test setup expected a multi-column page, got ${placements.length} columns")
    val contentRect = scene.paneLayouts(paneId).contentRect

    val column1 = placements(1)
    assert(column1.gutterWidthCells > 0, "test setup expected a non-zero line-number rail on the column")
    val visualRow = 0
    val localX    = 2
    val clickRow  = contentRect.y + visualRow
    // Click past column 1's rail, `localX` cells into its text.
    val clickCol = contentRect.x + column1.xOffsetCells + column1.gutterWidthCells + localX

    val expected = column1.snapshot.cursorForVisualRowAndXPx(visualRow, localX.toFloat)

    val resolved = targeting(state).resolveMouseTarget(MouseClick(clickCol, clickRow), state).unsafeRunSync()

    resolved.map { case (_, _, cursor) => cursor } shouldBe expected
  }

  it should "resolve a click in the inter-column gap to the nearer column" in {
    val state      = stateWith(columnConfig)
    val scene      = sceneOf(state)
    val placements = scene.columnSnapshotsFor(paneId)
    assert(placements.length > 1, s"test setup expected a multi-column page, got ${placements.length} columns")
    val contentRect = scene.paneLayouts(paneId).contentRect

    val left     = placements.head
    val right    = placements(1)
    val gapStart = left.xOffsetCells + left.columnWidthCells
    val gapEnd   = right.xOffsetCells
    assert(gapEnd > gapStart, s"expected a real gap between columns, got [$gapStart, $gapEnd)")

    val visualRow = 0

    // A gap cell adjacent to the left column's right edge is nearer the left column: it clamps to the left
    // column's own last-column position (localX == columnWidthCells).
    val nearLeftCol    = contentRect.x + gapStart
    val nearLeftExpect = left.snapshot.cursorForVisualRowAndXPx(visualRow, left.columnWidthCells.toFloat)
    val nearLeft =
      targeting(state).resolveMouseTarget(MouseClick(nearLeftCol, contentRect.y + visualRow), state).unsafeRunSync()
    nearLeft.map(_._3) shouldBe nearLeftExpect

    // A gap cell adjacent to the right column's left edge is nearer the right column: it clamps to the right
    // column's own start (localX == 0).
    val nearRightCol    = contentRect.x + gapEnd - 1
    val nearRightExpect = right.snapshot.cursorForVisualRowAndXPx(visualRow, 0.0f)
    val nearRight =
      targeting(state).resolveMouseTarget(MouseClick(nearRightCol, contentRect.y + visualRow), state).unsafeRunSync()
    nearRight.map(_._3) shouldBe nearRightExpect
  }

  "resolveMouseTarget, with column mode off" should "resolve against the single snapshot unchanged" in {
    val state = stateWith(
      AppConfig.default.withoutStatusLine.withLineNumbers(false).withWordWrap(true).withColumnMode(false)
    )
    val scene = sceneOf(state)
    scene.columnSnapshotsFor(paneId) shouldBe empty
    val contentRect = scene.paneLayouts(paneId).contentRect
    val snapshot    = scene.textSnapshot(paneId).getOrElse(fail("expected a single text snapshot"))

    val visualRow = 2
    val localX    = 4
    val clickRow  = contentRect.y + visualRow
    val clickCol  = contentRect.x + localX

    // Column mode off: the single-snapshot path stands. `cellWidthPx` is the snapshot's panel width per content cell.
    val cellWidthPx =
      if contentRect.width > 0 then snapshot.panelWidthPx.toFloat / contentRect.width.toFloat else 1.0f
    val expected = snapshot.cursorForVisualRowAndXPx(visualRow, (localX * cellWidthPx).max(0.0f))

    val resolved = targeting(state).resolveMouseTarget(MouseClick(clickCol, clickRow), state).unsafeRunSync()

    resolved.map(_._3) shouldBe expected
  }
