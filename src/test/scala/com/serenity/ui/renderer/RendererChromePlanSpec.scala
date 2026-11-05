package com.serenity.ui.renderer

import com.serenity.TestWorkspaceTrees
import com.serenity.state.models.*
import com.serenity.ui.layout.{EditorLayoutContract, LayoutEngine, LayoutRect, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererChromePlanSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val activePane = PaneId(0)
  private val otherPane  = PaneId(1)
  private val bufferId   = BufferId(1)

  private val viewport = ViewportSize(120, 40)

  private val state: AppState =
    val buffer  = Buffer.fromString(bufferId, "one\ntwo\nthree")
    val initial = AppState.initial
    initial.copy(persisted =
      initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = initial.persisted.layout.copy(
          editorPanes = Map(activePane -> EditorPane.withBuffer(activePane, buffer.id)),
          activeEditorPaneId = Some(activePane),
          workspaceTree = Some(TestWorkspaceTrees.linear(activePane))
        )
      )
    )

  private val contract = EditorLayoutContract.from(state, viewport, LayoutEngine.calculateLayout(state, viewport))

  private val dirtyRows = Map(activePane -> Set(4, 5, 6))

  private def signals(
    geometryChanged: Boolean = false,
    rowsOverflow: Boolean = false,
    surfaceOverChrome: Boolean = false,
    lastRowMoved: Set[Int] = Set.empty,
    staleSlack: Set[PaneId] = Set.empty
  ): RendererChromePlan.Signals =
    RendererChromePlan.Signals(geometryChanged, rowsOverflow, surfaceOverChrome, lastRowMoved, staleSlack)

  private def repaint(
    damage: Damage,
    geometryChanged: Boolean = false,
    rowsOverflow: Boolean = false,
    contract: EditorLayoutContract = contract
  ): ChromeRepaint =
    RendererChromePlan.repaintFor(contract, damage, dirtyRows, signals(geometryChanged, rowsOverflow))

  "RendererChromePlan.repaintFor" should "repaint no chrome but the dirty rows' line numbers when nothing chrome-like changed" in {
    val plan = repaint(Damage.BufferRows(bufferId, Set(5)))

    plan.spacers shouldBe false
    plan.statusRow shouldBe false
    plan.headers.includes(activePane) shouldBe false
    plan.lineNumberRows.includes(5) shouldBe true
    plan.lineNumberRows.includes(20) shouldBe false
  }

  it should "repaint the status row only for Chrome damage" in {
    repaint(Damage.Chrome).statusRow shouldBe true
    repaint(Damage.Chrome).headers.includes(activePane) shouldBe false
  }

  it should "repaint only the headers PaneChrome damage names" in {
    val plan = repaint(Damage.PaneChrome(otherPane))

    plan.headers.includes(otherPane) shouldBe true
    plan.headers.includes(activePane) shouldBe false
    plan.statusRow shouldBe false
  }

  it should "repaint only what is named when a surface changes away from the chrome" in {
    repaint(Damage.Surface(SurfaceId("tab-bar"))).spacers shouldBe false
    repaint(Damage.Surface(SurfaceId("tab-bar"))).statusRow shouldBe false
  }

  it should "repaint everything on Everything, on surface damage over the chrome and when the rects moved" in {
    repaint(Damage.Everything) shouldBe ChromeRepaint.Whole
    RendererChromePlan.repaintFor(
      contract,
      Damage.Surface(SurfaceId("outline")),
      dirtyRows,
      signals(surfaceOverChrome = true)
    ) shouldBe ChromeRepaint.Whole
    repaint(Damage.Nothing, geometryChanged = true) shouldBe ChromeRepaint.Whole
  }

  it should "keep the room below a pane's last row unless the pane's room is stale or the chrome repaints whole" in {
    repaint(Damage.BufferRows(bufferId, Set(5))).slack.includes(activePane) shouldBe false

    val stale = RendererChromePlan.repaintFor(
      contract,
      Damage.BufferRows(bufferId, Set(5)),
      dirtyRows,
      signals(staleSlack = Set(activePane))
    )
    stale.slack.includes(activePane) shouldBe true
    stale.slack.includes(otherPane) shouldBe false

    repaint(Damage.Everything).slack.includes(activePane) shouldBe true
    repaint(Damage.Nothing, geometryChanged = true).slack.includes(activePane) shouldBe true
  }

  it should "repaint the last row's counter too when the pane's row count changed" in {
    val plan = RendererChromePlan.repaintFor(contract, Damage.Nothing, dirtyRows, signals(lastRowMoved = Set(30)))

    plan.lineNumberRows.includes(30) shouldBe true
    plan.lineNumberRows.includes(29) shouldBe false
  }

  it should "repaint every line number when the active pane has no row records to pair them with" in {
    val plan = RendererChromePlan.repaintFor(contract, Damage.Nothing, Map.empty, signals())

    plan.lineNumberRows.everything shouldBe true
    plan.lineNumberRows.includes(99) shouldBe true
  }

  it should "repaint the spacers when row ink may have reached them, and every line number beside a right counter" in {
    repaint(Damage.Nothing, rowsOverflow = true).spacers shouldBe true
    repaint(Damage.Nothing, rowsOverflow = true).lineNumberRows.includes(99) shouldBe false

    val withRightCounter =
      contract.copy(workspace = contract.workspace.copy(rightLineNumberRect = Some(LayoutRect(100, 2, 5, 30))))
    repaint(Damage.Nothing, rowsOverflow = true, contract = withRightCounter).lineNumberRows.everything shouldBe true
  }

  "RendererFrameState.chromeGeometryChanged" should "report a change only when the chrome's rects differ from the last frame's" in {
    val frameState = RendererFrameState(64)
    val key        = SurfaceContentIdentity(new Object)
    val geometry =
      ChromeGeometry(List(LayoutRect(0, 0, 1, 10)), None, List(LayoutRect(1, 1, 4, 9)), Map.empty)

    frameState.chromeGeometryChanged(key, geometry) shouldBe true
    frameState.chromeGeometryChanged(key, geometry) shouldBe false
    frameState.chromeGeometryChanged(key, geometry.copy(lineNumbers = List(LayoutRect(1, 1, 5, 9)))) shouldBe true
  }
