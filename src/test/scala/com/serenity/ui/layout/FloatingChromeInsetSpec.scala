package com.serenity.ui.layout

import com.serenity.TestWorkspaceTrees
import com.serenity.config.{AppConfig, DockedCornerStyle}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #2389: docked corner style `floating` leaves the element gap round the whole chrome (the window's edges, the
  * gap above the status bar, and below it), `flush` keeps today's layout. Every number is a count of cells.
  */
class FloatingChromeInsetSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val columns  = 100
  private val rows     = 30
  private val viewport = ViewportSize(columns, rows)
  private val bufferId = BufferId(1)
  private val paneId   = PaneId(0)

  private def configWith(style: DockedCornerStyle, gap: Option[Double]): AppConfig =
    AppConfig.conventional.withDockedCornerStyle(style).withUiElementGap(gap)

  private def stateWith(config: AppConfig, buffers: Int = 1, panels: Boolean = true): AppState =
    val ids  = (1 to buffers).map(BufferId(_)).toList
    val all  = ids.map(id => Buffer.fromString(id, "line\n" * 5))
    val base = AppState.initial(config)
    val state = base.copy(
      persisted = base.persisted.copy(
        buffers = all.map(b => b.id -> b).toMap,
        bufferOrder = ids,
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, ids.head)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        focus = Focus.EditorPane(paneId)
      ),
      runtime = base.runtime.copy(viewportSize = Some(viewport))
    )
    val docked =
      if panels then
        List(
          SessionDockedPanel("surface-a", SessionPinnedPanel(PanelPosition.Left, 22, SessionPanelContent.Outline(Nil))),
          SessionDockedPanel(
            "surface-b",
            SessionPinnedPanel(PanelPosition.Right, 22, SessionPanelContent.Diagnostics(Nil))
          )
        )
      else Nil
    UiPreset.applyToState(
      UiPreset("Docked", config, Some(Theme.light.name), dockedPanels = docked),
      state,
      Some(Theme.light)
    )

  private def layoutOf(state: AppState): CalculatedLayout = LayoutEngine.calculateLayoutWithUI(state, viewport)

  private val floating = configWith(DockedCornerStyle.Floating, Some(1.0))
  private val flush    = configWith(DockedCornerStyle.Flush, Some(1.0))

  "Floating chrome" should "leave a one-cell margin at the left, right and top of the window" in {
    val layout = layoutOf(stateWith(floating))
    val left   = layout.pinnedPanelRects(PanelPosition.Left)
    val right  = layout.pinnedPanelRects(PanelPosition.Right)

    left.x shouldBe 1
    left.y shouldBe 1
    right.right shouldBe columns - 1
  }

  it should "put the status bar one cell in from each side, one cell up from the bottom, with a gap above it" in {
    val state  = stateWith(floating)
    val layout = layoutOf(state)
    val status = layout.gutterRect.getOrElse(fail("expected a status bar"))
    val panel  = layout.pinnedPanelRects(PanelPosition.Left)

    status.x shouldBe 1
    status.right shouldBe columns - 1
    status.bottom shouldBe rows - 1
    panel.bottom shouldBe status.y - 1
  }

  it should "cost exactly two columns and three rows against flush, with a status bar and no tab strip" in {
    val flushLayout    = layoutOf(stateWith(flush))
    val floatingLayout = layoutOf(stateWith(floating))

    floatingLayout.editorPanelRect.width shouldBe flushLayout.editorPanelRect.width - 2
    floatingLayout.editorPanelRect.height shouldBe flushLayout.editorPanelRect.height - 3
  }

  it should "cost one row more when a tab strip shows, and put the strip inside the margin" in {
    val flushLayout    = layoutOf(stateWith(flush, buffers = 2))
    val floatingLayout = layoutOf(stateWith(floating, buffers = 2))
    val strip          = floatingLayout.tabBarRect.getOrElse(fail("expected a tab strip"))

    strip shouldBe LayoutRect(1, 1, columns - 2, 1)
    floatingLayout.editorPanelRect.height shouldBe flushLayout.editorPanelRect.height - 4
    floatingLayout.pinnedPanelRects(PanelPosition.Left).y shouldBe strip.bottom + 1
  }

  it should "keep every rectangle inside the margin" in {
    val state  = stateWith(floating, buffers = 2)
    val layout = layoutOf(state)
    val inside = LayoutRect(1, 1, columns - 2, rows - 2)

    val rects =
      layout.pinnedPanelRects.values.toList ++ layout.gutterRect ++ layout.tabBarRect :+ layout.editorPanelRect
    rects.foreach(rect => withClue(s"$rect: ")(inside.containsRect(rect) shouldBe true))
  }

  it should "leave the margin and the gap above the status bar unclaimed by any element" in {
    val layout = layoutOf(stateWith(floating))
    val status = layout.gutterRect.getOrElse(fail("expected a status bar"))
    val rects  = layout.pinnedPanelRects.values.toList ++ layout.gutterRect :+ layout.editorPanelRect

    List((0, 0), (columns - 1, 0), (0, rows - 1), (columns - 1, rows - 1), (50, 0), (0, 15), (50, status.y - 1))
      .foreach { case (x, y) => withClue(s"cell ($x,$y): ")(rects.exists(_.contains(x, y)) shouldBe false) }
  }

  it should "add no inset when the element gap is zero" in {
    val zeroFloating = layoutOf(stateWith(configWith(DockedCornerStyle.Floating, Some(0.0))))
    val zeroFlush    = layoutOf(stateWith(configWith(DockedCornerStyle.Flush, Some(0.0))))

    zeroFloating shouldBe zeroFlush
  }

  "Flush chrome" should "reach the window's edges: panels at the corners and the status bar across the full width" in {
    val layout = layoutOf(stateWith(flush))
    val status = layout.gutterRect.getOrElse(fail("expected a status bar"))

    layout.pinnedPanelRects(PanelPosition.Left).x shouldBe 0
    layout.pinnedPanelRects(PanelPosition.Left).y shouldBe 0
    layout.pinnedPanelRects(PanelPosition.Right).right shouldBe columns
    status.x shouldBe 0
    status.width shouldBe columns
    status.bottom shouldBe rows
  }

  "A panel resize drag" should "land on the edge it was dragged to, flush or floating" in
    List(flush, floating).foreach { config =>
      val state = stateWith(config)
      val rect  = layoutOf(state).pinnedPanelRects(PanelPosition.Left)
      val drag  = LayoutEngine.pinnedPanelResizeFromDrag(state, viewport, rect.right - 1, rect.centerY)

      withClue(s"${config.dockedCornerStyle}: ")(drag.map(_.size) shouldBe Some(rect.width))
    }
