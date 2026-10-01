package com.serenity.state.reducers

import com.serenity.DockedPanelFixtures
import com.serenity.keystroke.events.Direction
import com.serenity.rope.Balance
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.ui.layout.{DirectionalFocusLayout, FocusTarget, LayoutRect, PanelPosition, SplitAxis, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Alt+Arrow moves focus to whatever lies next in that direction on screen -- an editor pane or a docked panel alike --
  * so the keyboard follows the layout the user can see rather than a separate per-edge vocabulary.
  */
class DirectionalFocusSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val base =
    AppState.initial.copy(runtime = AppState.initial.runtime.copy(viewportSize = Some(ViewportSize(120, 40))))

  private val firstPane = base.persisted.layout.orderedPaneIds.headOption.getOrElse(fail("expected an editor pane"))

  private def focusedOn(state: AppState, focus: Focus): AppState =
    state.copy(persisted = state.persisted.copy(focus = focus))

  private def docked(state: AppState, id: PanelId, content: SurfaceContent, position: PanelPosition): AppState =
    DockedPanelFixtures.dock(state, id.surfaceId, content, position, 20)

  private def split(axis: SplitAxis): (AppState, PaneId) =
    val withSplit = focusedOn(
      EditorState.splitFocusedPane(focusedOn(base, Focus.EditorPane(firstPane)), axis),
      Focus.EditorPane(firstPane)
    )
    val second =
      withSplit.persisted.layout.orderedPaneIds.find(_ != firstPane).getOrElse(fail("expected a second pane"))
    (
      withSplit.copy(persisted =
        withSplit.persisted.copy(layout = withSplit.persisted.layout.copy(activeEditorPaneId = Some(firstPane)))
      ),
      second
    )

  "Moving focus" should "reach a docked panel beside the editor, and come back" in {
    val state = focusedOn(
      docked(base, PanelId.Outline, SurfaceContent.Outline(Nil), PanelPosition.Left),
      Focus.EditorPane(firstPane)
    )

    val onPanel = DirectionalFocus.moved(state, Direction.Left)
    onPanel.persisted.focus shouldBe Focus.Surface(PanelId.Outline.surfaceId)

    DirectionalFocus.moved(onPanel, Direction.Right).persisted.focus shouldBe Focus.EditorPane(firstPane)
  }

  it should "move between side-by-side editor panes, making the target the active pane" in {
    val (state, second) = split(SplitAxis.Horizontal)

    val moved = DirectionalFocus.moved(state, Direction.Right)

    moved.persisted.focus shouldBe Focus.EditorPane(second)
    moved.persisted.layout.activeEditorPaneId shouldBe Some(second)
    DirectionalFocus.moved(moved, Direction.Left).persisted.focus shouldBe Focus.EditorPane(firstPane)
  }

  it should "move between stacked editor panes" in {
    val (state, second) = split(SplitAxis.Vertical)

    val moved = DirectionalFocus.moved(state, Direction.Down)

    moved.persisted.focus shouldBe Focus.EditorPane(second)
    DirectionalFocus.moved(moved, Direction.Up).persisted.focus shouldBe Focus.EditorPane(firstPane)
  }

  it should "reach a panel docked along the bottom from the editor above it" in {
    val state = focusedOn(
      docked(base, PanelId.Diagnostics, SurfaceContent.Diagnostics(Nil), PanelPosition.Bottom),
      Focus.EditorPane(firstPane)
    )

    DirectionalFocus.moved(state, Direction.Down).persisted.focus shouldBe Focus.Surface(PanelId.Diagnostics.surfaceId)
  }

  it should "pass over the companion, which cannot take focus" in {
    val companionOnly = focusedOn(
      docked(base, PanelId.Companion, SurfaceContent.CompanionSprite, PanelPosition.Right),
      Focus.EditorPane(firstPane)
    )
    DirectionalFocus.moved(companionOnly, Direction.Right) shouldBe companionOnly

    val withOutline = docked(companionOnly, PanelId.Outline, SurfaceContent.Outline(Nil), PanelPosition.Right)
    DirectionalFocus.moved(withOutline, Direction.Right).persisted.focus shouldBe Focus.Surface(
      PanelId.Outline.surfaceId
    )
  }

  it should "leave focus alone when nothing lies in that direction" in {
    val state = focusedOn(base, Focus.EditorPane(firstPane))

    Direction.values.foreach(direction => DirectionalFocus.moved(state, direction) shouldBe state)
  }

  it should "leave focus alone while a panel is maximised" in {
    val withPanel = docked(base, PanelId.Outline, SurfaceContent.Outline(Nil), PanelPosition.Left)
    val state = focusedOn(
      DockedPanelFixtures.expand(withPanel, PanelId.Outline.surfaceId),
      Focus.Surface(PanelId.Outline.surfaceId)
    )

    DirectionalFocus.moved(state, Direction.Right) shouldBe state
  }

  it should "leave focus alone while a floating surface has it" in {
    val withPanel = docked(base, PanelId.Outline, SurfaceContent.Outline(Nil), PanelPosition.Left)
    val state     = focusedOn(withPanel, Focus.Surface(SurfaceId("floating-picker")))

    DirectionalFocus.moved(state, Direction.Left) shouldBe state
  }

  "The nearest neighbour" should "be the closest in that direction, then the best aligned" in {
    val from = LayoutRect(40, 10, 20, 10)
    val candidates = List(
      FocusTarget.Pane(PaneId(1))                  -> LayoutRect(0, 0, 10, 40),   // far left
      FocusTarget.Panel(SurfaceId("near-low"))     -> LayoutRect(20, 30, 15, 10), // near left, but below
      FocusTarget.Panel(SurfaceId("near-aligned")) -> LayoutRect(20, 10, 15, 10), // near left, level
      FocusTarget.Pane(PaneId(2))                  -> LayoutRect(70, 10, 10, 10)  // right
    )

    DirectionalFocusLayout.neighbour(from, candidates, Direction.Left) shouldBe
      Some(FocusTarget.Panel(SurfaceId("near-aligned")))
    DirectionalFocusLayout.neighbour(from, candidates, Direction.Right) shouldBe Some(FocusTarget.Pane(PaneId(2)))
    DirectionalFocusLayout.neighbour(from, candidates, Direction.Up) shouldBe None
  }
