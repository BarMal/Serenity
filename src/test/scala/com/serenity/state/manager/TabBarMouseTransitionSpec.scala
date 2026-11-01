package com.serenity.state.manager

import com.serenity.keystroke.events.{MouseClick, MouseDrag, MousePress}
import com.serenity.rope.Balance
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ReducerResult, Transition, WorkflowEffect}
import com.serenity.ui.layout.{LayoutEngine, SurfaceHitRegion, TabBarSurfaceComposition, ViewportSize}
import com.serenity.ui.renderer.TabBarLayoutSettings
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The tab strip's click and drag handlers as pure transitions over a constructed `AppState`. */
class TabBarMouseTransitionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  // Same strip as TabBarMouseHitTestingSpec: tab 0 is active and wears its close box, tab 1 shows its own while the
  // pointer is over it, and the new-tab affordance follows. Columns come from the layout the code under test uses.
  private def twoBufferState: AppState =
    val second = Buffer.fromString(BufferId(1), "second")
    val base   = AppState.initial(com.serenity.ChromeOn.config)
    base.copy(
      runtime = base.runtime.copy(viewportSize = Some(ViewportSize(60, 5))),
      persisted = base.persisted.copy(
        buffers = base.persisted.buffers + (second.id -> second),
        bufferOrder = base.persisted.bufferOrder :+ second.id
      )
    )

  private def hovering(state: AppState, tab: BufferId): AppState =
    state.copy(runtime = state.runtime.copy(pointerGesture = state.runtime.pointerGesture.copy(hoveredTab = Some(tab))))

  private def stripParts(state: AppState): (List[SurfaceHitRegion], List[SurfaceHitRegion], SurfaceHitRegion) =
    val rect = LayoutEngine
      .calculateLayoutWithUI(state, state.runtime.viewportSize.getOrElse(fail("expected a viewport")))
      .tabBarRect
      .getOrElse(fail("expected a tab bar"))
    val (entries, active) = state.tabBarSurface.map(_.content) match
      case Some(SurfaceContent.TabBar(listed, activeBufferId)) => (listed, activeBufferId)
      case _                                                   => fail("expected tab bar content")
    val settings = TabBarLayoutSettings.of(state)
    (
      TabBarSurfaceComposition.forTabBar(entries, active, rect, settings).hitRegions,
      TabBarSurfaceComposition.closeAffordances(entries, active, rect, settings),
      TabBarSurfaceComposition.newTabAffordance(entries, rect, active, settings).getOrElse(fail("expected new tab"))
    )

  private def tabCol(state: AppState, index: Int): Int = stripParts(state)._1(index).rect.x.toInt + 1

  private def closeCol(state: AppState, bufferId: BufferId): Int =
    stripParts(state)._2
      .find(region => TabBarSurfaceComposition.closeBufferIdOf(region.focusId).contains(bufferId))
      .map(_.rect.x.toInt)
      .getOrElse(fail(s"expected a close box for $bufferId"))

  private def run[A](state: AppState)(transition: Transition[A]): (ReducerResult, A) =
    MouseTransition.run(state)(transition)

  "TabBarMouseHitTesting.click" should "switch to a clicked background tab" in {
    val state = twoBufferState

    val (result, claimed) = run(state)(TabBarMouseHitTesting.click(MouseClick(tabCol(state, 1), 0), state))

    claimed shouldBe true
    result.effects shouldBe Nil
    result.state.focusedBufferId shouldBe Some(BufferId(1))
  }

  it should "open a new tab from the trailing affordance" in {
    val state = twoBufferState

    val (result, claimed) =
      run(state)(TabBarMouseHitTesting.click(MouseClick(stripParts(state)._3.rect.x.toInt + 1, 0), state))

    claimed shouldBe true
    result.state.persisted.bufferOrder should have size 3
  }

  it should "claim a click on the active tab without changing anything" in {
    val state = twoBufferState

    run(state)(TabBarMouseHitTesting.click(MouseClick(tabCol(state, 0), 0), state)) shouldBe ((
      ReducerResult(state, Nil),
      true
    ))
  }

  it should "decline a click below the strip" in {
    val state = twoBufferState

    run(state)(TabBarMouseHitTesting.click(MouseClick(3, 2), state)) shouldBe ((ReducerResult(state, Nil), false))
  }

  // #1673: the close affordance starts the same close workflow as keyboard CloseTab, which prompts before discarding
  // unsaved changes, rather than dropping the buffer itself.
  it should "start the close workflow for a background tab, keeping the active tab active" in {
    val state = hovering(twoBufferState, BufferId(1))

    val (result, claimed) = run(state)(TabBarMouseHitTesting.click(MouseClick(closeCol(state, BufferId(1)), 0), state))

    claimed shouldBe true
    result.state shouldBe theSameInstanceAs(state)
    result.effects shouldBe List(
      AppEffect.Workflow(WorkflowEffect.BeginClose(CloseScope.Tab(BufferId(1), returnTo = Some(BufferId(0)))))
    )
  }

  it should "close the active tab exactly as keyboard CloseTab does" in {
    val state = twoBufferState

    val (result, claimed) = run(state)(TabBarMouseHitTesting.click(MouseClick(closeCol(state, BufferId(0)), 0), state))

    claimed shouldBe true
    result.state shouldBe theSameInstanceAs(state)
    result.effects shouldBe List(AppEffect.Workflow(WorkflowEffect.BeginClose(CloseScope.Current)))
  }

  "TabBarDragHitTesting.press" should "start a drag session on the pressed tab" in {
    val state = twoBufferState

    val (result, claimed) = run(state)(TabBarDragHitTesting.press(MousePress(tabCol(state, 1), 0), state))

    claimed shouldBe true
    result.effects shouldBe Nil
    result.state.runtime.pointerGesture.tabDragSession shouldBe Some(TabDragSession(BufferId(1)))
  }

  it should "end any previous session on a press off the strip, without claiming it" in {
    val dragging =
      twoBufferState.copy(runtime =
        twoBufferState.runtime.copy(pointerGesture =
          twoBufferState.runtime.pointerGesture.copy(tabDragSession = Some(TabDragSession(BufferId(1))))
        )
      )

    val (result, claimed) = run(dragging)(TabBarDragHitTesting.press(MousePress(3, 3), dragging))

    claimed shouldBe false
    result.state.runtime.pointerGesture.tabDragSession shouldBe None
  }

  it should "leave the state untouched when there was no session to end" in {
    val state = twoBufferState

    run(state)(TabBarDragHitTesting.press(MousePress(3, 3), state))._1.state shouldBe theSameInstanceAs(state)
  }

  "TabBarDragHitTesting.drag" should "reorder the dragged tab onto the tab under the pointer" in {
    val dragging =
      twoBufferState.copy(runtime =
        twoBufferState.runtime.copy(pointerGesture =
          twoBufferState.runtime.pointerGesture.copy(tabDragSession = Some(TabDragSession(BufferId(0))))
        )
      )

    val (result, claimed) = run(dragging)(TabBarDragHitTesting.drag(MouseDrag(tabCol(dragging, 1), 0), dragging))

    claimed shouldBe true
    result.effects shouldBe Nil
    result.state.persisted.bufferOrder shouldBe
      EditorState.reorderBuffer(dragging, BufferId(0), BufferId(1)).persisted.bufferOrder
  }

  it should "decline a drag with no session" in {
    val state = twoBufferState

    run(state)(TabBarDragHitTesting.drag(MouseDrag(tabCol(state, 1), 0), state)) shouldBe ((
      ReducerResult(state, Nil),
      false
    ))
  }
end TabBarMouseTransitionSpec
