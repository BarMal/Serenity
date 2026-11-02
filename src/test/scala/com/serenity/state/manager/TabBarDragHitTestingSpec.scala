package com.serenity.state.manager

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.keystroke.events.{MouseDrag, MousePress}
import com.serenity.rope.Balance
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutEngine, TabBarSurfaceComposition, ViewportSize}
import com.serenity.ui.renderer.TabBarLayoutSettings
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for `TabBarDragHitTesting` -- resolving a drag gesture's cell coordinate against the tab strip's actual
  * painted geometry (issue #1079: drag-to-reorder). Once the strip overflows (`TabBarSurfaceComposition.visibleWindow`)
  * and the active tab has scrolled away from index 0, the visible window is centred on the active tab -- so hit-testing
  * must resolve against that same active-tab-centred window, not the index-0-centred window `activeBufferId = None`
  * would produce.
  */
class TabBarDragHitTestingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  /** Ten open buffers (`BufferId(0)`..`BufferId(9)`) in a 46-column viewport, with `BufferId(9)` focused. Ten tabs do
    * not fit 46 columns, so the strip overflows and its window is centred on the active tab (index 9), which scrolls
    * the first tabs out: what is painted is the tabs `paintedTabs` lists. An `activeBufferId = None` window would
    * centre on index 0 instead and show the first tabs.
    */
  private def overflowingState: AppState =
    val base         = AppState.initial(com.serenity.ChromeOn.config)
    val extraBuffers = (1 to 9).map(i => BufferId(i) -> Buffer.fromString(BufferId(i), s"buffer$i"))
    val withBuffers = base.copy(
      runtime = base.runtime.copy(viewportSize = Some(ViewportSize(46, 10))),
      persisted = base.persisted.copy(
        buffers = base.persisted.buffers ++ extraBuffers,
        bufferOrder = base.persisted.bufferOrder ++ (1 to 9).map(BufferId.apply)
      )
    )
    EditorState.switchToBuffer(withBuffers, BufferId(9))

  /** The tabs the strip paints for `state`, left to right, with a column inside each. */
  private def paintedTabs(state: AppState): List[(BufferId, Int)] =
    val viewport = state.runtime.viewportSize.getOrElse(fail("expected a viewport"))
    val rect     = LayoutEngine.calculateLayoutWithUI(state, viewport).tabBarRect.getOrElse(fail("expected a tab bar"))
    val (entries, active) = state.tabBarSurface.map(_.content) match
      case Some(SurfaceContent.TabBar(listed, activeBufferId)) => (listed, activeBufferId)
      case _                                                   => fail("expected tab bar content")
    TabBarSurfaceComposition
      .forTabBar(entries, active, rect, TabBarLayoutSettings.of(state))
      .hitRegions
      .flatMap(region => TabBarSurfaceComposition.bufferIdOf(region.focusId).map(_ -> (region.rect.x.toInt + 1)))

  private def fixture(state: AppState) =
    val stateRef = Ref.of[IO, AppState](state).unsafeRunSync()
    val port = TabBarDragHitTestingPort(
      currentState = stateRef.get,
      applyReducerResult = (result, _) => stateRef.set(result.state)
    )
    (new TabBarDragHitTesting(port), stateRef)

  "the overflowing fixture" should "paint a window centred on the active tab, which hides the first tabs" in {
    val painted = paintedTabs(overflowingState).map(_._1)

    painted should contain(BufferId(9))
    painted should not contain BufferId(0)
    painted.size should be >= 2
  }

  "handleTabBarPress" should
    "start a drag session on the tab actually painted under the cursor, not the index-0-centred window" in {
      val state                      = overflowingState
      val (dragHitTesting, stateRef) = fixture(state)
      val (painted, col)             = paintedTabs(state).headOption.getOrElse(fail("expected painted tabs"))

      val started = dragHitTesting.handleTabBarPress(MousePress(col = col, row = 0), state).unsafeRunSync()

      started shouldBe true
      painted should not be BufferId(0)
      stateRef.get.unsafeRunSync().runtime.pointerGesture.tabDragSession shouldBe Some(TabDragSession(painted))
    }

  "handleTabBarDrag" should
    "reorder onto the tab actually painted under the cursor, not the index-0-centred window" in {
      val started                    = overflowingState
      val (dragHitTesting, stateRef) = fixture(started)
      val tabs                       = paintedTabs(started)
      val (first, firstCol)          = tabs.headOption.getOrElse(fail("expected painted tabs"))
      val (target, targetCol)        = tabs.lastOption.getOrElse(fail("expected painted tabs"))

      dragHitTesting.handleTabBarPress(MousePress(col = firstCol, row = 0), started).unsafeRunSync()
      val afterPress = stateRef.get.unsafeRunSync()
      afterPress.runtime.pointerGesture.tabDragSession shouldBe Some(TabDragSession(first))

      dragHitTesting.handleTabBarDrag(MouseDrag(col = targetCol, row = 0), afterPress).unsafeRunSync()

      stateRef.get.unsafeRunSync().persisted.bufferOrder shouldBe
        EditorState.reorderBuffer(afterPress, first, target).persisted.bufferOrder
    }
