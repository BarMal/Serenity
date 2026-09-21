package com.serenity.state.manager

import cats.effect.IO
import cats.effect.Ref
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{MouseDrag, MousePress}
import com.serenity.rope.Balance
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
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

  /** Ten open buffers (`BufferId(0)`..`BufferId(9)`) in a 30-column viewport, with `BufferId(9)` focused --
    * `TabBarSurfaceComposition`'s overflow window (`maxVisibleCount`/`visibleWindow`) fits only 4 tabs at
    * `MinTabWidth`, so this overflows and, centred on the active tab (index 9), windows to entries [6,7,8,9]
    * (`BufferId(6)`..`BufferId(9)`) -- what's actually painted. An `activeBufferId = None` window instead centres on
    * index 0, windowing to entries [0,1,2,3] (`BufferId(0)`..`BufferId(3)`).
    */
  private def overflowingState: AppState =
    val base         = AppState.initial
    val extraBuffers = (1 to 9).map(i => BufferId(i) -> Buffer.fromString(BufferId(i), s"buffer$i"))
    val withBuffers = base.copy(
      runtime = base.runtime.copy(viewportSize = Some(ViewportSize(30, 10))),
      persisted = base.persisted.copy(
        buffers = base.persisted.buffers ++ extraBuffers,
        bufferOrder = base.persisted.bufferOrder ++ (1 to 9).map(BufferId.apply)
      )
    )
    EditorState.switchToBuffer(withBuffers, BufferId(9))

  private def fixture(state: AppState) =
    val stateRef = Ref.of[IO, AppState](state).unsafeRunSync()
    val port = TabBarDragHitTestingPort(
      stateRef = stateRef,
      validateAndUpdateState = (updated, _) => stateRef.set(updated)
    )
    (new TabBarDragHitTesting(port), stateRef)

  "handleTabBarPress" should
    "start a drag session on the tab actually painted under the cursor, not the index-0-centred window" in {
      val state                      = overflowingState
      val (dragHitTesting, stateRef) = fixture(state)

      // Column 14 lands on the third visible tab slot ([12,16)): BufferId(8) in the active-tab-centred window
      // ([6,7,8,9]) that is actually painted; BufferId(2) in the index-0-centred window ([0,1,2,3]) that
      // `activeBufferId = None` would wrongly produce.
      val started = dragHitTesting.handleTabBarPress(MousePress(col = 14, row = 0), state).unsafeRunSync()

      started shouldBe true
      stateRef.get.unsafeRunSync().runtime.tabDragSession shouldBe Some(TabDragSession(BufferId(8)))
    }

  "handleTabBarDrag" should
    "reorder onto the tab actually painted under the cursor, not the index-0-centred window" in {
      val started                    = overflowingState
      val (dragHitTesting, stateRef) = fixture(started)

      dragHitTesting.handleTabBarPress(MousePress(col = 2, row = 0), started).unsafeRunSync()
      val afterPress = stateRef.get.unsafeRunSync()
      afterPress.runtime.tabDragSession shouldBe Some(TabDragSession(BufferId(6)))

      dragHitTesting.handleTabBarDrag(MouseDrag(col = 14, row = 0), afterPress).unsafeRunSync()

      stateRef.get.unsafeRunSync().persisted.bufferOrder shouldBe
        EditorState.reorderBuffer(afterPress, BufferId(6), BufferId(8)).persisted.bufferOrder
    }
