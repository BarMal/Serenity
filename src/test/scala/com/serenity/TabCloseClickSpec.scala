package com.serenity

import java.nio.file.Paths

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** #1673: a click on a tab's close (x) affordance runs the same close workflow as keyboard `CloseTab` -- a clean buffer
  * closes straight away, a buffer with unsaved changes raises the save prompt first -- rather than dropping the buffer
  * through the reducer directly.
  */
class TabCloseClickSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(80, 24)

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("TabCloseClickSpec"))
    StateManager.apply(logger).unsafeRunSync()

  /** Two file-backed, unmodified tabs, with `first` shown in the active pane. */
  private def withTwoTabs(sm: StateManager): (BufferId, BufferId) =
    val first  = sm.bufferManager.createBuffer("first", Some(Paths.get("first.txt"))).unsafeRunSync()
    val second = sm.bufferManager.createBuffer("second", Some(Paths.get("second.txt"))).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), first).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    (first, second)

  private def markDirty(sm: StateManager, bufferId: BufferId): Unit =
    sm.updateState { state =>
      val buffer = state.persisted.buffers(bufferId)
      state.copy(persisted =
        state.persisted.copy(buffers =
          state.persisted.buffers.updated(bufferId, buffer.copy(document = buffer.document.copy(isDirty = true)))
        )
      )
    }.unsafeRunSync()

  private def closeAffordancePoint(state: AppState, bufferId: BufferId): (Int, Int) =
    val (entries, activeBufferId) = state.tabBarSurface.map(_.content) match
      case Some(SurfaceContent.TabBar(entries, activeBufferId)) => (entries, activeBufferId)
      case other                                                => fail(s"Expected a tab bar, got $other")
    val rect = LayoutEngine.calculateLayoutWithUI(state, viewport).tabBarRect.getOrElse(fail("Expected a tab bar rect"))
    val region = TabBarSurfaceComposition
      .closeAffordances(entries, activeBufferId, rect)
      .find(region => TabBarSurfaceComposition.closeBufferIdOf(region.focusId).contains(bufferId))
      .getOrElse(fail(s"Expected a close affordance for $bufferId"))
    (region.rect.x.toInt, region.rect.y.toInt)

  private def clickClose(sm: StateManager, bufferId: BufferId): Unit =
    val (col, row) = closeAffordancePoint(sm.getCurrentState.unsafeRunSync(), bufferId)
    sm.applyEvent(MouseClick(col, row)).unsafeRunSync()

  private def closeWorkflow(state: AppState): Option[CloseWorkflowState] =
    state.topModal.flatMap {
      _.modal match
        case Modal.CloseWorkflow(workflow) => Some(workflow)
        case _                             => None
    }

  "Clicking a tab's close affordance" should "close a tab without unsaved changes straight away" in {
    val sm              = makeStateManager()
    val (first, second) = withTwoTabs(sm)

    clickClose(sm, second)

    val after = sm.getCurrentState.unsafeRunSync()
    after.topModal shouldBe None
    after.persisted.buffers should not contain key(second)
    after.persisted.buffers should contain key first
  }

  it should "close the active tab the same way" in {
    val sm              = makeStateManager()
    val (first, second) = withTwoTabs(sm)

    clickClose(sm, first)

    val after = sm.getCurrentState.unsafeRunSync()
    after.topModal shouldBe None
    after.persisted.buffers should not contain key(first)
    after.persisted.buffers should contain key second
  }

  it should "raise the save prompt for a tab with unsaved changes instead of closing it" in {
    val sm          = makeStateManager()
    val (_, second) = withTwoTabs(sm)
    markDirty(sm, second)

    clickClose(sm, second)

    val after = sm.getCurrentState.unsafeRunSync()
    closeWorkflow(after).map(_.currentBufferId) shouldBe Some(second)
    after.persisted.buffers should contain key second
  }

  it should "keep the tab open when the save prompt is cancelled" in {
    val sm          = makeStateManager()
    val (_, second) = withTwoTabs(sm)
    markDirty(sm, second)
    clickClose(sm, second)

    val prompted = sm.getCurrentState.unsafeRunSync()
    val workflow = closeWorkflow(prompted).getOrElse(fail("Expected the close prompt"))
    val modal    = UiSceneSnapshot.from(prompted, viewport).modal.lastOption.getOrElse(fail("Expected a modal node"))
    val cancel = ModalSurfaceComposition
      .forModal(
        Modal.CloseWorkflow(workflow),
        modal.frameRect,
        SurfaceFrameLayout.minimumTargetRows(prompted.persisted.config.interfaceDensity)
      )
      .getOrElse(fail("Expected the close prompt composition"))
      .hitRegions
      .find(_.actionId.contains(SurfaceActionId("close-cancel")))
      .getOrElse(fail("Expected a cancel action"))
    sm.applyEvent(MouseClick(cancel.rect.x.toInt, cancel.rect.y.toInt)).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.topModal shouldBe None
    after.persisted.buffers should contain key second
    after.persisted.buffers(second).document.isDirty shouldBe true
  }
end TabCloseClickSpec
