package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** #1673: a click on a tab's close (x) affordance runs the same close workflow as keyboard `CloseTab` -- a clean buffer
  * closes straight away, a buffer with unsaved changes raises the save prompt first -- rather than dropping the buffer
  * through the reducer directly. Closing a background tab leaves the tab the user was on active; while its save prompt
  * is up, the tab being closed is shown so the user can see what they are saving, and the original tab comes back once
  * the prompt is resolved either way.
  */
class TabCloseClickSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(80, 24)

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("TabCloseClickSpec"))
    StateManager.apply(logger).unsafeRunSync()

  /** Two file-backed, unmodified tabs under `root`, with `first` shown in the active pane. */
  private def withTwoTabs(sm: StateManager, root: Path): (BufferId, BufferId) =
    val first  = sm.bufferManager.createBuffer("first", Some(root.resolve("first.txt"))).unsafeRunSync()
    val second = sm.bufferManager.createBuffer("second", Some(root.resolve("second.txt"))).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), first).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    (first, second)

  private def withTempRoot(test: Path => Unit): Unit =
    val root = Files.createTempDirectory("tab-close-click")
    try test(root)
    finally
      Files.list(root).forEach(path => Files.deleteIfExists(path): Unit)
      Files.deleteIfExists(root): Unit

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

  private def clickPromptChoice(sm: StateManager, actionId: String): Unit =
    val prompted = sm.getCurrentState.unsafeRunSync()
    val workflow = closeWorkflow(prompted).getOrElse(fail("Expected the close prompt"))
    val modal    = UiSceneSnapshot.from(prompted, viewport).modal.lastOption.getOrElse(fail("Expected a modal node"))
    val choice = ModalSurfaceComposition
      .forModal(
        Modal.CloseWorkflow(workflow),
        modal.frameRect,
        SurfaceFrameLayout.minimumTargetRows(prompted.persisted.config.interfaceDensity)
      )
      .getOrElse(fail("Expected the close prompt composition"))
      .hitRegions
      .find(_.actionId.contains(SurfaceActionId(actionId)))
      .getOrElse(fail(s"Expected a $actionId action"))
    sm.applyEvent(MouseClick(choice.rect.x.toInt, choice.rect.y.toInt)).unsafeRunSync()

  private def activeBufferId(state: AppState): Option[BufferId] = state.activeBuffer.map(_.id)

  "Clicking a background tab's close affordance" should "close it and leave the active tab active" in withTempRoot {
    root =>
      val sm              = makeStateManager()
      val (first, second) = withTwoTabs(sm, root)

      clickClose(sm, second)

      val after = sm.getCurrentState.unsafeRunSync()
      after.topModal shouldBe None
      after.persisted.buffers should not contain key(second)
      activeBufferId(after) shouldBe Some(first)
      after.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  it should "raise the save prompt for unsaved changes, showing the tab it asks about" in withTempRoot { root =>
    val sm          = makeStateManager()
    val (_, second) = withTwoTabs(sm, root)
    markDirty(sm, second)

    clickClose(sm, second)

    val after = sm.getCurrentState.unsafeRunSync()
    closeWorkflow(after).map(_.currentBufferId) shouldBe Some(second)
    after.persisted.buffers should contain key second
    activeBufferId(after) shouldBe Some(second)
  }

  it should "keep everything as it was when the save prompt is cancelled" in withTempRoot { root =>
    val sm              = makeStateManager()
    val (first, second) = withTwoTabs(sm, root)
    markDirty(sm, second)
    clickClose(sm, second)

    clickPromptChoice(sm, "close-cancel")

    val after = sm.getCurrentState.unsafeRunSync()
    after.topModal shouldBe None
    after.persisted.buffers should contain key second
    after.persisted.buffers(second).document.isDirty shouldBe true
    activeBufferId(after) shouldBe Some(first)
  }

  it should "discard and close it, then return to the original tab" in withTempRoot { root =>
    val sm              = makeStateManager()
    val (first, second) = withTwoTabs(sm, root)
    markDirty(sm, second)
    clickClose(sm, second)

    clickPromptChoice(sm, "close-discard")

    val after = sm.getCurrentState.unsafeRunSync()
    after.topModal shouldBe None
    after.persisted.buffers should not contain key(second)
    activeBufferId(after) shouldBe Some(first)
  }

  it should "save and close it, then return to the original tab" in withTempRoot { root =>
    val sm              = makeStateManager()
    val (first, second) = withTwoTabs(sm, root)
    markDirty(sm, second)
    clickClose(sm, second)

    clickPromptChoice(sm, "close-save")

    val after = sm.getCurrentState.unsafeRunSync()
    after.topModal shouldBe None
    after.persisted.buffers should not contain key(second)
    activeBufferId(after) shouldBe Some(first)
    Files.readString(root.resolve("second.txt")) shouldBe "second"
  }

  it should "keep it, dirty, and return to the original tab when its save fails (#1708)" in withTempRoot { root =>
    val sm = makeStateManager()
    // A regular file where the tab's parent directory should be: the save cannot create the file under it.
    val blocker = Files.writeString(root.resolve("blocker"), "")
    val first   = sm.bufferManager.createBuffer("first", Some(root.resolve("first.txt"))).unsafeRunSync()
    val second  = sm.bufferManager.createBuffer("second", Some(blocker.resolve("second.txt"))).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), first).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    markDirty(sm, second)
    clickClose(sm, second)

    clickPromptChoice(sm, "close-save")

    val after = sm.getCurrentState.unsafeRunSync()
    after.topModal shouldBe None
    after.persisted.buffers should contain key second
    after.persisted.buffers(second).document.isDirty shouldBe true
    activeBufferId(after) shouldBe Some(first)
  }

  "Clicking the active tab's close affordance" should "close it the same way keyboard CloseTab does" in withTempRoot {
    root =>
      val sm              = makeStateManager()
      val (first, second) = withTwoTabs(sm, root)

      clickClose(sm, first)

      val after = sm.getCurrentState.unsafeRunSync()
      after.topModal shouldBe None
      after.persisted.buffers should not contain key(first)
      after.persisted.buffers should contain key second
  }

  it should "raise the save prompt for unsaved changes and keep the tab when cancelled" in withTempRoot { root =>
    val sm         = makeStateManager()
    val (first, _) = withTwoTabs(sm, root)
    markDirty(sm, first)

    clickClose(sm, first)
    closeWorkflow(sm.getCurrentState.unsafeRunSync()).map(_.currentBufferId) shouldBe Some(first)
    clickPromptChoice(sm, "close-cancel")

    val after = sm.getCurrentState.unsafeRunSync()
    after.topModal shouldBe None
    after.persisted.buffers should contain key first
    activeBufferId(after) shouldBe Some(first)
  }
end TabCloseClickSpec
