package com.serenity

import java.nio.file.Paths

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{MoveDown, NextTab, PreviousTab, ResizeEvent}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.state.reducers.PopupSurfaceReducer
import com.serenity.testkit.AwaitCondition.awaitValue
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.{DirectoryTreeData, PanelPosition, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Ctrl+Tab / Ctrl+Shift+Tab step from the buffer in the active editor pane, whatever surface has focus. */
class TabCycleOutsideEditorFocusSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val viewport   = ViewportSize(160, 32)
  private val explorerId = SurfaceId("explorer")
  private val root       = Paths.get("/repo")

  /** A state manager with four buffers in order and the third shown in the only pane. */
  private def fourBuffers(): (StateManager, Vector[BufferId]) =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("TabCycleOutsideEditorFocusSpec"))
    val sm     = StateManager.apply(logger, dictionaryCache = SharedDictionary.default).unsafeRunSync()
    (1 to 3).foreach(index => sm.createBuffer(s"buffer $index", None).unsafeRunSync())
    val order = sm.getCurrentState.unsafeRunSync().persisted.bufferOrder.toVector
    order should have size 4
    sm.setBufferForPane(PaneId(0), order(2)).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    (sm, order)

  private def focusDockedExplorer(sm: StateManager): Unit =
    val tree = DirectoryTreeData(root, entries = Map(root -> Nil))
    sm.updateState(state =>
      val docked = DockedPanelFixtures
        .dock(state, explorerId, SurfaceContent.DirectoryTree(tree, Some(root)), PanelPosition.Left, 28)
      docked.copy(persisted = docked.persisted.copy(focus = Focus.Surface(explorerId)))
    ).unsafeRunSync()

  private def shownIn(state: AppState, paneId: PaneId): Option[BufferId] =
    state.persisted.layout.editorPanes.get(paneId).flatMap(_.bufferId)

  "Ctrl+Tab from a focused pinned panel" should "show the buffer after the active pane's and focus that pane" in {
    val (sm, order) = fourBuffers()
    focusDockedExplorer(sm)

    sm.applyEvent(NextTab).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    shownIn(after, PaneId(0)) shouldBe Some(order(3))
    after.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  "Ctrl+Shift+Tab from a focused pinned panel" should "show the buffer before the active pane's and focus that pane" in {
    val (sm, order) = fourBuffers()
    focusDockedExplorer(sm)

    sm.applyEvent(PreviousTab).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    shownIn(after, PaneId(0)) shouldBe Some(order(1))
    after.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }

  "Ctrl+Tab from a focused pinned panel with two panes" should "step from the active pane, not the first" in {
    val (sm, order) = fourBuffers()
    sm.setBufferForPane(PaneId(0), order(0)).unsafeRunSync()
    val second = sm.splitPaneHorizontal(PaneId(0), Some(order(2))).unsafeRunSync()
    sm.updateState(state =>
      state
        .copy(persisted = state.persisted.copy(layout = state.persisted.layout.copy(activeEditorPaneId = Some(second))))
    ).unsafeRunSync()
    focusDockedExplorer(sm)

    sm.applyEvent(NextTab).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    shownIn(after, second) shouldBe Some(order(3))
    shownIn(after, PaneId(0)) shouldBe Some(order(0))
    after.persisted.focus shouldBe Focus.EditorPane(second)
  }

  "Ctrl+Tab from a focused modeless list picker" should "dismiss it as Escape would, then show the next buffer" in {
    val (sm, order) = fourBuffers()
    sm.updateState(state =>
      PopupSurfaceReducer.openThemePicker(List("dark", "light"), state).map(_.state).getOrElse(state)
    ).unsafeRunSync()
    sm.applyEvent(MoveDown).unsafeRunSync()
    awaitValue(sm.getCurrentState.map(_.persisted.theme.name))(_ == "light").unsafeRunSync() shouldBe "light"

    sm.applyEvent(NextTab).unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.modalSurface shouldBe None
    shownIn(after, PaneId(0)) shouldBe Some(order(3))
    after.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    awaitValue(sm.getCurrentState.map(_.persisted.theme.name))(_ == "dark").unsafeRunSync() shouldBe "dark"
  }
