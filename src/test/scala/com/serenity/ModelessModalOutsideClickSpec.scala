package com.serenity

import java.nio.file.Paths

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.state.reducers.PopupSurfaceReducer
import com.serenity.testkit.AwaitCondition.awaitValue
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** A click outside a modeless modal dismisses it exactly as Escape would, then lands where it was aimed. */
class ModelessModalOutsideClickSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val viewport = ViewportSize(100, 32)
  private val text     = (1 to 40).map(line => s"line number $line of the document").mkString("\n")

  private def editorWith(text: String): (StateManager, BufferId) =
    val logger   = LoggerFactory[IO].getLogger(using LoggerName("ModelessModalOutsideClickSpec"))
    val sm       = StateManager.apply(logger).unsafeRunSync()
    val bufferId = sm.createBuffer(text, None).unsafeRunSync()
    sm.setBufferForPane(PaneId(0), bufferId).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    (sm, bufferId)

  private def pressAndClick(sm: StateManager, point: (Int, Int)): Unit =
    sm.applyEvent(MousePress(point._1, point._2)).unsafeRunSync()
    sm.applyEvent(MouseClick(point._1, point._2)).unsafeRunSync()

  private def modalFrame(state: AppState): LayoutRect =
    val modalId = state.modalSurface.map(_.id).getOrElse(fail("no modeless modal open"))
    UiSceneSnapshot
      .from(state, viewport)
      .floating
      .find(_.id == SceneNodeId.Surface(modalId))
      .map(_.frameRect)
      .getOrElse(fail("modeless modal not in the scene"))

  /** A point low in the editor's text area, checked to lie outside the open modal. */
  private def editorPointOutsideModal(state: AppState): (Int, Int) =
    val paneRect = LayoutEngine
      .calculatePaneLayouts(state, LayoutEngine.calculateLayout(state, viewport))
      .getOrElse(PaneId(0), fail("no editor pane"))
    val point = (paneRect.x + 6, paneRect.y + paneRect.height - 3)
    modalFrame(state).contains(point._1, point._2) shouldBe false
    point

  /** Where the same click puts the cursor with no modal open. */
  private def cursorsAfterPlainClick(point: (Int, Int)): List[CursorPosition] =
    val (sm, bufferId) = editorWith(text)
    pressAndClick(sm, point)
    sm.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).editing.cursorPositions

  private def themeBecomes(sm: StateManager, name: String): String =
    awaitValue(sm.getCurrentState.map(_.persisted.theme.name))(_ == name).unsafeRunSync()

  "A click on the editor" should "dismiss an open theme picker as Escape would, restoring the theme, then move the cursor" in {
    val (sm, bufferId) = editorWith(text)
    sm.updateState(state =>
      PopupSurfaceReducer.openThemePicker(List("dark", "light"), state).map(_.state).getOrElse(state)
    ).unsafeRunSync()
    sm.applyEvent(MoveDown).unsafeRunSync()
    themeBecomes(sm, "light") shouldBe "light"

    val opened = sm.getCurrentState.unsafeRunSync()
    val point  = editorPointOutsideModal(opened)
    pressAndClick(sm, point)

    val after = sm.getCurrentState.unsafeRunSync()
    after.modalSurface shouldBe None
    after.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    after.persisted.buffers(bufferId).editing.cursorPositions shouldBe cursorsAfterPlainClick(point)
    themeBecomes(sm, "dark") shouldBe "dark"
  }

  it should "dismiss an open Find, then move the cursor" in {
    val (sm, bufferId) = editorWith(text)
    sm.applyEvent(OpenFind).unsafeRunSync()
    val opened = sm.getCurrentState.unsafeRunSync()
    opened.modalSurface.map(_.content) should matchPattern {
      case Some(SurfaceContent.ModalWorkflow(_: Modal.Find)) =>
    }

    val point = editorPointOutsideModal(opened)
    pressAndClick(sm, point)

    val after = sm.getCurrentState.unsafeRunSync()
    after.modalSurface shouldBe None
    after.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    after.persisted.buffers(bufferId).editing.cursorPositions shouldBe cursorsAfterPlainClick(point)
  }

  "A click on a pinned panel row" should "dismiss an open Find, then select and focus the row" in {
    val root = Paths.get("/repo")
    val src  = root.resolve("src")
    val test = root.resolve("test")
    val tree = DirectoryTreeData(
      root,
      entries = Map(root -> List(DirEntry(src, "src", isDirectory = true), DirEntry(test, "test", isDirectory = true)))
    )
    val explorerId = SurfaceId("explorer")
    val (sm, _)    = editorWith(text)
    sm.updateState(state =>
      DockedPanelFixtures.dock(state, explorerId, SurfaceContent.DirectoryTree(tree, Some(root)), PanelPosition.Left, 28)
    ).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    sm.applyEvent(OpenFind).unsafeRunSync()

    val opened   = sm.getCurrentState.unsafeRunSync()
    val contract = EditorLayoutContract.from(opened, viewport, LayoutEngine.calculateLayoutWithUI(opened, viewport))
    val rowY = contract
      .panelRowSlots(explorerId)
      .collectFirst { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(2), y) => y }
      .getOrElse(fail("no explorer row"))
    val point = (contract.panelContentRect(explorerId).getOrElse(fail("no explorer rect")).x + 1, rowY)
    modalFrame(opened).contains(point._1, point._2) shouldBe false

    pressAndClick(sm, point)

    val after = sm.getCurrentState.unsafeRunSync()
    after.modalSurface shouldBe None
    after.persisted.focus shouldBe Focus.Surface(explorerId)
    after.surfaceById(explorerId).map(_.content) shouldBe Some(SurfaceContent.DirectoryTree(tree, Some(test)))
  }

  "A click inside the modeless modal" should "leave it open and focused" in {
    val (sm, bufferId) = editorWith(text)
    sm.applyEvent(OpenFind).unsafeRunSync()
    val opened  = sm.getCurrentState.unsafeRunSync()
    val modalId = opened.modalSurface.map(_.id).getOrElse(fail("no Find open"))
    val frame   = modalFrame(opened)

    pressAndClick(sm, (frame.x + 1, frame.y + 1))

    val after = sm.getCurrentState.unsafeRunSync()
    after.modalSurface.map(_.id) shouldBe Some(modalId)
    after.persisted.focus shouldBe Focus.Surface(modalId)
    after.persisted.buffers(bufferId).editing shouldBe opened.persisted.buffers(bufferId).editing
  }
