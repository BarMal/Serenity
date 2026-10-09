package com.serenity

import java.nio.file.Paths

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class PointerShapeMouseSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport   = ViewportSize(100, 32)
  private val explorerId = SurfaceId("explorer")

  private def makeStateManager() =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager
      .apply(logger, dictionaryCache = SharedDictionary.default)(using
        com.serenity.rope.Balance.default,
        LoggerFactory[IO]
      )
      .unsafeRunSync()

  private def managerWithLeftExplorer(): StateManager =
    val root = Paths.get("/repo")
    val tree = DirectoryTreeData(root, entries = Map(root -> List(DirEntry(root.resolve("src"), "src", true))))
    val sm   = makeStateManager()
    sm.updateState(state =>
      DockedPanelFixtures.dock(
        state,
        explorerId,
        SurfaceContent.DirectoryTree(tree, Some(root)),
        PanelPosition.Left,
        28
      )
    ).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    sm

  private def shape(sm: StateManager): PointerShape =
    sm.getCurrentState.unsafeRunSync().runtime.pointerGesture.pointerShape

  private def move(sm: StateManager, col: Int, row: Int): PointerShape =
    sm.applyEvent(MouseMove(col, row)).unsafeRunSync()
    shape(sm)

  private def contract(sm: StateManager): EditorLayoutContract =
    val state = sm.getCurrentState.unsafeRunSync()
    EditorLayoutContract.from(state, viewport, LayoutEngine.calculateLayoutWithUI(state, viewport))

  private def leftPanelRect(sm: StateManager): LayoutRect =
    contract(sm).pinnedPanelRects.getOrElse(PanelPosition.Left, fail("Expected a left dock"))

  private def editorTextPoint(sm: StateManager): (Int, Int) =
    val content = contract(sm).activePaneLayout.map(_.contentRect).getOrElse(fail("Expected an active editor pane"))
    (content.x + 2, content.y + 1)

  "Pointer shape" should "start as the default pointer" in {
    shape(managerWithLeftExplorer()) shouldBe PointerShape.Default
  }

  it should "become the text pointer over editor text" in {
    val sm         = managerWithLeftExplorer()
    val (col, row) = editorTextPoint(sm)

    move(sm, col, row) shouldBe PointerShape.Text
  }

  it should "become a horizontal resize pointer on a left dock's inner edge" in {
    val sm   = managerWithLeftExplorer()
    val rect = leftPanelRect(sm)

    move(sm, rect.right - 1, rect.y + 2) shouldBe PointerShape.ResizeHorizontal
  }

  it should "become the hand pointer over a dock row away from the edge" in {
    val sm   = managerWithLeftExplorer()
    val rect = leftPanelRect(sm)
    val rowY = contract(sm)
      .panelRowSlots(explorerId)
      .collectFirst { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(0), y) => y }
      .getOrElse(fail("Expected the first explorer row"))

    move(sm, rect.x + 3, rowY) shouldBe PointerShape.Hand
  }

  it should "follow the pointer back from the edge to text" in {
    val sm         = managerWithLeftExplorer()
    val rect       = leftPanelRect(sm)
    val (col, row) = editorTextPoint(sm)

    move(sm, rect.right - 1, rect.y + 2) shouldBe PointerShape.ResizeHorizontal
    move(sm, col, row) shouldBe PointerShape.Text
  }

  it should "stay the drag's shape while a drag moves across other regions" in {
    val sm         = managerWithLeftExplorer()
    val rect       = leftPanelRect(sm)
    val (col, row) = editorTextPoint(sm)

    move(sm, rect.right - 1, rect.y + 2) shouldBe PointerShape.ResizeHorizontal
    sm.applyEvent(MouseDrag(col, row)).unsafeRunSync()

    shape(sm) shouldBe PointerShape.ResizeHorizontal
  }

  it should "become a horizontal resize pointer in the text-area margin, where a drag sets the inset" in {
    val sm = managerWithLeftExplorer()
    sm.updateState(state =>
      state.copy(persisted = state.persisted.copy(config = state.persisted.config.withTextAreaRightInset(0.1)))
    ).unsafeRunSync()
    val margin = contract(sm).rightSpacerRect

    margin.width should be > 0
    move(sm, margin.x + margin.width / 2, margin.y + margin.height / 2) shouldBe PointerShape.ResizeHorizontal
  }
