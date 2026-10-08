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

class MouseGestureOriginSpec extends AnyFlatSpec with Matchers:

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
      DockedPanelFixtures
        .dock(state, explorerId, SurfaceContent.DirectoryTree(tree, Some(root)), PanelPosition.Left, 28)
    ).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    sm

  private def contract(sm: StateManager): EditorLayoutContract =
    val state = sm.getCurrentState.unsafeRunSync()
    EditorLayoutContract.from(state, viewport, LayoutEngine.calculateLayoutWithUI(state, viewport))

  private def leftPanelRect(sm: StateManager): LayoutRect =
    contract(sm).pinnedPanelRects.getOrElse(PanelPosition.Left, fail("Expected a left dock"))

  private def editorTextPoint(sm: StateManager): (Int, Int) =
    val content = contract(sm).activePaneLayout.map(_.contentRect).getOrElse(fail("Expected an active editor pane"))
    (content.x + 2, content.y + 1)

  "A drag" should "keep selecting text when it moves into the dock region instead of resizing the dock" in {
    val sm         = managerWithLeftExplorer()
    val rect       = leftPanelRect(sm)
    val (col, row) = editorTextPoint(sm)

    sm.applyEvent(MousePress(col, row)).unsafeRunSync()
    sm.applyEvent(MouseDrag(rect.right - 6, rect.y + 2)).unsafeRunSync()

    leftPanelRect(sm) shouldBe rect
  }

  it should "resize the dock when the press landed on its inner edge" in {
    val sm   = managerWithLeftExplorer()
    val rect = leftPanelRect(sm)
    val to   = rect.right - 6

    sm.applyEvent(MousePress(rect.right - 1, rect.y + 2)).unsafeRunSync()
    sm.applyEvent(MouseDrag(to, rect.y + 2)).unsafeRunSync()

    leftPanelRect(sm).width shouldBe to + 1
  }

  it should "not resize the dock when the press landed on a dock row" in {
    val sm   = managerWithLeftExplorer()
    val rect = leftPanelRect(sm)
    val rowY = contract(sm)
      .panelRowSlots(explorerId)
      .collectFirst { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(0), y) => y }
      .getOrElse(fail("Expected the first explorer row"))

    sm.applyEvent(MousePress(rect.x + 3, rowY)).unsafeRunSync()
    sm.applyEvent(MouseDrag(rect.right - 6, rowY)).unsafeRunSync()

    leftPanelRect(sm) shouldBe rect
  }

  it should "start a new gesture at each press" in {
    val sm         = managerWithLeftExplorer()
    val rect       = leftPanelRect(sm)
    val (col, row) = editorTextPoint(sm)
    val to         = rect.right - 6

    sm.applyEvent(MousePress(col, row)).unsafeRunSync()
    sm.applyEvent(MouseDrag(to, rect.y + 2)).unsafeRunSync()
    sm.applyEvent(MousePress(rect.right - 1, rect.y + 2)).unsafeRunSync()
    sm.applyEvent(MouseDrag(to, rect.y + 2)).unsafeRunSync()

    leftPanelRect(sm).width shouldBe to + 1
  }

  it should "resize the dock when there was no press to say otherwise" in {
    val sm   = managerWithLeftExplorer()
    val rect = leftPanelRect(sm)
    val to   = rect.right - 6

    sm.applyEvent(MouseDrag(to, rect.y + 2)).unsafeRunSync()

    leftPanelRect(sm).width shouldBe to + 1
  }
