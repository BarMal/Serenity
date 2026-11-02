package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfigOps.withExplorerOpenMode
import com.serenity.config.ExplorerOpenMode
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

/** How a click in the explorer opens a file or folds a folder, for each `explorer.open_mode` (#2479). */
class ExplorerOpenModeClickSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val viewport  = ViewportSize(100, 32)
  private val surfaceId = SurfaceId("explorer")

  final private case class Fixture(sm: StateManager, folder: Path, folderRow: Int, file: Path, fileRow: Int)

  private def fixture(mode: Option[ExplorerOpenMode]): Fixture =
    val root   = TestTemp.directory("explorer-open-mode")
    val folder = Files.createDirectory(root.resolve("sub"))
    val file   = Files.writeString(root.resolve("notes.txt"), "hello")
    Files.writeString(folder.resolve("inner.txt"), "inner")
    val tree = DirectoryTreeData(
      root,
      entries = Map(
        root   -> List(DirEntry(folder, "sub", isDirectory = true), DirEntry(file, "notes.txt", isDirectory = false)),
        folder -> List(DirEntry(folder.resolve("inner.txt"), "inner.txt", isDirectory = false))
      )
    )
    val logger = LoggerFactory[IO].getLogger(using LoggerName("ExplorerOpenModeClickSpec"))
    val sm     = StateManager.apply(logger, dictionaryCache = SharedDictionary.default).unsafeRunSync()
    sm.updateState { state =>
      val configured = mode.fold(state)(m =>
        state.copy(persisted = state.persisted.copy(config = state.persisted.config.withExplorerOpenMode(m)))
      )
      DockedPanelFixtures.dock(
        configured,
        surfaceId,
        SurfaceContent.DirectoryTree(tree, Some(root)),
        PanelPosition.Left,
        28
      )
    }.unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()
    Fixture(sm, folder, folderRow = 1, file, fileRow = 2)

  private def rowPoint(sm: StateManager, row: Int): (Int, Int) =
    val state    = sm.getCurrentState.unsafeRunSync()
    val layout   = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val contract = EditorLayoutContract.from(state, viewport, layout)
    val content  = contract.panelContentRect(surfaceId).getOrElse(fail("Expected the explorer's content rect"))
    val rowY = contract
      .panelRowSlots(surfaceId)
      .collectFirst { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(`row`), y) => y }
      .getOrElse(fail(s"Expected explorer row $row"))
    (content.x + 1, rowY)

  private def click(fixture: Fixture, row: Int, count: Int): Unit =
    val (col, y) = rowPoint(fixture.sm, row)
    (fixture.sm.applyEvent(MouseClick(col, y, clickCount = count)) >> fixture.sm.runtimeLifecycle.awaitEffects)
      .unsafeRunSync()

  private def openFiles(fixture: Fixture): List[String] =
    fixture.sm.getCurrentState
      .unsafeRunSync()
      .persisted
      .buffers
      .values
      .flatMap(_.document.filePath)
      .map(_.toString)
      .toList

  private def folderExpanded(fixture: Fixture): Boolean =
    fixture.sm.getCurrentState.unsafeRunSync().surfaceById(surfaceId).map(_.content) match
      case Some(SurfaceContent.DirectoryTree(tree, _, _)) => tree.expandedPaths.contains(fixture.folder)
      case other                                          => fail(s"Expected a directory tree, got $other")

  "With no explorer.open_mode set, a single click on a file" should "open it, as single-click is the default" in {
    val f = fixture(None)
    click(f, f.fileRow, count = 1)
    openFiles(f).exists(_.endsWith("notes.txt")) shouldBe true
  }

  "In single-click mode, a click" should "open a file" in {
    val f = fixture(Some(ExplorerOpenMode.SingleClick))
    click(f, f.fileRow, count = 1)
    openFiles(f).exists(_.endsWith("notes.txt")) shouldBe true
  }

  it should "not open the file a second time when the click turns into a double-click" in {
    val f = fixture(Some(ExplorerOpenMode.SingleClick))
    click(f, f.fileRow, count = 1)
    click(f, f.fileRow, count = 2)
    openFiles(f).count(_.endsWith("notes.txt")) shouldBe 1
  }

  it should "expand a folder" in {
    val f = fixture(Some(ExplorerOpenMode.SingleClick))
    click(f, f.folderRow, count = 1)
    folderExpanded(f) shouldBe true
  }

  it should "leave the folder as the first click left it when it turns into a double-click" in {
    val f = fixture(Some(ExplorerOpenMode.SingleClick))
    click(f, f.folderRow, count = 1)
    click(f, f.folderRow, count = 2)
    folderExpanded(f) shouldBe true
  }

  "In double-click mode, a single click" should "select a file without opening it" in {
    val f = fixture(Some(ExplorerOpenMode.DoubleClick))
    click(f, f.fileRow, count = 1)
    openFiles(f).exists(_.endsWith("notes.txt")) shouldBe false
  }

  it should "select a folder without expanding it" in {
    val f = fixture(Some(ExplorerOpenMode.DoubleClick))
    click(f, f.folderRow, count = 1)
    folderExpanded(f) shouldBe false
  }

  "In double-click mode, a double-click" should "open a file" in {
    val f = fixture(Some(ExplorerOpenMode.DoubleClick))
    click(f, f.fileRow, count = 2)
    openFiles(f).exists(_.endsWith("notes.txt")) shouldBe true
  }

  it should "expand a folder" in {
    val f = fixture(Some(ExplorerOpenMode.DoubleClick))
    click(f, f.folderRow, count = 2)
    folderExpanded(f) shouldBe true
  }
