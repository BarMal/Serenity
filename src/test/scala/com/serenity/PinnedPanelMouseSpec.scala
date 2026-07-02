package com.serenity

import java.nio.file.Paths

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{MouseClick, MouseMove, ResizeEvent}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class PinnedPanelMouseSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(100, 32)

  private def makeStateManager() =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

  private def explorerSurface(tree: DirectoryTreeData, selectedPath: Option[java.nio.file.Path]) =
    UiSurface(
      id = SurfaceId("explorer"),
      content = SurfaceContent.DirectoryTree(tree, selectedPath),
      presentation = SurfacePresentation.Pinned(PanelPosition.Left, 28)
    )

  private def leftPanelContentRect(state: AppState): LayoutRect =
    val layout = LayoutEngine.calculateLayoutWithUI(state, viewport)
    SurfaceFrameLayout(layout.pinnedSurfaceRects(SurfaceId("explorer"))).contentRect

  "Pinned panel mouse support" should "select and focus a directory tree row on primary click" in {
    val root = Paths.get("/repo")
    val src  = root.resolve("src")
    val test = root.resolve("test")
    val tree = DirectoryTreeData(
      root,
      entries = Map(
        root -> List(
          DirEntry(src, "src", isDirectory = true),
          DirEntry(test, "test", isDirectory = true)
        )
      )
    )
    val surface = explorerSurface(tree, selectedPath = Some(root))
    val sm      = makeStateManager()
    sm.updateState(_.copy(uiSurfaces = List(surface))).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val rect = leftPanelContentRect(sm.getCurrentState.unsafeRunSync())
    sm.applyEvent(MouseClick(rect.x + 1, rect.y + 2)).unsafeRunSync()

    val updated = sm.getCurrentState.unsafeRunSync()
    updated.focus shouldBe Focus.Surface(surface.id)
    updated.surfaceById(surface.id).map(_.content) shouldBe Some(SurfaceContent.DirectoryTree(tree, Some(test)))
  }

  it should "select a directory tree row on hover without stealing focus" in {
    val root = Paths.get("/repo")
    val src  = root.resolve("src")
    val tree = DirectoryTreeData(
      root,
      entries = Map(root -> List(DirEntry(src, "src", isDirectory = true)))
    )
    val surface = explorerSurface(tree, selectedPath = Some(root))
    val sm      = makeStateManager()
    sm.updateState(_.copy(uiSurfaces = List(surface))).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val before = sm.getCurrentState.unsafeRunSync()
    val rect   = leftPanelContentRect(before)
    sm.applyEvent(MouseMove(rect.x + 1, rect.y + 1)).unsafeRunSync()

    val updated = sm.getCurrentState.unsafeRunSync()
    updated.focus shouldBe before.focus
    updated.surfaceById(surface.id).map(_.content) shouldBe Some(SurfaceContent.DirectoryTree(tree, Some(src)))
  }

  it should "activate a double-clicked loaded directory tree row" in {
    val root = Paths.get("/repo")
    val src  = root.resolve("src")
    val tree = DirectoryTreeData(
      root,
      entries = Map(
        root -> List(DirEntry(src, "src", isDirectory = true)),
        src  -> List(DirEntry(src.resolve("Main.scala"), "Main.scala", isDirectory = false))
      )
    )
    val surface = explorerSurface(tree, selectedPath = Some(root))
    val sm      = makeStateManager()
    sm.updateState(_.copy(uiSurfaces = List(surface))).unsafeRunSync()
    sm.applyEvent(ResizeEvent(viewport)).unsafeRunSync()

    val rect = leftPanelContentRect(sm.getCurrentState.unsafeRunSync())
    sm.applyEvent(MouseClick(rect.x + 1, rect.y + 1, clickCount = 2)).unsafeRunSync()

    val expandedTree = tree.copy(expandedPaths = Set(src))
    val updated      = sm.getCurrentState.unsafeRunSync()
    updated.focus shouldBe Focus.Surface(surface.id)
    updated.surfaceById(surface.id).map(_.content) shouldBe Some(SurfaceContent.DirectoryTree(expandedTree, Some(src)))
  }
