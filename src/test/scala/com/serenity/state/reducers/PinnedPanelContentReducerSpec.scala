package com.serenity.state.reducers

import java.nio.file.{Path, Paths}

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{DirEntry, DirectoryTreeData, PanelContent, PanelPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PinnedPanelContentReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val repo = Paths.get("/repo")

  private def directoryTrees(state: AppState): List[(DirectoryTreeData, Option[Path])] =
    state.pinnedSurfaces.map(_.content).collect { case SurfaceContent.DirectoryTree(tree, selected) =>
      tree -> selected
    }

  private def terminals(state: AppState): List[String] =
    state.pinnedSurfaces.map(_.content).collect { case SurfaceContent.Terminal(text, _) => text }

  private def valid(result: ReducerResult): Boolean = AppStateValidation.validated(result.state).isRight

  "PinnedPanelContentReducer.pinOrUpdateTerminal" should "pin a new terminal panel and declare its undo boundary" in {
    val result = PinnedPanelContentReducer.pinOrUpdateTerminal("out", PanelPosition.Bottom, 10, AppState.initial)

    terminals(result.state) shouldBe List("out")
    result.effects should matchPattern { case List(AppEffect.Undo(UndoEffect.RecordBoundary(_, false))) => }
    valid(result) shouldBe true
  }

  it should "update an existing terminal in place without a new surface or undo boundary" in {
    val pinned = PinnedPanelContentReducer.pinOrUpdateTerminal("one", PanelPosition.Bottom, 10, AppState.initial).state

    val result = PinnedPanelContentReducer.pinOrUpdateTerminal("one\ntwo", PanelPosition.Bottom, 10, pinned)

    terminals(result.state) shouldBe List("one\ntwo")
    result.state.pinnedSurfaces.map(_.id) shouldBe pinned.pinnedSurfaces.map(_.id)
    result.effects shouldBe Nil
    valid(result) shouldBe true
  }

  "PinnedPanelContentReducer.loadDirectoryTree" should "pin a Left explorer listing the given entries" in {
    val result = PinnedPanelContentReducer.loadDirectoryTree(repo, List("src/", "build.sbt"), AppState.initial)

    directoryTrees(result.state) match
      case List((tree, None)) =>
        tree.rootPath shouldBe repo
        tree.entries.getOrElse(repo, Nil).map(e => e.name -> e.isDirectory) shouldBe
          List("src/" -> true, "build.sbt" -> false)
      case other => fail(s"Expected one directory tree, got $other")
    val explorerId = result.state.pinnedSurfaces.head.id
    val tree       = result.state.persisted.layout.workspaceTree
    tree.flatMap(_.positionForSurface(explorerId)) shouldBe Some(PanelPosition.Left)
    result.effects should matchPattern { case List(AppEffect.Undo(_)) => }
    valid(result) shouldBe true
  }

  it should "replace an existing explorer's tree and clear its selection" in {
    val first = PinnedPanelContentReducer.loadDirectoryTree(Paths.get("/old"), List("a.txt"), AppState.initial).state
    val selected = PinnedPanelContentReducer.selectFileInExplorer(Paths.get("/old/a.txt"), first).state

    val result = PinnedPanelContentReducer.loadDirectoryTree(repo, List("b.txt"), selected)

    directoryTrees(result.state).map((tree, selection) => tree.rootPath -> selection) shouldBe List(repo -> None)
    result.effects shouldBe Nil
    valid(result) shouldBe true
  }

  "PinnedPanelContentReducer.selectFileInExplorer" should "select the path in the newest explorer" in {
    val loaded = PinnedPanelContentReducer.loadDirectoryTree(repo, List("build.sbt"), AppState.initial).state

    val result = PinnedPanelContentReducer.selectFileInExplorer(repo.resolve("build.sbt"), loaded)

    directoryTrees(result.state).map(_._2) shouldBe List(Some(repo.resolve("build.sbt")))
    result.effects shouldBe Nil
    valid(result) shouldBe true
  }

  it should "leave the state unchanged when no explorer is pinned" in {
    PinnedPanelContentReducer.selectFileInExplorer(repo.resolve("build.sbt"), AppState.initial) shouldBe
      ReducerResult.noEffects(AppState.initial)
  }

  "PinnedPanelContentReducer.forgetMovedFile" should "drop the moved file from explorers listing its directory" in {
    val loaded = PinnedPanelContentReducer.loadDirectoryTree(repo, List("a.txt", "b.txt"), AppState.initial).state

    val result = PinnedPanelContentReducer.forgetMovedFile(repo.resolve("a.txt"), loaded)

    directoryTrees(result.state).map(_._1.entries.getOrElse(repo, Nil).map(_.name)) shouldBe List(List("b.txt"))
    result.effects shouldBe Nil
    valid(result) shouldBe true
  }

  it should "leave explorers that don't list the source directory untouched" in {
    val loaded = PinnedPanelContentReducer.loadDirectoryTree(repo, List("a.txt"), AppState.initial).state

    PinnedPanelContentReducer.forgetMovedFile(Paths.get("/elsewhere/a.txt"), loaded) shouldBe
      ReducerResult.noEffects(loaded)
  }

  it should "leave the state unchanged for a source path with no parent directory" in {
    val loaded = PinnedPanelContentReducer.loadDirectoryTree(repo, List("a.txt"), AppState.initial).state

    PinnedPanelContentReducer.forgetMovedFile(Paths.get("a.txt"), loaded) shouldBe ReducerResult.noEffects(loaded)
  }

  it should "not touch a non-explorer panel" in {
    val withOutline = PanelStateReducer.pin(PanelContent.Outline(Nil), PanelPosition.Right, 20, AppState.initial).state
    val entries     = Map(repo -> List(DirEntry(repo.resolve("a.txt"), "a.txt", isDirectory = false)))
    val explorer    = PanelContent.DirectoryTree(DirectoryTreeData(repo, entries = entries))
    val loaded      = PanelStateReducer.pin(explorer, PanelPosition.Left, 30, withOutline).state

    val result = PinnedPanelContentReducer.forgetMovedFile(repo.resolve("a.txt"), loaded)

    result.state.pinnedSurfaces.map(_.content).collect { case o: SurfaceContent.Outline => o } should have size 1
    directoryTrees(result.state).map(_._1.entries.getOrElse(repo, Nil)) shouldBe List(Nil)
    valid(result) shouldBe true
  }
