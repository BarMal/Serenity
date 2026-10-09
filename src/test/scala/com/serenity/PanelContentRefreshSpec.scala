package com.serenity

import java.nio.file.{Files, Paths}

import cats.effect.unsafe.implicits.global
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Rope
import com.serenity.state.models.*
import com.serenity.ui.layout.{DirectoryTreeData, Location, PanelPosition, Symbol, SymbolKind}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A docked outline catches up with edits through the real commit path, once the edit burst pauses. */
class PanelContentRefreshSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private val outlineId = SurfaceId("outline")

  private def withMarkdown(state: AppState, text: String): AppState =
    val bufferId = BufferId(0)
    val buffer   = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers + (bufferId -> buffer.copy(document =
          buffer.document.withContent(Rope(text)).copy(language = Some(LanguageId.Markdown))
        ))
      )
    )

  private def outlineNames(state: AppState): List[String] =
    state.surfaceById(outlineId).toList.flatMap {
      _.content match
        case SurfaceContent.Outline(symbols, _, _) => symbols.map(_.name)
        case _                                     => Nil
    }

  "A docked outline" should "pick up headings added by an edit" in {
    val stateManager = createStateManager("PanelContentRefreshSpec")
    stateManager
      .updateStateValidated(state =>
        DockedPanelFixtures.dock(
          withMarkdown(state, "# One\n"),
          outlineId,
          SurfaceContent.Outline(List(Symbol("One", SymbolKind.Heading, Location(0, 0)))),
          PanelPosition.Right,
          30
        )
      )
      .unsafeRunSync()

    stateManager.updateStateValidated(withMarkdown(_, "# One\n\n# Two\n")).unsafeRunSync()

    val refreshed = awaitState(stateManager)(outlineNames(_) == List("One", "Two")).unsafeRunSync()
    outlineNames(refreshed) shouldBe List("One", "Two")
  }
  private val explorerId = SurfaceId("explorer")

  private def explorerTree(state: AppState): Option[DirectoryTreeData] =
    state.surfaceById(explorerId).map(_.content).collect { case SurfaceContent.DirectoryTree(tree, _, _) => tree }

  "A docked explorer" should "list its root and expanded directories however it was docked" in {
    val root = TestTemp.directory("panel-refresh-explorer")
    val src  = Files.createDirectory(root.resolve("src"))
    val main = Files.createFile(src.resolve("Main.scala"))
    try
      val stateManager = createStateManager("PanelContentRefreshSpec")
      stateManager
        .updateStateValidated(state =>
          DockedPanelFixtures.dock(
            state,
            explorerId,
            SurfaceContent.DirectoryTree(DirectoryTreeData(root, expandedPaths = Set(src))),
            PanelPosition.Left,
            30
          )
        )
        .unsafeRunSync()

      val listed = awaitState(stateManager)(explorerTree(_).exists(_.entries.size == 2)).unsafeRunSync()
      val tree   = explorerTree(listed).getOrElse(fail("explorer gone"))
      tree.entries.get(root).map(_.map(_.path)) shouldBe Some(List(src))
      tree.entries.get(src).map(_.map(_.path)) shouldBe Some(List(main))
      tree.loading shouldBe empty
    finally
      Files.deleteIfExists(main)
      Files.deleteIfExists(src)
      Files.deleteIfExists(root)
  }

  it should "show why a directory could not be listed" in {
    val missing      = Paths.get("/definitely/not/a/serenity/directory")
    val stateManager = createStateManager("PanelContentRefreshSpec")
    stateManager
      .updateStateValidated(state =>
        DockedPanelFixtures.dock(
          state,
          explorerId,
          SurfaceContent.DirectoryTree(DirectoryTreeData(missing)),
          PanelPosition.Left,
          30
        )
      )
      .unsafeRunSync()

    val failed = awaitState(stateManager)(explorerTree(_).exists(_.failed.contains(missing))).unsafeRunSync()
    explorerTree(failed).map(_.loading) shouldBe Some(Set.empty)
  }
end PanelContentRefreshSpec
