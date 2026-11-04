package com.serenity

import java.nio.file.Paths

import _root_.io.circe.parser.decode
import _root_.io.circe.syntax.*
import com.serenity.keystroke.events.{Direction, PanelInputEvent}
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, PinnedPanelComponent}
import com.serenity.state.models.*
import com.serenity.ui.layout.{*, given}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The explorer's root folder collapses and expands like any other (#2479): a dropped folder that becomes the root
  * could not be folded away before.
  */
class ExplorerRootCollapseSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId    = PaneId(0)
  private val surfaceId = SurfaceId("left-panel")
  private val root      = Paths.get("/repo")
  private val src       = root.resolve("src")

  private val expandedRoot = DirectoryTreeData(
    root,
    entries = Map(root -> List(DirEntry(src, "src", isDirectory = true)))
  )

  private def dockedState(tree: DirectoryTreeData, selected: Option[java.nio.file.Path]): AppState =
    val bufferId = BufferId(1)
    val pane     = EditorPane.withBuffer(paneId, bufferId)
    val base = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> Buffer.fromString(bufferId, "hello")),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )
    val docked = DockedPanelFixtures.dock(
      base,
      surfaceId,
      SurfaceContent.DirectoryTree(tree, selected),
      PanelPosition.Left,
      24
    )
    docked.copy(persisted = docked.persisted.copy(focus = Focus.Surface(surfaceId)))

  private def treeAfter(event: PanelInputEvent, state: AppState): DirectoryTreeData =
    PinnedPanelComponent(PanelPosition.Left).processEvent(event, state) match
      case ComponentResult.StateChange(update) =>
        update(state).surfaceById(surfaceId).map(_.content) match
          case Some(SurfaceContent.DirectoryTree(tree, _, _)) => tree
          case other                                          => fail(s"Expected a directory tree, got $other")
      case other => fail(s"Expected StateChange, got $other")

  "A directory tree" should "start with its root expanded" in {
    expandedRoot.rootCollapsed shouldBe false
    DirectoryTreeData.visibleRows(expandedRoot).map(_.path) shouldBe Vector(root, src)
  }

  it should "show only the root row while the root is collapsed" in {
    val rows = DirectoryTreeData.visibleRows(expandedRoot.copy(rootCollapsed = true))
    rows.map(_.path) shouldBe Vector(root)
    rows.map(_.isExpanded) shouldBe Vector(false)
  }

  it should "not ask for a listing of a collapsed root" in {
    val unlisted = DirectoryTreeData(root, rootCollapsed = true)
    unlisted.awaitingListing shouldBe Set.empty
    unlisted.copy(rootCollapsed = false).awaitingListing shouldBe Set(root)
  }

  "The explorer" should "collapse the root when Left is pressed on the expanded root" in {
    val state = dockedState(expandedRoot, Some(root))
    treeAfter(PanelInputEvent.Navigate(Direction.Left), state).rootCollapsed shouldBe true
  }

  it should "collapse the root when Enter is pressed on the expanded root" in {
    val state = dockedState(expandedRoot, Some(root))
    treeAfter(PanelInputEvent.Activate, state).rootCollapsed shouldBe true
  }

  it should "expand a collapsed root with Enter" in {
    val state = dockedState(expandedRoot.copy(rootCollapsed = true), Some(root))
    treeAfter(PanelInputEvent.Activate, state).rootCollapsed shouldBe false
  }

  it should "expand a collapsed root with Right" in {
    val state = dockedState(expandedRoot.copy(rootCollapsed = true), Some(root))
    treeAfter(PanelInputEvent.Navigate(Direction.Right), state).rootCollapsed shouldBe false
  }

  it should "keep a collapsed root's subfolders expanded for when it opens again" in {
    val tree  = expandedRoot.copy(expandedPaths = Set(src))
    val state = dockedState(tree, Some(root))
    val after = treeAfter(PanelInputEvent.Navigate(Direction.Left), state)
    after.rootCollapsed shouldBe true
    after.expandedPaths shouldBe Set(src)
  }

  "A saved explorer" should "round-trip a collapsed root" in {
    val saved = SessionPanelContent.DirectoryTree("/repo", None, Nil, rootCollapsed = true)
    decode[SessionPanelContent](saved.asJson.noSpaces) shouldBe Right(saved)
    saved.toSurfaceContent match
      case SurfaceContent.DirectoryTree(tree, _, _) => tree.rootCollapsed shouldBe true
      case other                                    => fail(s"Expected a directory tree, got $other")
  }

  it should "load, with the root expanded, from a snapshot written before the root could collapse" in {
    val legacy = """{"DirectoryTree":{"rootPath":"/repo","selectedPath":null,"expandedPaths":[]}}"""
    decode[SessionPanelContent](legacy) shouldBe Right(
      SessionPanelContent.DirectoryTree("/repo", None, Nil, rootCollapsed = false)
    )
  }
