package com.serenity.session

import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Panels docked before panels had fixed surface ids were saved under an allocated `surface-N` id; loading them must
  * move both the panel and the workspace-tree node that references it onto the panel's own fixed id.
  */
class DockedPanelIdMigrationSpec extends AnyFlatSpec with Matchers:

  private def docked(surfaceId: String, position: PanelPosition, content: SessionPanelContent): SessionDockedPanel =
    SessionDockedPanel(surfaceId, SessionPinnedPanel(position, 30, content))

  private val legacyOutline  = docked("surface-3", PanelPosition.Right, SessionPanelContent.Outline(Nil))
  private val legacyTerminal = docked("surface-7", PanelPosition.Bottom, SessionPanelContent.Terminal("done", 0))

  private val legacyTree = SessionWorkspaceNode.Split(
    "root",
    "Vertical",
    0.75,
    SessionWorkspaceNode.EditorLeaf("pane-0", 0),
    SessionWorkspaceNode.Split(
      "dock",
      "Horizontal",
      0.5,
      SessionWorkspaceNode.DockedSurface("dock-outline", "surface-3", "Right"),
      SessionWorkspaceNode.DockedSurface("dock-terminal", "surface-7", "Bottom")
    )
  )

  "SessionDockedPanel.withPanelIds" should "move a registered panel and its tree node onto the panel's fixed id" in {
    val (panels, tree) = SessionDockedPanel.withPanelIds(List(legacyOutline), Some(legacyTree))

    panels.map(_.surfaceId) shouldBe List("panel-outline")
    tree.toList.flatMap(dockedSurfaceIds) should contain("panel-outline")
    tree.toList.flatMap(dockedSurfaceIds) should not contain "surface-3"
  }

  it should "leave panels that have no fixed id, and their nodes, as they were" in {
    val (panels, tree) = SessionDockedPanel.withPanelIds(List(legacyOutline, legacyTerminal), Some(legacyTree))

    panels.map(_.surfaceId) shouldBe List("panel-outline", "surface-7")
    tree.toList.flatMap(dockedSurfaceIds) should contain("surface-7")
  }

  it should "keep only the first of two saved panels of the same kind" in {
    val second = docked("surface-9", PanelPosition.Left, SessionPanelContent.Outline(Nil))

    SessionDockedPanel.withPanelIds(List(legacyOutline, second), None)._1 shouldBe
      List(legacyOutline.copy(surfaceId = "panel-outline"))
  }

  it should "leave an already-migrated layout unchanged" in {
    val migrated = SessionDockedPanel.withPanelIds(List(legacyOutline, legacyTerminal), Some(legacyTree))

    SessionDockedPanel.withPanelIds(migrated._1, migrated._2) shouldBe migrated
  }

  "SessionLayout.restore" should "restore a legacy docked panel under its fixed id, still docked in its saved tree" in {
    val restored = SessionLayout.restore(
      SessionLayout(
        editorPanes = List(SessionEditorPane(0, None)),
        activeEditorPaneId = Some(0),
        workspaceTree = Some(legacyTree),
        dockedPanels = List(legacyOutline, legacyTerminal)
      )
    )

    restored.surfaces.map(_.id) shouldBe List(PanelId.Outline.surfaceId, SurfaceId("surface-7"))
    restored.layout.workspaceTree.flatMap(_.positionForSurface(PanelId.Outline.surfaceId)) shouldBe
      Some(PanelPosition.Right)
  }

  private def dockedSurfaceIds(node: SessionWorkspaceNode): List[String] =
    node match
      case SessionWorkspaceNode.EditorLeaf(_, _)               => Nil
      case SessionWorkspaceNode.DockedSurface(_, surfaceId, _) => List(surfaceId)
      case SessionWorkspaceNode.Split(_, _, _, first, second)  => dockedSurfaceIds(first) ++ dockedSurfaceIds(second)
