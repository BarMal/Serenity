package com.serenity

import _root_.io.circe.Json
import _root_.io.circe.parser.decode
import _root_.io.circe.syntax.*
import com.serenity.config.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.layout.given
import com.serenity.ui.presets.UiPreset
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers #820's remaining acceptance criteria: UI presets persist and migrate workspace-tree/dock topology the same
  * way session persistence does. Position/size for a docked panel is owned solely by the workspace tree (issue #817:
  * `SurfacePresentation` carries no payload), so fixtures dock `outlinePanel` into `nestedTree` directly rather than
  * relying on its presentation.
  */
class UiPresetWorkspaceTreeSpec extends AnyFlatSpec with Matchers:

  import UiPreset.given

  given Balance = Balance.default

  private val pane0 = PaneId(0)
  private val pane1 = PaneId(1)

  // Root split ratio 0.30 against the assumed 100-cell fallback viewport (no live viewport in these fixtures) so the
  // docked surface's tree-derived size round-trips to exactly 30, matching `outlinePanel`'s captured panel size below.
  private def nestedTree: WorkspaceTree =
    WorkspaceTree(
      WorkspaceNode.Split(
        WorkspaceNodeId("dock-split-1"),
        SplitAxis.Horizontal,
        0.30,
        WorkspaceNode.DockedSurface(WorkspaceNodeId("dock-node-1"), SurfaceId("surface-7"), PanelPosition.Left),
        WorkspaceNode.Split(
          WorkspaceNodeId("editor-split-1"),
          SplitAxis.Vertical,
          0.5,
          WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), pane0),
          WorkspaceNode.Leaf(WorkspaceNodeId("editor-1"), pane1)
        )
      )
    )

  private def nestedSessionTree: SessionWorkspaceNode =
    SessionWorkspaceNode.Split(
      "dock-split-1",
      "Horizontal",
      0.30,
      SessionWorkspaceNode.DockedSurface("dock-node-1", "surface-7", "Left"),
      SessionWorkspaceNode.Split(
        "editor-split-1",
        "Vertical",
        0.5,
        SessionWorkspaceNode.EditorLeaf("editor-0", 0),
        SessionWorkspaceNode.EditorLeaf("editor-1", 1)
      )
    )

  private val outlinePanel = UiSurface(
    id = SurfaceId("surface-7"),
    content = SurfaceContent.Outline(Nil),
    presentation = SurfacePresentation.Docked
  )

  "UiPreset" should "capture a nested split and docked-panel workspace tree" in {
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        layout = Layout(
          editorPanes = Map(pane0 -> EditorPane.empty(pane0), pane1 -> EditorPane.empty(pane1)),
          activeEditorPaneId = Some(pane0),
          paneOrder = List(pane0, pane1),
          workspaceTree = Some(nestedTree)
        )
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(outlinePanel), nextPaneId = PaneId(2))
    )

    val preset = UiPreset.capture("Nested", state, preferredWindowSize = None)

    preset.workspaceTree shouldBe Some(nestedSessionTree)
    preset.dockedPanels shouldBe List(
      SessionDockedPanel("surface-7", SessionPinnedPanel(PanelPosition.Left, 30, SessionPanelContent.Outline(Nil)))
    )
    preset.schemaVersion shouldBe UiPreset.CurrentSchemaVersion
  }

  it should "round-trip a nested workspace tree and docked panels through JSON" in {
    val preset = UiPreset(
      name = "Nested",
      config = AppConfig.default,
      themeName = Theme.dark.name,
      dockedPanels = List(
        SessionDockedPanel("surface-7", SessionPinnedPanel(PanelPosition.Left, 30, SessionPanelContent.Outline(Nil)))
      ),
      workspaceTree = Some(nestedSessionTree)
    )

    val decoded = decode[UiPreset](preset.asJson.noSpaces).getOrElse(fail("nested preset should decode"))

    decoded.workspaceTree shouldBe preset.workspaceTree
    decoded.dockedPanels shouldBe preset.dockedPanels
    decoded.schemaVersion shouldBe preset.schemaVersion
  }

  it should "restore a persisted nested workspace tree onto app state" in {
    val bufferId = BufferId(0)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        layout = Layout(
          editorPanes = Map(pane0 -> EditorPane.withBuffer(pane0, bufferId), pane1 -> EditorPane.empty(pane1)),
          activeEditorPaneId = Some(pane0),
          paneOrder = List(pane0, pane1)
        )
      ),
      runtime = AppState.initial.runtime.copy(nextPaneId = PaneId(2))
    )
    val preset = UiPreset(
      name = "Nested",
      config = AppConfig.default,
      themeName = Theme.dark.name,
      dockedPanels = List(
        SessionDockedPanel("surface-7", SessionPinnedPanel(PanelPosition.Left, 30, SessionPanelContent.Outline(Nil)))
      ),
      workspaceTree = Some(nestedSessionTree),
      targetEditorPaneCount = Some(2)
    )

    val restored = UiPreset.applyToState(preset, state, Theme.dark)

    restored.persisted.layout.workspaceTree.map(_.paneIds) shouldBe Some(List(pane0, pane1))
    restored.persisted.layout.workspaceTree.map(_.dockedSurfaceIds) shouldBe Some(List(SurfaceId("surface-7")))
    restored.persisted.layout.workspaceTree.map(_.root.axis) shouldBe Some(Some(SplitAxis.Horizontal))
    restored.pinnedSurfaces should have size 1
    restored.pinnedSurfaces.head.id shouldBe SurfaceId("surface-7")
    restored.pinnedSurfaces.head.presentation shouldBe SurfacePresentation.Docked
    restored.persisted.layout.workspaceTree.flatMap(
      _.positionForSurface(SurfaceId("surface-7"))
    ) shouldBe Some(PanelPosition.Left)
    restored.persisted.layout.workspaceTree.flatMap(
      _.currentSize(SurfaceId("surface-7"), restored.runtime.viewportSize)
    ) shouldBe Some(30)
    restored.isValid shouldBe true
  }

  it should "migrate a legacy preset with only pinnedPanels into dockedPanels with no persisted tree" in {
    val legacyPanel = SessionPinnedPanel(PanelPosition.Left, 20, SessionPanelContent.DirectoryTree("/repo", None, Nil))
    val baseJson    = UiPreset(name = "Legacy", config = AppConfig.default, themeName = Theme.dark.name).asJson
    val legacyJson = baseJson.mapObject(
      _.remove("dockedPanels")
        .remove("workspaceTree")
        .remove("maximizedWorkspaceNodeId")
        .remove("schemaVersion")
        .add("pinnedPanels", Json.arr(legacyPanel.asJson))
    )

    val decoded = decode[UiPreset](legacyJson.noSpaces).getOrElse(fail("legacy preset should decode"))

    decoded.workspaceTree shouldBe None
    decoded.dockedPanels.map(_.panel) shouldBe List(legacyPanel)

    val reEncoded = decoded.asJson.hcursor
    reEncoded.downField("dockedPanels").succeeded shouldBe true
    reEncoded.downField("pinnedPanels").succeeded shouldBe false
  }

  it should "fall back safely to a valid one-editor layout when a persisted tree is invalid" in {
    val state = AppState.initial
    val preset = UiPreset(
      name = "Broken",
      config = AppConfig.default,
      themeName = Theme.dark.name,
      workspaceTree = Some(SessionWorkspaceNode.EditorLeaf("bad-leaf", 999)),
      targetEditorPaneCount = Some(1)
    )

    val restored = UiPreset.applyToState(preset, state, Theme.dark)

    restored.persisted.buffers shouldBe state.persisted.buffers
    restored.persisted.layout.editorPanes should have size 1
    restored.persisted.layout.workspaceTree.map(_.paneIds.size) shouldBe Some(1)
    restored.isValid shouldBe true
  }

  it should "drop a maximized node reference that no longer resolves to a docked surface" in {
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        layout = Layout(
          editorPanes = Map(pane0 -> EditorPane.empty(pane0)),
          activeEditorPaneId = Some(pane0),
          paneOrder = List(pane0)
        )
      )
    )
    val preset = UiPreset(
      name = "StaleMaximized",
      config = AppConfig.default,
      themeName = Theme.dark.name,
      maximizedWorkspaceNodeId = Some("dock-node-1"),
      targetEditorPaneCount = Some(1)
    )

    val restored = UiPreset.applyToState(preset, state, Theme.dark)

    restored.persisted.layout.maximizedWorkspaceNodeId shouldBe None
    restored.isValid shouldBe true
  }

  it should "preserve unknown top-level fields alongside a persisted workspace tree" in {
    val preset = UiPreset(
      name = "Nested",
      config = AppConfig.default,
      themeName = Theme.dark.name,
      workspaceTree = Some(nestedSessionTree)
    )
    val withUnknownField = preset.asJson.mapObject(_.add("futureField", Json.fromString("keep-me")))

    val decoded = decode[UiPreset](withUnknownField.noSpaces).getOrElse(fail("preset should decode"))

    decoded.workspaceTree shouldBe preset.workspaceTree
    decoded.asJson.hcursor.downField("futureField").as[String] shouldBe Right("keep-me")
  }
