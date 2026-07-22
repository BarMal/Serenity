package com.serenity

import com.serenity.config.{AppConfig, InterfaceDensity}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.accessibility.{AccessibilityRole, AccessibilitySnapshot}
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AccessibilityModelSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val viewport = ViewportSize(100, 30)

  "AccessibilitySnapshot" should "expose stable document and startup action semantics" in {
    val surfaceId = SurfaceId("startup")
    val page = StartupPage(
      "Welcome to Serenity",
      actions = List(
        StartupAction(
          "new-document",
          "New document",
          com.serenity.command.Command.typed(
            "new-document",
            "New document",
            com.serenity.command.CommandIntent.NewFile,
            com.serenity.command.CommandCategory.File
          )
        )
      )
    )
    val state = AppState.initial.copy(
      uiSurfaces = List(
        UiSurface(
          surfaceId,
          SurfaceContent.StartPage(page),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      focus = Focus.Surface(surfaceId)
    )

    val snapshot = AccessibilitySnapshot.from(state, viewport)

    snapshot.nodes.find(_.id == "pane:0").map(_.role) shouldBe Some(AccessibilityRole.Document)
    snapshot.nodes.find(_.id == "surface:startup/action:new-document").map(_.role) shouldBe Some(
      AccessibilityRole.Button
    )
    snapshot.focused.map(_.id) shouldBe Some("surface:startup/action:new-document")
  }

  it should "announce a focus change once while ignoring unchanged editor state" in {
    val before    = AccessibilitySnapshot.from(AppState.initial, viewport)
    val surfaceId = SurfaceId("outline")
    val afterState = AppState.initial.copy(
      uiSurfaces =
        List(UiSurface(surfaceId, SurfaceContent.Outline(Nil), SurfacePresentation.Pinned(PanelPosition.Left, 20))),
      focus = Focus.Surface(surfaceId)
    )

    val after = AccessibilitySnapshot.from(afterState, viewport, Some(before))

    after.announcements.map(_.message) should contain("Outline")
  }

  it should "reserve larger pointer targets outside compact density" in {
    AccessibilitySnapshot.minimumTargetRows(InterfaceDensity.Compact) shouldBe 1
    AccessibilitySnapshot.minimumTargetRows(InterfaceDensity.Comfortable) shouldBe 2
    AccessibilitySnapshot.minimumTargetRows(InterfaceDensity.Spacious) shouldBe 2
  }

  it should "expose visible context-menu actions through their rendered row slots" in {
    val surfaceId = SurfaceId("menu")
    val command = com.serenity.command.Command.typed(
      "save",
      "Save",
      com.serenity.command.CommandIntent.SaveCurrentFile,
      com.serenity.command.CommandCategory.File
    )
    val menu = ContextMenu("Editor", Focus.EditorPane(PaneId(0)), List(
      ContextMenuItem("save", "Save", command),
      ContextMenuItem("save-as", "Save As", command),
      ContextMenuItem("close", "Close", command)
    ), selectedIndex = 1)
    val state = AppState.initial.copy(
      uiSurfaces = List(UiSurface(surfaceId, SurfaceContent.ContextMenu(menu), SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor))),
      focus = Focus.Surface(surfaceId)
    )

    val controls = AccessibilitySnapshot.from(state, ViewportSize(40, 10)).nodes.filter(_.id.startsWith("surface:menu/item:"))

    controls.map(_.name) shouldBe List("Save", "Save As")
    controls.map(_.bounds.y).distinct.size shouldBe controls.size
    controls.map(_.bounds.height).distinct shouldBe List(2)
    controls.find(_.name == "Save As").exists(_.focused) shouldBe true
  }

  it should "expose find and replace workflow fields and actions" in {
    val surfaceId = SurfaceId("replace")
    val state = AppState.initial.copy(
      uiSurfaces = List(
        UiSurface(
          surfaceId,
          SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(ReplaceWorkflowState(findText = "before", replacementText = "after"))),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      focus = Focus.Surface(surfaceId)
    )

    val controls = AccessibilitySnapshot.from(state, viewport).nodes.filter(_.id.startsWith("surface:replace/control:"))

    controls.map(node => node.name -> node.role) should contain allOf (
      "Find" -> AccessibilityRole.TextField,
      "Replace" -> AccessibilityRole.TextField,
      "Replace Next" -> AccessibilityRole.Button,
      "Replace All" -> AccessibilityRole.Button
    )
  }

  it should "align wrapped toolbar accessibility bounds with rendered row slots" in {
    val surfaceId = SurfaceId("toolbar")
    val bufferId = BufferId(42)
    val paneId = PaneId(0)
    val buffer = Buffer.fromString(bufferId, "toolbar").copy(cursors = List(CursorPosition(0, 0)))
    val toolbarState = ContextualToolbarState()
    val state = AppState.initial.copy(
      config = AppConfig.default.withUiElementGap(1),
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)), activeEditorPaneId = Some(paneId)),
      uiSurfaces = List(UiSurface(surfaceId, SurfaceContent.ContextualToolbar(toolbarState), SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)))
    )

    val snapshot = AccessibilitySnapshot.from(state, ViewportSize(20, 28))
    val frame = snapshot.nodes.find(_.id == s"surface:${surfaceId.value}").map(_.bounds).getOrElse(fail("Expected toolbar surface"))
    val rows = ContextualToolbar.rowGroups(ContextualToolbar.itemsFor(state), frame.width - 2, toolbarState.displayMode)
    val expectedRows = SurfaceFrameLayout
      .forContent(frame, SurfaceContent.ContextualToolbar(toolbarState))
      .contentRowSlots(rows.size, hasHeader = false, hasFooter = false, itemGapRows = 1, itemTargetRows = 2)
      .collect { case SurfaceContentRowSlot(SurfaceContentRowKind.Item(_), y) => y }
    val controls = snapshot.nodes.filter(_.id.startsWith(s"surface:${surfaceId.value}/item:"))

    controls.map(_.bounds.y).distinct shouldBe expectedRows
    controls.map(_.bounds.height).distinct shouldBe List(2)
  }
