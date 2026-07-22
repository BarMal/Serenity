package com.serenity

import com.serenity.config.InterfaceDensity
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.accessibility.{AccessibilityRole, AccessibilitySnapshot}
import com.serenity.ui.layout.{PanelPosition, ViewportSize}
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

    val controls = AccessibilitySnapshot.from(state, ViewportSize(40, 7)).nodes.filter(_.id.startsWith("surface:menu/item:"))

    controls.map(_.name) shouldBe List("Save As")
    controls.map(_.bounds.y).distinct.size shouldBe controls.size
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
