package com.serenity

import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModalSurfaceCompositionSpec extends AnyFlatSpec with Matchers:

  private val frame = LayoutRect(10, 4, 60, 12)

  private def planFor(modal: Modal, targetRows: Int = 1): ResolvedSurfaceComposition =
    ModalSurfaceComposition
      .forModal(modal, frame, targetRows)
      .getOrElse(fail(s"expected composition for $modal"))

  "ModalSurfaceComposition" should "derive close workflow paint, focus, and hit geometry from one plan" in {
    val workflow = CloseWorkflowState(
      CloseScope.Current,
      BufferId(7),
      "notes.scala",
      selectedChoice = CloseWorkflowChoice.Discard
    )

    val plan = ModalSurfaceComposition.close(workflow, LayoutRect(10, 4, 60, 10), targetRows = 2)

    plan.focusOrder shouldBe List(
      SurfaceFocusId("close-save"),
      SurfaceFocusId("close-discard"),
      SurfaceFocusId("close-cancel")
    )
    plan.hitRegions.map(_.semanticLabel) shouldBe List("Save", "Close Anyway", "Cancel")
    plan.hitRegions.map(_.rect.height).distinct shouldBe List(2.0)
    plan.paintBoxes.filter(_.actionId.nonEmpty).map(_.rect) shouldBe plan.hitRegions.map(_.rect)
    plan.paintBoxes.find(_.actionId.contains(SurfaceActionId("close-discard"))).exists(_.selected) shouldBe true
  }

  it should "scale close action targets with interface density without changing action identity" in {
    val workflow = CloseWorkflowState(CloseScope.Current, BufferId(7), "notes.scala")
    val frame    = LayoutRect(0, 0, 40, 12)

    val compact     = ModalSurfaceComposition.close(workflow, frame, targetRows = 1)
    val comfortable = ModalSurfaceComposition.close(workflow, frame, targetRows = 2)

    compact.hitRegions.map(_.actionId) shouldBe comfortable.hitRegions.map(_.actionId)
    compact.hitRegions.map(_.rect.height).distinct shouldBe List(1.0)
    comfortable.hitRegions.map(_.rect.height).distinct shouldBe List(2.0)
    ModalSurfaceComposition.closeFrameHeight(targetRows = 1) shouldBe 7
    ModalSurfaceComposition.closeFrameHeight(targetRows = 2) shouldBe 10
  }

  it should "map only declared close action identities back to workflow choices" in {
    ModalSurfaceComposition.closeChoice(SurfaceActionId("close-save")) shouldBe Some(CloseWorkflowChoice.Save)
    ModalSurfaceComposition.closeChoice(SurfaceActionId("close-discard")) shouldBe Some(CloseWorkflowChoice.Discard)
    ModalSurfaceComposition.closeChoice(SurfaceActionId("close-cancel")) shouldBe Some(CloseWorkflowChoice.Cancel)
    ModalSurfaceComposition.closeChoice(SurfaceActionId("unsupported")) shouldBe None
  }

  it should "reflow every close action inside a height-constrained frame" in {
    val workflow = CloseWorkflowState(CloseScope.Current, BufferId(7), "notes.scala")
    val frame    = LayoutRect(5, 2, 30, 4)
    val content  = SurfaceFrameLayout(frame).contentRect

    val plan = ModalSurfaceComposition.close(workflow, frame, targetRows = 2)

    plan.paintBoxes.foreach(box => plan.bounds.containsRect(box.rect) shouldBe true)
    plan.hitRegions.foreach(hit => plan.bounds.containsRect(hit.rect) shouldBe true)
    plan.hitRegions.map(_.semanticLabel) shouldBe List("Save", "Close Anyway", "Cancel")
    plan.hitRegions.map(_.rect.y).distinct shouldBe List(content.bottom - 1.0)
    plan.hitRegions.map(_.rect.width).sum shouldBe content.width.toDouble
  }

  it should "omit clipped controls from paint, hit, and focus output in a tiny frame" in {
    val workflow = CloseWorkflowState(CloseScope.Current, BufferId(7), "notes.scala")
    val plan     = ModalSurfaceComposition.close(workflow, LayoutRect(0, 0, 3, 3), targetRows = 2)

    plan.paintBoxes.foreach { box =>
      plan.bounds.containsRect(box.rect) shouldBe true
      box.rect.width should be > 0.0
      box.rect.height should be > 0.0
    }
    plan.hitRegions.foreach(hit => plan.bounds.containsRect(hit.rect) shouldBe true)
    plan.focusOrder shouldBe plan.hitRegions.map(_.focusId)
  }

  it should "compose goto and custom workflows as labelled text inputs" in {
    val goto   = planFor(Modal.GotoLine("42"))
    val custom = planFor(Modal.Custom("rename", "draft"))

    goto.hitRegions.map(_.semanticLabel) shouldBe List("Go to line")
    goto.paintBoxes.flatMap(_.cursorOffset) shouldBe List("Go to line 42".length)
    custom.hitRegions.map(_.semanticLabel) shouldBe List("rename")
    custom.paintBoxes.flatMap(_.cursorOffset) shouldBe List("rename draft".length)
    goto.focusOrder shouldBe goto.hitRegions.map(_.focusId)
    custom.focusOrder shouldBe custom.hitRegions.map(_.focusId)
  }

  it should "compose find input and result rows from one clipped plan" in {
    val plan = planFor(
      Modal.Find(
        "needle",
        List(FindResult(1, 2), FindResult(4, 5), FindResult(7, 8)),
        currentIndex = 1
      )
    )

    plan.hitRegions.map(_.semanticLabel) shouldBe List("Find")
    plan.paintBoxes.flatMap(_.text) should contain allOf ("Find needle", "1. 2:3", "2. 5:6", "3. 8:9")
    plan.paintBoxes.flatMap(_.text) should contain("3 matches, 2/3 at 5:6")
    plan.paintBoxes.find(_.text.contains("2. 5:6")).exists(_.selected) shouldBe true
    plan.paintBoxes.foreach(box => plan.bounds.containsRect(box.rect) shouldBe true)
  }

  it should "compose a zero-match footer for a non-empty find query" in {
    val plan = planFor(Modal.Find("missing", Nil, currentIndex = 0))

    plan.paintBoxes.flatMap(_.text) should contain("0 matches")
  }

  it should "compose replace fields and actions with stable semantic identities" in {
    val workflow = ReplaceWorkflowState(
      findText = "before",
      replacementText = "after",
      activeField = ReplaceWorkflowField.ReplaceWith,
      selectedAction = ReplaceWorkflowAction.ReplaceAll,
      selectedScope = ReplaceWorkflowScope.Selection
    )

    val plan = planFor(Modal.ReplaceWorkflow(workflow))

    plan.hitRegions.map(_.semanticLabel) should contain allOf (
      "Find",
      "Replace",
      "Replace Next",
      "Replace All",
      "Current Buffer",
      "Selection"
    )
    plan.paintBoxes.find(_.actionId.contains(SurfaceActionId("replace-all"))).exists(_.selected) shouldBe true
    plan.paintBoxes.find(_.actionId.contains(SurfaceActionId("replace-selection"))).exists(_.selected) shouldBe true
    plan.paintBoxes
      .find(_.semanticLabel.contains("Find"))
      .exists(box => !box.selected && box.cursorOffset.isEmpty) shouldBe true
    plan.paintBoxes
      .find(_.semanticLabel.contains("Replace"))
      .exists(box => box.selected && box.cursorOffset.contains("Replace after".length)) shouldBe true
    plan.paintBoxes.filter(_.actionId.nonEmpty).map(_.rect) shouldBe
      plan.hitRegions.filter(_.actionId.nonEmpty).map(_.rect)
  }

  it should "compose file fields and suggestions with matching paint and hit boxes" in {
    val workflow = FileWorkflowState(
      mode = FileWorkflowMode.SaveAs,
      filename = "notes.scala",
      path = "/tmp/project",
      activeField = FileWorkflowField.Path,
      suggestions = List(
        FileWorkflowSuggestion("/tmp/project", isDirectory = true),
        FileWorkflowSuggestion("/tmp/project/notes.scala", isDirectory = false)
      ),
      selectedSuggestionIndex = 1
    )

    val plan = planFor(Modal.FileWorkflow(workflow), targetRows = 2)

    plan.hitRegions.map(_.semanticLabel) should contain allOf (
      "Filename",
      "Path",
      "/tmp/project/",
      "/tmp/project/notes.scala"
    )
    plan.paintBoxes.find(_.actionId.contains(SurfaceActionId("file-suggestion-1"))).exists(_.selected) shouldBe true
    plan.paintBoxes.flatMap(_.text) should contain allOf ("save-as", "Filename notes.scala", "Path /tmp/project")
    plan.paintBoxes
      .find(_.text.contains("Path /tmp/project"))
      .map(_.segments.exists(_.tone == OverlayTone.Error)) shouldBe Some(false)
    plan.paintBoxes.filter(_.focusId.nonEmpty).map(_.rect) shouldBe plan.hitRegions.map(_.rect)
  }

  it should "derive preferred frame height from each workflow composition" in {
    ModalSurfaceComposition.frameHeight(Modal.GotoLine(""), targetRows = 1) shouldBe 3
    ModalSurfaceComposition.frameHeight(Modal.Custom("rename", ""), targetRows = 1) shouldBe 4
    ModalSurfaceComposition.frameHeight(Modal.Find("needle", Nil, 0), targetRows = 1) shouldBe 4
    ModalSurfaceComposition.frameHeight(
      Modal.ReplaceWorkflow(ReplaceWorkflowState(statusMessage = Some("Nothing to replace"))),
      targetRows = 1
    ) shouldBe 8
    ModalSurfaceComposition.frameHeight(
      Modal.FileWorkflow(
        FileWorkflowState(
          mode = FileWorkflowMode.Open,
          suggestions = List(FileWorkflowSuggestion("one", false), FileWorkflowSuggestion("two", false))
        )
      ),
      targetRows = 1
    ) shouldBe 8
  }
