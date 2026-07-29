package com.serenity

import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModalSurfaceCompositionSpec extends AnyFlatSpec with Matchers:

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
