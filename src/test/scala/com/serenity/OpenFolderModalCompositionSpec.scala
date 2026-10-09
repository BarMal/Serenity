package com.serenity

import com.serenity.config.ModalKeyAction
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OpenFolderModalCompositionSpec extends AnyFlatSpec with Matchers:

  private val frame = LayoutRect(10, 4, 60, 14)

  private val folders = List(
    FileWorkflowSuggestion("/tmp/project/docs", isDirectory = true),
    FileWorkflowSuggestion("/tmp/project/src", isDirectory = true)
  )

  private def planFor(workflow: FileWorkflowState): ResolvedSurfaceComposition =
    ModalSurfaceComposition
      .forModal(Modal.FileWorkflow(workflow), frame, targetRows = 1, ModalKeyAction.defaultBindings)
      .getOrElse(fail("expected a file workflow composition"))

  private def texts(plan: ResolvedSurfaceComposition): List[String] = plan.paintBoxes.flatMap(_.text)

  private def hintLine(plan: ResolvedSurfaceComposition): String =
    texts(plan).find(_.contains("Cancel")).getOrElse(fail("expected a key hint line"))

  private val folderForm = FileWorkflowState(
    mode = FileWorkflowMode.OpenFolder,
    path = "/tmp/project",
    activeField = FileWorkflowField.Path,
    suggestions = folders
  )

  "An Open Folder form" should "be headed Open Folder" in {
    val heading = planFor(folderForm).paintBoxes.find(_.kind == SurfacePaintKind.Heading)

    heading.flatMap(_.text) shouldBe Some("Open Folder")
  }

  it should "render a Path row and no Filename row" in {
    val plan = planFor(folderForm)

    texts(plan).exists(_.contains("Filename")) shouldBe false
    plan.hitRegions.map(_.semanticLabel) should not contain "Filename"
    plan.focusOrder should contain(SurfaceFocusId("path"))
    plan.focusOrder should not contain SurfaceFocusId("filename")
  }

  it should "list the folders with a trailing slash, the highlighted one selected and each one clickable" in {
    val plan = planFor(folderForm.updated(selectedSuggestionIndex = 1))

    texts(plan) should contain allOf ("/tmp/project/docs/", "/tmp/project/src/")
    plan.paintBoxes.filter(_.selected).flatMap(_.text) should contain("/tmp/project/src/")
    plan.hitRegions.flatMap(_.actionId) should contain allOf (
      SurfaceActionId("file-suggestion-0"),
      SurfaceActionId(
        "file-suggestion-1"
      )
    )
  }

  it should "start the folder list on the row under the Path, with no Filename row above it" in {
    val folderRows = planFor(folderForm).paintBoxes.filter(box => box.text.exists(_.endsWith("docs/"))).map(_.rect.y)
    val openRows =
      planFor(FileWorkflowState(mode = FileWorkflowMode.Open, path = "/tmp/project", suggestions = folders)).paintBoxes
        .filter(box => box.text.exists(_.endsWith("docs/")))
        .map(_.rect.y)

    folderRows.zip(openRows).map { case (folder, open) => open - folder } shouldBe List(1.0)
  }

  it should "show Open folder, not Open as root, in its key hints, from the live keymap" in {
    val hints = hintLine(planFor(folderForm))

    hints should include("Open folder ctrl+r")
    hints should include("Cancel escape")
    hints should not include "Open as root"
  }

  it should "describe Enter as browsing and Tab as descending, so neither reads as opening the folder" in {
    val hints = hintLine(planFor(folderForm))

    hints should include("Browse enter")
    hints should include("Descend tab")
  }

  it should "show a status message in place of the listing footer" in {
    val plan = planFor(folderForm.updated(statusMessage = Some("Not a folder: /tmp/project/notes.txt")))

    texts(plan) should contain("Not a folder: /tmp/project/notes.txt")
  }

  "The generic Open form" should "keep its Filename row and its Open as root hint" in {
    val plan = planFor(FileWorkflowState(mode = FileWorkflowMode.Open, path = "/tmp/project", suggestions = folders))

    texts(plan).exists(_.contains("Filename")) shouldBe true
    hintLine(plan) should include("Open as root ctrl+r")
    hintLine(plan) should not include "Open folder"
  }
end OpenFolderModalCompositionSpec
