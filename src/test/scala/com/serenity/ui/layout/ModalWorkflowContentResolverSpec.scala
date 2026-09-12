package com.serenity.ui.layout

import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated unit coverage for `ModalWorkflowContentResolver` (issue #1421). `SurfaceContentResolverModalWorkflowSpec`
  * covers the find/replace/file-workflow/close-workflow branches through `SurfaceContentResolver.resolve`, but never
  * the fallback `modalLines` path -- `Modal.GotoLine` and `Modal.Custom` never appear there at all. Lives in this
  * package because the resolver is `private[layout]`.
  */
class ModalWorkflowContentResolverSpec extends AnyFlatSpec with Matchers:

  "resolve" should "render a GotoLine modal as its label and current input, via the fallback modalLines path" in {
    val resolved = ModalWorkflowContentResolver.resolve(
      Modal.GotoLine("42"),
      LayoutRect(0, 0, 40, 10),
      SurfaceRenderMode.Floating
    )

    resolved.rows.map(_.plainText) shouldBe List("goto-line", "42")
    resolved.title shouldBe None
    resolved.header shouldBe None
  }

  it should "render a Custom modal as its name and input, via the fallback modalLines path" in {
    val resolved = ModalWorkflowContentResolver.resolve(
      Modal.Custom("confirm-quit", "y"),
      LayoutRect(0, 0, 40, 10),
      SurfaceRenderMode.Pinned
    )

    resolved.rows.map(_.plainText) shouldBe List("confirm-quit", "y")
    // Pinned mode still yields no title here -- the fallback path never calls `titleFor`.
    resolved.title shouldBe None
  }

  it should "render Find as a query row with a cursor at the end of the typed text" in {
    val resolved = ModalWorkflowContentResolver.resolve(
      Modal.Find("needle", Nil, 0),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    resolved.header.map(_.plainText) shouldBe Some("find")
    resolved.rows.head.plainText shouldBe "Find needle"
    resolved.rows.head.cursorColumn shouldBe Some("Find needle".length)
    resolved.footer.map(_.plainText) shouldBe Some("0 matches")
  }

  it should "produce no content for CloseWorkflow, leaving composition to the shared modal chrome" in {
    val resolved = ModalWorkflowContentResolver.resolve(
      Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, BufferId(1), "notes.scala")),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    resolved shouldBe ResolvedSurfaceContent()
  }

  it should "render ReplaceWorkflow's active field as the selected split row" in {
    val workflow = ReplaceWorkflowState(
      findText = "old",
      replacementText = "new",
      activeField = ReplaceWorkflowField.Find
    )

    val resolved = ModalWorkflowContentResolver.resolve(
      Modal.ReplaceWorkflow(workflow),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    resolved.rows.head.selected shouldBe true
    resolved.rows(1).selected shouldBe false
  }

  it should "render FileWorkflow rows and use its operation label as both title and header" in {
    val workflow = FileWorkflowState(mode = FileWorkflowMode.Open, filename = "notes.scala", path = "/tmp")

    val resolved = ModalWorkflowContentResolver.resolve(
      Modal.FileWorkflow(workflow),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Pinned
    )

    resolved.title shouldBe Some("open")
    resolved.header.map(_.plainText) shouldBe Some("open")
  }

  "resolveFileWorkflow (via resolve)" should "render a single empty path segment for an empty path" in {
    val workflow = FileWorkflowState(mode = FileWorkflowMode.SaveAs, filename = "notes.scala", path = "")

    val resolved = ModalWorkflowContentResolver.resolve(
      Modal.FileWorkflow(workflow),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    val pathRow = resolved.rows(1)
    pathRow.plainText shouldBe "Path "
    // "Path" label segment plus one empty path segment.
    pathRow.segments should have size 2
  }

  it should "mark path segments matching missingPathSegments with an error tone" in {
    val workflow = FileWorkflowState(
      mode = FileWorkflowMode.SaveAs,
      filename = "notes.scala",
      path = "a/b/c",
      activeField = FileWorkflowField.Path,
      missingPathSegments = List("b", "c")
    )

    val resolved = ModalWorkflowContentResolver.resolve(
      Modal.FileWorkflow(workflow),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    val pathSegments = resolved.rows(1).segments.tail // drop the leading "Path" label
    pathSegments.map(_.text) shouldBe List("a", "b", "c")
    pathSegments.map(_.tone) shouldBe List(OverlayTone.Normal, OverlayTone.Error, OverlayTone.Error)
  }

  it should "suffix a directory suggestion with a trailing slash" in {
    val workflow = FileWorkflowState(
      mode = FileWorkflowMode.Open,
      filename = "",
      path = "/tmp",
      suggestions =
        List(FileWorkflowSuggestion("file.txt", isDirectory = false), FileWorkflowSuggestion("dir", isDirectory = true))
    )

    val resolved = ModalWorkflowContentResolver.resolve(
      Modal.FileWorkflow(workflow),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    resolved.rows.drop(2).map(_.plainText) shouldBe List("file.txt", "dir/")
  }
