package com.serenity

import com.serenity.command.*
import com.serenity.document.RenderedComment
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceContentResolverModalWorkflowSpec extends AnyFlatSpec with Matchers:

  "SurfaceContentResolver" should "resolve context menus into a selected command list" in {
    val save = Command.typed("save", "Save file", CommandIntent.File(FileIntent.SaveCurrentFile), label = "Save")
    val find = Command.typed("find", "Find text", CommandIntent.Edit(EditIntent.FindInCurrentFile), label = "Find")
    val menu = ContextMenu(
      title = "editor",
      targetFocus = Focus.EditorPane(PaneId(0)),
      items = List(
        ContextMenuItem(save.name, save.label, save),
        ContextMenuItem(find.name, find.label, find)
      ),
      selectedIndex = 1
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ContextMenu(menu),
      LayoutRect(0, 0, 28, 8),
      SurfaceRenderMode.Floating
    )

    floating.title shouldBe None
    floating.header.map(_.plainText) shouldBe Some("editor")
    floating.rows.map(_.plainText) shouldBe List("Save", "Find")
    floating.rows.map(_.selected) shouldBe List(false, true)
    floating.footer.map(_.plainText) shouldBe Some("2/2")
  }

  it should "reserve configured gap rows between context menu items" in {
    val save = Command.typed("save", "Save file", CommandIntent.File(FileIntent.SaveCurrentFile), label = "Save")
    val find = Command.typed("find", "Find text", CommandIntent.Edit(EditIntent.FindInCurrentFile), label = "Find")
    val menu = ContextMenu(
      title = "editor",
      targetFocus = Focus.EditorPane(PaneId(0)),
      items = List(
        ContextMenuItem(save.name, save.label, save),
        ContextMenuItem(find.name, find.label, find)
      )
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ContextMenu(menu),
      LayoutRect(0, 0, 28, 6),
      SurfaceRenderMode.Floating,
      itemGapRows = 1
    )

    floating.rows.map(_.plainText) shouldBe List("Save")
  }

  it should "resolve multiline comment lenses into editable draft rows" in {
    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.CommentLens(
        CommentLensState(
          RenderedComment(
            sourceLine = 4,
            raw = "/*\n* **Review** this value\n*/",
            inlineMarkdown = "Review this value\nbefore release"
          ),
          draft = "Review this value\nbefore release",
          cursor = "Review this value".length,
          target = None
        )
      ),
      LayoutRect(0, 0, 28, 8),
      SurfaceRenderMode.Floating
    )

    floating.title shouldBe None
    floating.header.map(_.plainText) shouldBe Some("comment")
    floating.rows.map(_.plainText) shouldBe List(
      "Review this value",
      "before release"
    )
    floating.rows.map(_.selected) shouldBe List(true, false)
    floating.rows.head.cursorColumn shouldBe Some("Review this value".length)
  }

  it should "resolve file workflow modals into field rows, suggestion rows, and a directory confirmation footer" in {
    val workflow = FileWorkflowState(
      mode = FileWorkflowMode.SaveAs,
      filename = "notes.scala",
      path = "/tmp/project/new/nested",
      activeField = FileWorkflowField.Path,
      suggestions = List(
        FileWorkflowSuggestion("/tmp/project", isDirectory = true),
        FileWorkflowSuggestion("/tmp/project/new", isDirectory = true)
      ),
      selectedSuggestionIndex = 1,
      missingPathSegments = List("new", "nested"),
      confirmCreateDirectories = true
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating.header.map(_.plainText) shouldBe Some("save-as")
    floating.rows should have size 4

    val filenameRow = floating.rows.head
    filenameRow.layout shouldBe OverlayRowLayout.Split
    filenameRow.segments.head.text shouldBe "Filename"
    filenameRow.segments.last.text shouldBe "notes.scala"

    val pathRow = floating.rows(1)
    pathRow.layout shouldBe OverlayRowLayout.Split
    pathRow.selected shouldBe true
    pathRow.segments.head.text shouldBe "Path"
    pathRow.segments.exists(_.tone == OverlayTone.Error) shouldBe true

    val suggestionRows = floating.rows.drop(2)
    suggestionRows.map(_.plainText) shouldBe List("/tmp/project/", "/tmp/project/new/")
    suggestionRows.count(_.selected) shouldBe 1
    suggestionRows.find(_.selected).map(_.plainText) shouldBe Some("/tmp/project/new/")

    floating.footer.map(_.plainText) shouldBe Some("Create directories: new / nested")
  }

  it should "render file workflow status messages as a visible footer when present" in {
    val workflow = FileWorkflowState(
      mode = FileWorkflowMode.Open,
      filename = "missing.scala",
      path = "/tmp/project",
      statusMessage = Some("File not found: /tmp/project/missing.scala")
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating.footer.map(_.plainText) shouldBe Some("File not found: /tmp/project/missing.scala")
  }

  it should "leave close workflow rendering to the shared modal composition" in {
    val workflow = CloseWorkflowState(
      scope = CloseScope.Current,
      currentBufferId = BufferId(7),
      currentBufferLabel = "notes.scala",
      selectedChoice = CloseWorkflowChoice.Discard
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.CloseWorkflow(workflow)),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating shouldBe ResolvedSurfaceContent()
  }

  it should "render replace workflow modals with separate find and replace rows" in {
    val workflow = ReplaceWorkflowState(
      findText = "needle",
      replacementText = "thread",
      activeField = ReplaceWorkflowField.ReplaceWith,
      selectedAction = ReplaceWorkflowAction.ReplaceNext,
      selectedScope = ReplaceWorkflowScope.Selection,
      statusMessage = Some("3 matches will be replaced")
    )

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating.header.map(_.plainText) shouldBe Some("replace")
    floating.rows should have size 4

    val findRow = floating.rows.head
    findRow.layout shouldBe OverlayRowLayout.Split
    findRow.segments.head.text shouldBe "Find"
    findRow.segments.last.text shouldBe "needle"
    findRow.selected shouldBe false

    val replaceRow = floating.rows(1)
    replaceRow.layout shouldBe OverlayRowLayout.Split
    replaceRow.segments.head.text shouldBe "Replace"
    replaceRow.segments.last.text shouldBe "thread"
    replaceRow.selected shouldBe true

    val actionRow = floating.rows(2)
    actionRow.layout shouldBe OverlayRowLayout.Distributed
    actionRow.segments.map(_.text) shouldBe List("Replace Next", "Replace All")
    actionRow.segments.find(_.selected).map(_.text) shouldBe Some("Replace Next")

    val scopeRow = floating.rows(3)
    scopeRow.layout shouldBe OverlayRowLayout.Distributed
    scopeRow.segments.map(_.text) shouldBe List("Current Buffer", "Selection")
    scopeRow.segments.find(_.selected).map(_.text) shouldBe Some("Selection")

    floating.footer.map(_.plainText) shouldBe Some("3 matches will be replaced")
  }

  // ── ThemePicker resolver ──────────────────────────────────────────────────

  it should "render find modals as focused query overlays" in {
    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.Find("needle", Nil, 0)),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating.header.map(_.plainText) shouldBe Some("find")
    floating.rows should have size 1

    val queryRow = floating.rows.head
    queryRow.layout shouldBe OverlayRowLayout.Split
    queryRow.selected shouldBe true
    queryRow.cursorColumn shouldBe Some("Find ".length + "needle".length)
    queryRow.segments.map(_.text) shouldBe List("Find", "needle")
    queryRow.segments.last.selected shouldBe true
    floating.footer.map(_.plainText) shouldBe Some("0 matches")
  }

  it should "render find result position when the modal carries match results" in {
    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(
        Modal.Find("needle", List(FindResult(2, 4), FindResult(5, 8), FindResult(8, 12)), 1)
      ),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating.header.map(_.plainText) shouldBe Some("find")
    floating.rows.head.plainText shouldBe "Find needle"
    floating.rows.map(_.plainText) shouldBe List("Find needle", "1. 3:5", "2. 6:9", "3. 9:13")
    floating.rows(2).selected shouldBe true
    floating.footer.map(_.plainText) shouldBe Some("3 matches, 2/3 at 6:9")
  }

  it should "render a find result window that keeps the selected match visible" in {
    val results = (0 until 6).map(line => FindResult(line, 0)).toList

    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.Find("needle", results, 4)),
      LayoutRect(0, 0, 60, 6),
      SurfaceRenderMode.Floating
    )

    floating.rows.map(_.plainText) shouldBe List("Find needle", "4. 4:1", "5. 5:1", "6. 6:1")
    floating.rows(2).selected shouldBe true
    floating.footer.map(_.plainText) shouldBe Some("6 matches, 5/6 at 5:1")
  }

  it should "render an explicit empty result state for find queries with no matches" in {
    val floating = SurfaceContentResolver.resolve(
      SurfaceContent.ModalWorkflow(Modal.Find("missing", Nil, 0)),
      LayoutRect(0, 0, 60, 12),
      SurfaceRenderMode.Floating
    )

    floating.header.map(_.plainText) shouldBe Some("find")
    floating.rows.map(_.plainText) shouldBe List("Find missing")
    floating.footer.map(_.plainText) shouldBe Some("0 matches")
  }

end SurfaceContentResolverModalWorkflowSpec
