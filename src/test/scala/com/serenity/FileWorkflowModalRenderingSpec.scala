package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{CursorLayout, Layout, LayoutEngine, ViewportSize}
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FileWorkflowModalRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  "Renderer.render" should "paint file workflow fields, suggestions, and confirmation footer through the shared modal surface" in {
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
    val buffer = Buffer.fromString(bufferId, "alpha\nbeta\ngamma").copy(
      cursors = List(CursorPosition(1, 2))
    )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("file-modal")),
      theme = Theme.light,
      uiSurfaces = List(
        UiSurface(
          SurfaceId("file-modal"),
          SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))
    val paneRect = LayoutEngine
      .calculatePaneLayouts(state, layout)
      .getOrElse(paneId, fail("Expected pane layout"))
    val contentRect = CursorLayout.contentRectForPane(paneRect)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    val filenameLine =
      (overlay.x + 1 until overlay.right - 1).map(x => surface.getChar(x, overlay.y + 2)).mkString.trim
    val pathLine =
      (overlay.x + 1 until overlay.right - 1).map(x => surface.getChar(x, overlay.y + 3)).mkString.trim
    val innerLines =
      (overlay.y + 1 until overlay.bottom - 1).toList.map { y =>
        (overlay.x + 1 until overlay.right - 1).map(x => surface.getChar(x, y)).mkString.trim
      }

    filenameLine should include("Filename")
    filenameLine should include("notes.scala")
    pathLine should include("Path")
    pathLine should include("tmp")
    pathLine should include("project")
    innerLines.exists(_.contains("/tmp/project/new/")) shouldBe true
    innerLines.exists(_.contains("Create directories: new / nested")) shouldBe true
    overlay.x shouldBe contentRect.x
    overlay.width shouldBe contentRect.width

    val suggestionRowsHighlighted =
      List(overlay.y + 4, overlay.y + 5).exists { y =>
        surface.getBg(overlay.x + 1, y) == state.theme.highlighted.background
      }
    suggestionRowsHighlighted shouldBe true
  }

  it should "paint file workflow status messages when a target cannot be opened" in {
    val workflow = FileWorkflowState(
      mode = FileWorkflowMode.Open,
      filename = "missing.scala",
      path = "/tmp/project",
      statusMessage = Some("File not found: /tmp/project/missing.scala")
    )
    val buffer = Buffer.fromString(bufferId, "alpha\nbeta\ngamma").copy(
      cursors = List(CursorPosition(1, 2))
    )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.Surface(SurfaceId("file-modal")),
      theme = Theme.light,
      uiSurfaces = List(
        UiSurface(
          SurfaceId("file-modal"),
          SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)),
          SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
        )
      )
    )
    val surface = new MockRenderSurface(100, 30)
    val layout  = LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val overlay = layout.belowCursorOverlayRect.getOrElse(fail("Expected below-cursor overlay rect"))
    val paneRect = LayoutEngine
      .calculatePaneLayouts(state, layout)
      .getOrElse(paneId, fail("Expected pane layout"))
    val contentRect = CursorLayout.contentRectForPane(paneRect)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    val innerLines =
      (overlay.y + 1 until overlay.bottom - 1).toList.map { y =>
        (overlay.x + 1 until overlay.right - 1).map(x => surface.getChar(x, y)).mkString.trim
      }

    innerLines.mkString(" ") should include("File not found:")
    innerLines.mkString(" ") should include("missing.scala")
    overlay.x shouldBe contentRect.x
    overlay.width shouldBe contentRect.width
  }
