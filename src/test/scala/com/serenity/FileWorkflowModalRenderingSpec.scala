package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
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
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(
        editing = EditingState(cursors = List(CursorPosition(1, 2)))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.Surface(SurfaceId("file-modal")),
        theme = Theme.light
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("file-modal"),
            SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
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
    // Centered, fixed-width like the command palette (issue #1253) -- no longer spans the full editor-pane width.
    overlay.width shouldBe math.min(contentRect.width, 72)
    overlay.x shouldBe (contentRect.x + math.max(0, (contentRect.width - overlay.width) / 2))

    val suggestionRowsHighlighted =
      List(overlay.y + 5, overlay.y + 6).exists { y =>
        surface.getBg(overlay.x + 1, y) == state.persisted.theme.highlighted.background
      }
    suggestionRowsHighlighted shouldBe true

    // Save As renders Format as a selectable input row (no colon), matching the Filename/Path pattern above it --
    // Open (below) keeps the plain "Format:" label since it has no format concept.
    val formatLine = (overlay.x + 1 until overlay.right - 1).map(x => surface.getChar(x, overlay.y + 4)).mkString.trim
    formatLine should include("Format")
    formatLine should include("Scala")
  }

  it should "paint file workflow status messages when a target cannot be opened" in {
    val workflow = FileWorkflowState(
      mode = FileWorkflowMode.Open,
      filename = "missing.scala",
      path = "/tmp/project",
      statusMessage = Some("File not found: /tmp/project/missing.scala")
    )
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(
        editing = EditingState(cursors = List(CursorPosition(1, 2)))
      )
    val pane = EditorPane.withBuffer(paneId, bufferId)
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.Surface(SurfaceId("file-modal")),
        theme = Theme.light
      ),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("file-modal"),
            SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)),
            SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
          )
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
    // Centered, fixed-width like the command palette (issue #1253) -- no longer spans the full editor-pane width.
    overlay.width shouldBe math.min(contentRect.width, 72)
    overlay.x shouldBe (contentRect.x + math.max(0, (contentRect.width - overlay.width) / 2))
  }

  it should "dim the workspace behind a blocking modal without depending on motion settings" in {
    val close = UiSurface(
      SurfaceId("close-confirmation"),
      SurfaceContent.ModalWorkflow(
        Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, bufferId, "notes.scala"))
      ),
      SurfacePresentation.Modal
    )
    val state = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(close.id)),
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(close))
    )
    val surface = new MockRenderSurface(100, 30)

    Renderer.render(state, cursorVisible = true, surface, ViewportSize(100, 30))

    surface.alphaCalls should contain(0.4f)
    surface.currentAlphaValue shouldBe 1.0f
  }

  it should "paint the open dialog as a modal over the startup page" in {
    val startPage = StartupPage(
      title = "Welcome to Serenity",
      options = List("New document", "Open file or folder"),
      selectedIndex = 1
    )
    val workflow = FileWorkflowState(
      mode = FileWorkflowMode.Open,
      path = "/home/user",
      activeField = FileWorkflowField.Path
    )
    val startupId = SurfaceId("surface-0")
    val modalId   = SurfaceId("surface-1")
    val state = AppState.empty.copy(
      persisted = AppState.empty.persisted.copy(
        focus = Focus.Surface(modalId),
        theme = Theme.light
      ),
      runtime = AppState.empty.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            startupId,
            SurfaceContent.StartPage(startPage),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          ),
          UiSurface(modalId, SurfaceContent.ModalWorkflow(Modal.FileWorkflow(workflow)), SurfacePresentation.Modal)
        )
      )
    )
    val viewportSize = ViewportSize(100, 30)
    val surface      = new MockRenderSurface(100, 30)
    val layout       = LayoutEngine.calculateLayout(state, viewportSize)
    val modalSurface = state.runtime.uiSurfaces.find(_.id == modalId).get
    val modalRect    = LayoutEngine.calculateModalRect(modalSurface, state, layout)

    Renderer.render(state, cursorVisible = true, surface, viewportSize)

    val innerLines =
      (modalRect.y + 1 until modalRect.bottom - 1).toList.map { y =>
        (modalRect.x + 1 until modalRect.right - 1).map(x => surface.getChar(x, y)).mkString.trim
      }
    innerLines.exists(_.contains("Path")) shouldBe true
  }
