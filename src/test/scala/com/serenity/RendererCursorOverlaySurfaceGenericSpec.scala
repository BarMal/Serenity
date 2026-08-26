package com.serenity

import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.Renderer
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Covers the surface-generic `renderCursorOnly`/`renderWithCursorOverlay` entry points added for #1104: before this,
  * both methods only took a `SwingWindow` and so could only be exercised through a live Swing canvas. Driving them
  * against `MockRenderSurface` here is the whole point of the extraction -- a shell with no `SwingWindow` (a terminal)
  * needs the exact same cursor-only and base-plus-cursor render paths GUI mode already relies on.
  */
class RendererCursorOverlaySurfaceGenericSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val paneId      = PaneId(0)
  private val bufferId    = BufferId(1)
  private val viewport    = ViewportSize(80, 24)
  private val codeFont    = java.awt.Font(java.awt.Font.MONOSPACED, java.awt.Font.PLAIN, 12)
  private val cellMetrics = com.serenity.ui.layout.CellMetrics.fromFont(codeFont)

  private def editorState: AppState =
    val buffer =
      Buffer.fromString(bufferId, "hello world").copy(editing = EditingState(cursors = List(CursorPosition(0, 3))))
    AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = AppState.initial.layout.copy(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      theme = Theme.light
    )

  /** Every string the frame asked the surface to paint, whichever text path it took. */
  private def drawnText(surface: MockRenderSurface): List[String] =
    surface.putStringCalls.map(_.s) ++ surface.drawRunPxCalls.map(_.s)

  private def drew(surface: MockRenderSurface, text: String): Boolean =
    drawnText(surface).exists(_.contains(text))

  private def startPageState: AppState =
    val surfaceId = SurfaceId("start-page")
    AppState.initial.copy(
      uiSurfaces = List(
        UiSurface(
          id = surfaceId,
          content = SurfaceContent.StartPage(StartupPage(title = "Serenity", options = Nil, selectedIndex = 0)),
          presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      ),
      focus = Focus.Surface(surfaceId)
    )

  "Renderer.renderCursorOnly (surface-generic)" should "draw the cursor into the given surface and report success" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)
    Renderer.render(editorState, cursorVisible = false, surface, viewport, codeFont, codeFont, cellMetrics, None)
    surface.fillPixelRectCalls shouldBe empty

    val rendered = Renderer.renderCursorOnly(
      editorState,
      cursorVisible = true,
      surface,
      viewport,
      codeFont,
      codeFont,
      codeFont,
      cellMetrics,
      cellMetrics,
      None
    )

    rendered shouldBe true
    surface.fillPixelRectCalls should not be empty
  }

  it should "report no cursor drawn for a start-page-only state" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)

    val rendered = Renderer.renderCursorOnly(
      startPageState,
      cursorVisible = true,
      surface,
      viewport,
      codeFont,
      codeFont,
      codeFont,
      cellMetrics,
      cellMetrics,
      None
    )

    rendered shouldBe false
    surface.fillPixelRectCalls shouldBe empty
  }

  "Renderer.renderWithCursorOverlay (surface-generic)" should "draw both the base content and the cursor into the same surface" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)

    val rendered = Renderer.renderWithCursorOverlay(
      editorState,
      surface,
      viewport,
      codeFont,
      codeFont,
      codeFont,
      cellMetrics,
      cellMetrics,
      None
    )

    rendered shouldBe true
    drew(surface, "hello world") shouldBe true
    surface.fillPixelRectCalls should not be empty
  }

  it should "render the start page and report success even though it has no cursor" in {
    val surface = new MockRenderSurface(80, 24, persistentContent = true)

    val rendered = Renderer.renderWithCursorOverlay(
      startPageState,
      surface,
      viewport,
      codeFont,
      codeFont,
      codeFont,
      cellMetrics,
      cellMetrics,
      None
    )

    rendered shouldBe true
    surface.fillPixelRectCalls shouldBe empty
  }
