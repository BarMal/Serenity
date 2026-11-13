package com.serenity

import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.layout.{ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.renderer.RendererEntryPoints
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A modal and a floating panel are painted straight onto the frame: their text is drawn, and stays drawn when a later
  * frame only changes the editor's content.
  */
class PanelAndModalDirectPaintSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val modalId  = SurfaceId("close-confirmation")
  private val peekId   = SurfaceId("peek")

  private def modalDialog: ModalDialog =
    ModalDialog(modalId, Modal.Confirm(ConfirmPrompt.closeUnsaved("notes.scala")), ModalPlacement.Centered)

  private def floatingPanel: UiSurface =
    UiSurface(
      peekId,
      SurfaceContent.QuickInfo("hover text"),
      SurfacePresentation.Floating(Some(CursorPosition(1, 1)), SurfacePlacement.AboveCursor)
    )

  private def stateWith(content: String, focus: Focus, surfaces: List[UiSurface], modals: List[ModalDialog]): AppState =
    val buffer = Buffer.fromString(bufferId, content)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = focus
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = surfaces, modalStack = modals)
    )

  private def editContent(state: AppState): AppState =
    val edited = state.persisted
      .buffers(bufferId)
      .document
      .content
      .insert(0, "X")
      .getOrElse(fail("expected insert to succeed"))
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(
          bufferId,
          state.persisted
            .buffers(bufferId)
            .copy(document = state.persisted.buffers(bufferId).document.copy(content = edited))
        )
      )
    )

  private def drawnText(surface: MockRenderSurface): List[String] =
    surface.putStringCalls.map(_.s).toList ++ surface.drawRunPxCalls.map(_.s)

  "RendererEntryPoints.render" should "paint the modal straight onto the frame" in {
    val surface = new MockRenderSurface(80, 24)
    val state   = stateWith("alpha\nbeta\ngamma", Focus.Modal, Nil, List(modalDialog))
    val caches  = com.serenity.state.manager.RenderCaches.create()

    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      ViewportSize(80, 24),
      None,
      Damage.Everything,
      caches
    )

    drawnText(surface).exists(_.contains("notes.scala")) shouldBe true
  }

  it should "paint a floating panel straight onto the frame, and again after the editor content changes" in {
    val surface  = new MockRenderSurface(120, 40)
    val before   = stateWith("alpha\nbeta\ngamma", Focus.EditorPane(paneId), List(floatingPanel), Nil)
    val caches   = com.serenity.state.manager.RenderCaches.create()
    val viewport = ViewportSize(120, 40)

    RendererEntryPoints.render(before, cursorVisible = false, surface, viewport, None, Damage.Everything, caches)
    val after = editContent(before)
    RendererEntryPoints.render(
      after,
      cursorVisible = false,
      surface,
      viewport,
      None,
      DamageProducer.forTransition(before, after),
      caches
    )

    drawnText(surface).exists(_.contains("hover text")) shouldBe true
  }

end PanelAndModalDirectPaintSpec
