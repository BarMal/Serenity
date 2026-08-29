package com.serenity

import java.awt.image.BufferedImage

import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.{LayerBufferSupport, RenderSurface, Renderer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1100 stage 2: the modal layer owns its own buffer and skips repainting it when `DamageProducer` reports the
  * transition didn't touch the modal -- the seam #1100 stage 1 introduced but left every layer unable to safely use
  * ([[com.serenity.ui.renderer.LayerCompositor.dirtyLayers]] wasn't called from the render path at all).
  */
class ModalLayerCompositingSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val viewport = ViewportSize(80, 24)
  private val modalId  = SurfaceId("close-confirmation")

  private def modalSurface: UiSurface =
    UiSurface(
      modalId,
      SurfaceContent.ModalWorkflow(
        Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, bufferId, "notes.scala"))
      ),
      SurfacePresentation.Modal
    )

  private def stateWith(content: String, modal: UiSurface): AppState =
    val buffer = Buffer.fromString(bufferId, content)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          paneOrder = List(paneId)
        ),
        focus = Focus.Surface(modal.id)
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = List(modal))
    )

  "Renderer.render" should "not repaint the modal layer's own buffer when only editor content changed" in {
    val surface = new CountingLayerBufferSurface(80, 24)
    val before  = stateWith("alpha\nbeta\ngamma", modalSurface)

    Renderer.render(before, cursorVisible = false, surface, viewport, None, Damage.Everything)
    surface.newLayerSurfaceCalls.get() shouldBe 1

    val editedContent =
      before.persisted
        .buffers(bufferId)
        .document
        .content
        .insert(0, "X")
        .getOrElse(fail("expected insert to succeed"))
    val after = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(document = before.persisted.buffers(bufferId).document.copy(content = editedContent))
        )
      )
    )
    surface.clear()

    Renderer.render(
      after,
      cursorVisible = false,
      surface,
      viewport,
      None,
      DamageProducer.forTransition(before, after)
    )

    surface.newLayerSurfaceCalls.get() shouldBe 1
    val drawnText = surface.putStringCalls.map(_.s) ++ surface.drawRunPxCalls.map(_.s)
    drawnText.exists(_.contains("Xalpha")) shouldBe true
  }

  it should "repaint the modal layer's buffer when only the modal's own content changes" in {
    val surface = new CountingLayerBufferSurface(80, 24)
    val before  = stateWith("alpha\nbeta\ngamma", modalSurface)

    Renderer.render(before, cursorVisible = false, surface, viewport, None, Damage.Everything)
    surface.newLayerSurfaceCalls.get() shouldBe 1

    val changedModal = modalSurface.copy(content =
      SurfaceContent.ModalWorkflow(
        Modal.CloseWorkflow(CloseWorkflowState(CloseScope.Current, bufferId, "renamed.scala"))
      )
    )
    val after = before.copy(runtime = before.runtime.copy(uiSurfaces = List(changedModal)))

    val transitionDamage = DamageProducer.forTransition(before, after)
    transitionDamage shouldBe Damage.Surface(modalId)

    Renderer.render(after, cursorVisible = false, surface, viewport, None, transitionDamage)

    surface.newLayerSurfaceCalls.get() shouldBe 2
  }

  it should "reuse the cached modal buffer's pixels: composited output matches a fresh repaint" in {
    val surface = new CountingLayerBufferSurface(80, 24)
    val state   = stateWith("alpha\nbeta\ngamma", modalSurface)

    Renderer.render(state, cursorVisible = false, surface, viewport, None, Damage.Everything)
    val firstDrawImageCalls = surface.drawImageCalls.size
    firstDrawImageCalls should be > 0

    Renderer.render(state, cursorVisible = false, surface, viewport, None, DamageProducer.forTransition(state, state))

    surface.newLayerSurfaceCalls.get() shouldBe 1
    surface.drawImageCalls.size shouldBe firstDrawImageCalls + 1
  }

  /** A [[MockRenderSurface]] that also advertises [[LayerBufferSupport]] -- exercising the same
    * `context.surface.layerBuffers`-gated path `Java2DRenderSurface` takes in production, while keeping the char/bg
    * grid assertions [[MockRenderSurface]] already gives tests. `newLayerSurface` hands back a fresh inner
    * `MockRenderSurface` whose `flush()` -- unlike the base class's no-op -- actually invokes `onFlush`, matching what
    * a real offscreen surface does.
    */
  private class CountingLayerBufferSurface(width: Int, height: Int) extends MockRenderSurface(width, height):
    val newLayerSurfaceCalls = new java.util.concurrent.atomic.AtomicInteger(0)

    override def layerBuffers: Option[LayerBufferSupport] = Some(
      new LayerBufferSupport:
        def newLayerSurface(onFlush: BufferedImage => Unit): RenderSurface =
          newLayerSurfaceCalls.incrementAndGet()
          new FlushingLayerSurface(width, height, onFlush)
    )

  private class FlushingLayerSurface(width: Int, height: Int, onFlush: BufferedImage => Unit)
      extends MockRenderSurface(width, height):
    override def flush(): Unit = onFlush(new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB))
