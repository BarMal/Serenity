package com.serenity

import java.awt.image.BufferedImage

import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.layout.{ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.renderer.{LayerBufferSupport, RenderSurface, RendererEntryPoints, ScreenIdentity}
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

  private def modalDialog: ModalDialog =
    ModalDialog(
      modalId,
      Modal.Confirm(ConfirmPrompt.closeUnsaved("notes.scala")),
      ModalPlacement.Centered
    )

  private def stateWith(content: String, modal: ModalDialog, layerCaching: Boolean = true): AppState =
    val buffer = Buffer.fromString(bufferId, content)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppState.initial.persisted.config.withLayerCaching(layerCaching),
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.Modal
      ),
      runtime = AppState.initial.runtime.copy(modalStack = List(modal))
    )

  "RendererEntryPoints.render" should "not repaint the modal layer's own buffer when only editor content changed" in {
    val surface = new CountingLayerBufferSurface(80, 24)
    val before  = stateWith("alpha\nbeta\ngamma", modalDialog)
    // Shared across both render calls below (issue #1677): a real `StateManager` reuses one `RenderCaches` across
    // every frame it renders, and it's exactly that reuse -- not a JVM-wide singleton -- these modal-layer-buffer
    // reuse assertions depend on.
    val caches = com.serenity.state.manager.RenderCaches.create()

    RendererEntryPoints.render(
      before,
      cursorVisible = false,
      surface,
      viewport,
      None,
      Damage.Everything,
      caches
    )
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

    RendererEntryPoints.render(
      after,
      cursorVisible = false,
      surface,
      viewport,
      None,
      DamageProducer.forTransition(before, after),
      caches
    )

    surface.newLayerSurfaceCalls.get() shouldBe 1
    val drawnText = surface.putStringCalls.map(_.s) ++ surface.drawRunPxCalls.map(_.s)
    drawnText.exists(_.contains("Xalpha")) shouldBe true
  }

  it should "repaint the modal layer's buffer when only the modal's own content changes" in {
    val surface = new CountingLayerBufferSurface(80, 24)
    val before  = stateWith("alpha\nbeta\ngamma", modalDialog)
    val caches  = com.serenity.state.manager.RenderCaches.create()

    RendererEntryPoints.render(
      before,
      cursorVisible = false,
      surface,
      viewport,
      None,
      Damage.Everything,
      caches
    )
    surface.newLayerSurfaceCalls.get() shouldBe 1

    val changedModal =
      modalDialog.copy(modal = Modal.Confirm(ConfirmPrompt.closeUnsaved("renamed.scala")))
    val after = before.copy(runtime = before.runtime.copy(modalStack = List(changedModal)))

    val transitionDamage = DamageProducer.forTransition(before, after)
    transitionDamage shouldBe Damage.Surface(modalId)

    RendererEntryPoints.render(
      after,
      cursorVisible = false,
      surface,
      viewport,
      None,
      transitionDamage,
      caches
    )

    surface.newLayerSurfaceCalls.get() shouldBe 2
  }

  it should "repaint the modal layer when the bottom of a two-deep modal stack changes, not just the top (#814)" in {
    val surface = new CountingLayerBufferSurface(80, 24)
    val parent  = modalDialog
    val childId = SurfaceId("save-before-close")
    val child = ModalDialog(
      childId,
      Modal.FileWorkflow(FileWorkflowState(mode = FileWorkflowMode.SaveAs, filename = "notes.scala")),
      ModalPlacement.Centered
    )
    val before          = stateWith("alpha\nbeta\ngamma", parent)
    val beforeWithStack = before.copy(runtime = before.runtime.copy(modalStack = List(parent, child)))
    val caches          = com.serenity.state.manager.RenderCaches.create()

    RendererEntryPoints.render(
      beforeWithStack,
      cursorVisible = false,
      surface,
      viewport,
      None,
      Damage.Everything,
      caches
    )
    surface.newLayerSurfaceCalls.get() shouldBe 1

    // Only the bottom (parent) dialog's content changes; the top (child) is untouched.
    val changedParent =
      parent.copy(modal = Modal.Confirm(ConfirmPrompt.closeUnsaved("renamed.scala")))
    val after = beforeWithStack.copy(runtime = beforeWithStack.runtime.copy(modalStack = List(changedParent, child)))

    val transitionDamage = DamageProducer.forTransition(beforeWithStack, after)
    transitionDamage shouldBe Damage.Surface(parent.id)

    RendererEntryPoints.render(
      after,
      cursorVisible = false,
      surface,
      viewport,
      None,
      transitionDamage,
      caches
    )

    surface.newLayerSurfaceCalls.get() shouldBe 2
  }

  it should "reuse the cached modal buffer's pixels: composited output matches a fresh repaint" in {
    val surface = new CountingLayerBufferSurface(80, 24)
    val state   = stateWith("alpha\nbeta\ngamma", modalDialog)
    val caches  = com.serenity.state.manager.RenderCaches.create()

    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      None,
      Damage.Everything,
      caches
    )
    val firstDrawImageCalls = surface.drawImageCalls.size
    firstDrawImageCalls should be > 0

    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      None,
      DamageProducer.forTransition(state, state),
      caches
    )

    surface.newLayerSurfaceCalls.get() shouldBe 1
    surface.drawImageCalls.size shouldBe firstDrawImageCalls + 1
  }

  it should "reuse the cached modal layer across frames painted on different surfaces of the same window" in {
    val window      = new Object
    val firstFrame  = new CountingLayerBufferSurface(80, 24, Some(window))
    val secondFrame = new CountingLayerBufferSurface(80, 24, Some(window))
    val state       = stateWith("alpha\nbeta\ngamma", modalDialog)
    val caches      = com.serenity.state.manager.RenderCaches.create()

    RendererEntryPoints.render(state, cursorVisible = false, firstFrame, viewport, None, Damage.Everything, caches)
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      secondFrame,
      viewport,
      None,
      DamageProducer.forTransition(state, state),
      caches
    )

    firstFrame.newLayerSurfaceCalls.get() shouldBe 1
    secondFrame.newLayerSurfaceCalls.get() shouldBe 0
    secondFrame.drawImageCalls.size shouldBe 1
  }

  it should "paint the modal straight onto the frame, with no layer buffer, when layer caching is off" in {
    val surface = new CountingLayerBufferSurface(80, 24)
    val state   = stateWith("alpha\nbeta\ngamma", modalDialog, layerCaching = false)
    val caches  = com.serenity.state.manager.RenderCaches.create()

    RendererEntryPoints.render(state, cursorVisible = false, surface, viewport, None, Damage.Everything, caches)

    surface.newLayerSurfaceCalls.get() shouldBe 0
    caches.frameState.cachedModalLayerFor(surface) shouldBe None
    val drawnText = surface.putStringCalls.map(_.s) ++ surface.drawRunPxCalls.map(_.s)
    drawnText.exists(_.contains("notes.scala")) shouldBe true
  }

  it should "repaint into the previous modal image rather than allocating a new one" in {
    val surface = new CountingLayerBufferSurface(80, 24)
    val before  = stateWith("alpha\nbeta\ngamma", modalDialog)
    val caches  = com.serenity.state.manager.RenderCaches.create()

    RendererEntryPoints.render(before, cursorVisible = false, surface, viewport, None, Damage.Everything, caches)
    val firstImage = caches.frameState.cachedModalLayerFor(surface).map(_.image)

    val changedModal = modalDialog.copy(modal = Modal.Confirm(ConfirmPrompt.closeUnsaved("renamed.scala")))
    val after        = before.copy(runtime = before.runtime.copy(modalStack = List(changedModal)))
    RendererEntryPoints.render(
      after,
      cursorVisible = false,
      surface,
      viewport,
      None,
      DamageProducer.forTransition(before, after),
      caches
    )

    surface.newLayerSurfaceCalls.get() shouldBe 2
    firstImage should not be empty
    surface.recycledImages.lastOption.flatten shouldBe firstImage
  }

  /** A [[MockRenderSurface]] that also advertises [[LayerBufferSupport]] -- exercising the same
    * `context.surface.layerBuffers`-gated path `Java2DRenderSurface` takes in production, while keeping the char/bg
    * grid assertions [[MockRenderSurface]] already gives tests. `newLayerSurface` hands back a fresh inner
    * `MockRenderSurface` whose `flush()` -- unlike the base class's no-op -- actually invokes `onFlush`, matching what
    * a real offscreen surface does.
    */
  private class CountingLayerBufferSurface(width: Int, height: Int, window: Option[AnyRef] = None)
      extends MockRenderSurface(width, height):
    val newLayerSurfaceCalls = new java.util.concurrent.atomic.AtomicInteger(0)
    val recycledImages       = scala.collection.mutable.ListBuffer.empty[Option[BufferedImage]]

    override def layerCacheOwner: ScreenIdentity = window.fold(super.layerCacheOwner)(ScreenIdentity(_))

    override def layerBuffers: Option[LayerBufferSupport] = Some(
      new LayerBufferSupport:
        def newLayerSurface(onFlush: BufferedImage => Unit, recycled: Option[BufferedImage]): RenderSurface =
          newLayerSurfaceCalls.incrementAndGet()
          recycledImages += recycled
          new FlushingLayerSurface(width, height, onFlush, recycled)
    )

  /** Flushes the recycled image when there is one, as [[com.serenity.ui.renderer.Java2DRenderSurface.forLayer]] does.
    */
  private class FlushingLayerSurface(
      width: Int,
      height: Int,
      onFlush: BufferedImage => Unit,
      recycled: Option[BufferedImage]
  ) extends MockRenderSurface(width, height):
    override def flush(): Unit = onFlush(recycled.getOrElse(new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)))
