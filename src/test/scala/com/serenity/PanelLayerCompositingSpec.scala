package com.serenity

import java.awt.image.BufferedImage
import java.nio.file.Paths

import com.serenity.config.MaterialPreset
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{LayerBufferSupport, RenderSurface, Renderer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1100 stage 3: pinned, expanded, and floating panels each own their own layer buffer and skip repainting it when
  * it's safe to -- the generalisation of [[ModalLayerCompositingSpec]] beyond the modal layer.
  *
  * Unlike the modal, a panel's paint step samples the pixels behind it (`SurfaceMaterials.effectiveBlurRadius`'s
  * `blurRegion` call), so its cache is safe to reuse only when either blur is off (the modal's own narrower per-surface
  * rule applies unchanged) or the whole frame's damage is `Damage.Nothing` (see `Renderer.panelDirtyCheck`). The
  * `AppConfig.default` material preset (`Frosted`) has blur active, so most of these fixtures disable it explicitly to
  * exercise the narrower per-surface reuse rule the same way [[ModalLayerCompositingSpec]] does for the modal.
  */
class PanelLayerCompositingSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val paneId     = PaneId(0)
  private val bufferId   = BufferId(1)
  private val viewport   = ViewportSize(120, 40)
  private val pinnedId   = SurfaceId("outline")
  private val floatingId = SurfaceId("peek")

  private def pinnedPanel: UiSurface =
    UiSurface.fromPanelContent(
      pinnedId,
      PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/repo")), None),
      PanelPosition.Left,
      24
    )

  private def floatingPanel: UiSurface =
    UiSurface(
      floatingId,
      SurfaceContent.QuickInfo("hover text"),
      SurfacePresentation.Floating(Some(CursorPosition(1, 1)), SurfacePlacement.AboveCursor)
    )

  private def stateWith(content: String, surfaces: List[UiSurface], blurOff: Boolean = true): AppState =
    val buffer = Buffer.fromString(bufferId, content)
    val bare = AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.EditorPane(paneId)
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = surfaces)
    )
    if blurOff then
      bare.copy(persisted =
        bare.persisted.copy(config = bare.persisted.config.withMaterialPreset(MaterialPreset.Solid))
      )
    else bare

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

  "Renderer.render" should "not repaint a pinned panel's own buffer when only editor content changed and blur is off" in {
    val surface = new CountingLayerBufferSurface(120, 40)
    val before  = stateWith("alpha\nbeta\ngamma", List(pinnedPanel), blurOff = true)

    Renderer.render(before, cursorVisible = false, surface, viewport, None, Damage.Everything)
    surface.newSeededLayerSurfaceCalls.get() shouldBe 1

    val after = editContent(before)
    Renderer.render(after, cursorVisible = false, surface, viewport, None, DamageProducer.forTransition(before, after))

    surface.newSeededLayerSurfaceCalls.get() shouldBe 1
  }

  it should "repaint a pinned panel's buffer when only its own content changes and blur is off" in {
    val surface = new CountingLayerBufferSurface(120, 40)
    val before  = stateWith("alpha\nbeta\ngamma", List(pinnedPanel), blurOff = true)

    Renderer.render(before, cursorVisible = false, surface, viewport, None, Damage.Everything)
    surface.newSeededLayerSurfaceCalls.get() shouldBe 1

    val changed          = pinnedPanel.copy(dismissOnMove = true)
    val after            = before.copy(runtime = before.runtime.copy(uiSurfaces = List(changed)))
    val transitionDamage = DamageProducer.forTransition(before, after)
    transitionDamage shouldBe Damage.Surface(pinnedId)

    Renderer.render(after, cursorVisible = false, surface, viewport, None, transitionDamage)

    surface.newSeededLayerSurfaceCalls.get() shouldBe 2
  }

  it should "repaint a pinned panel with active blur whenever anything elsewhere in the frame changed" in {
    val surface = new CountingLayerBufferSurface(120, 40)
    val before  = stateWith("alpha\nbeta\ngamma", List(pinnedPanel), blurOff = false)

    Renderer.render(before, cursorVisible = false, surface, viewport, None, Damage.Everything)
    surface.newSeededLayerSurfaceCalls.get() shouldBe 1

    val after            = editContent(before)
    val transitionDamage = DamageProducer.forTransition(before, after)
    transitionDamage should not be Damage.Nothing

    Renderer.render(after, cursorVisible = false, surface, viewport, None, transitionDamage)

    // Blur samples the live frame, so an unrelated content change still forces this panel to repaint.
    surface.newSeededLayerSurfaceCalls.get() shouldBe 2
  }

  it should "reuse a blurred pinned panel's cached buffer on a truly clean re-render" in {
    val surface = new CountingLayerBufferSurface(120, 40)
    val state   = stateWith("alpha\nbeta\ngamma", List(pinnedPanel), blurOff = false)

    Renderer.render(state, cursorVisible = false, surface, viewport, None, Damage.Everything)
    val firstDrawImageCalls = surface.drawImageCalls.size
    firstDrawImageCalls should be > 0

    Renderer.render(state, cursorVisible = false, surface, viewport, None, DamageProducer.forTransition(state, state))

    surface.newSeededLayerSurfaceCalls.get() shouldBe 1
    surface.drawImageCalls.size shouldBe firstDrawImageCalls + 1
  }

  it should "not repaint an expanded panel's own buffer when only editor content changed and blur is off" in {
    val surface    = new CountingLayerBufferSurface(120, 40)
    val expandedId = SurfaceId("expanded-outline")
    val expanded = UiSurface(
      expandedId,
      SurfaceContent.Outline(Nil),
      SurfacePresentation.Expanded(PanelPosition.Right, 22)
    )
    val before = stateWith("alpha\nbeta\ngamma", List(expanded), blurOff = true)

    Renderer.render(before, cursorVisible = false, surface, viewport, None, Damage.Everything)
    surface.newSeededLayerSurfaceCalls.get() shouldBe 1

    val after = editContent(before)
    Renderer.render(after, cursorVisible = false, surface, viewport, None, DamageProducer.forTransition(before, after))

    surface.newSeededLayerSurfaceCalls.get() shouldBe 1
  }

  it should "not let a different render surface reusing the same SurfaceId disturb this surface's own cached panel buffer" in {
    // Renderer's panel layer cache used to be a single JVM-wide slot keyed only by SurfaceId, so two independently
    // rendered surfaces sharing a SurfaceId (as "outline" is, across a dozen specs) could stomp on each other's
    // cached image -- exactly the shape of the flake seen when this spec ran under sbt's default parallel-suite
    // execution alongside another spec painting a same-named panel. This reproduces that cross-surface interaction
    // deterministically, without depending on real thread scheduling.
    val surfaceA = new CountingLayerBufferSurface(120, 40)
    val before   = stateWith("alpha\nbeta\ngamma", List(pinnedPanel), blurOff = true)

    Renderer.render(before, cursorVisible = false, surfaceA, viewport, None, Damage.Everything)
    surfaceA.newSeededLayerSurfaceCalls.get() shouldBe 1

    // Stand in for a concurrently running render path -- another suite, another window -- painting a panel with the
    // *same* SurfaceId but a different frame shape.
    val surfaceB     = new CountingLayerBufferSurface(200, 60)
    val wideViewport = ViewportSize(200, 60)
    Renderer.render(before, cursorVisible = false, surfaceB, wideViewport, None, Damage.Everything)

    val after = editContent(before)
    Renderer.render(after, cursorVisible = false, surfaceA, viewport, None, DamageProducer.forTransition(before, after))

    surfaceA.newSeededLayerSurfaceCalls.get() shouldBe 1
  }

  it should "not repaint a floating panel's own buffer when only editor content changed and blur is off" in {
    val surface = new CountingLayerBufferSurface(120, 40)
    val before  = stateWith("alpha\nbeta\ngamma", List(floatingPanel), blurOff = true)

    Renderer.render(before, cursorVisible = false, surface, viewport, None, Damage.Everything)
    surface.newSeededLayerSurfaceCalls.get() shouldBe 1

    val after = editContent(before)
    Renderer.render(after, cursorVisible = false, surface, viewport, None, DamageProducer.forTransition(before, after))

    surface.newSeededLayerSurfaceCalls.get() shouldBe 1
  }

  it should "repaint a floating panel's buffer when only its own content changes and blur is off" in {
    val surface = new CountingLayerBufferSurface(120, 40)
    val before  = stateWith("alpha\nbeta\ngamma", List(floatingPanel), blurOff = true)

    Renderer.render(before, cursorVisible = false, surface, viewport, None, Damage.Everything)
    surface.newSeededLayerSurfaceCalls.get() shouldBe 1

    val changed          = floatingPanel.copy(content = SurfaceContent.QuickInfo("different text"))
    val after            = before.copy(runtime = before.runtime.copy(uiSurfaces = List(changed)))
    val transitionDamage = DamageProducer.forTransition(before, after)
    transitionDamage shouldBe Damage.Surface(floatingId)

    Renderer.render(after, cursorVisible = false, surface, viewport, None, transitionDamage)

    surface.newSeededLayerSurfaceCalls.get() shouldBe 2
  }

  /** A [[MockRenderSurface]] that also advertises [[LayerBufferSupport]] -- see
    * [[ModalLayerCompositingSpec.CountingLayerBufferSurface]] for why.
    */
  private class CountingLayerBufferSurface(width: Int, height: Int) extends MockRenderSurface(width, height):
    val newSeededLayerSurfaceCalls = new java.util.concurrent.atomic.AtomicInteger(0)

    override def layerBuffers: Option[LayerBufferSupport] = Some(
      new LayerBufferSupport:
        def newLayerSurface(onFlush: BufferedImage => Unit): RenderSurface =
          new FlushingLayerSurface(width, height, onFlush)

        def newSeededLayerSurface(onFlush: BufferedImage => Unit): RenderSurface =
          newSeededLayerSurfaceCalls.incrementAndGet()
          new FlushingLayerSurface(width, height, onFlush)
    )

  private class FlushingLayerSurface(width: Int, height: Int, onFlush: BufferedImage => Unit)
      extends MockRenderSurface(width, height):
    override def flush(): Unit = onFlush(new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB))

end PanelLayerCompositingSpec
