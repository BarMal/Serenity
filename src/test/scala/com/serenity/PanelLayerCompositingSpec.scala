package com.serenity

import java.awt.image.BufferedImage
import java.nio.file.Paths

import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{LayerBufferSupport, RenderSurface, RendererEntryPoints, ScreenIdentity}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1100 stage 3: pinned, expanded, and floating panels each own their own layer buffer and skip repainting it when
  * it's safe to -- the generalisation of [[ModalLayerCompositingSpec]] beyond the modal layer.
  *
  * Like the modal, a panel never reads back the pixels behind it, so the same per-surface reuse rule applies (see
  * `RendererFramePlanner.panelDirtyCheck`).
  */
class PanelLayerCompositingSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val paneId     = PaneId(0)
  private val bufferId   = BufferId(1)
  private val viewport   = ViewportSize(120, 40)
  private val pinnedId   = SurfaceId("outline")
  private val floatingId = SurfaceId("peek")

  private def pinnedPanel: UiSurface =
    UiSurface.fromPanelContent(pinnedId, PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/repo")), None))

  /** Like `stateWith`, but also docks `pinnedPanel` into a real workspace tree at Left (issue #817: `pinnedSurfaces`,
    * and everything the renderer paints from it, now reads the tree -- a surface merely appended to `uiSurfaces`
    * without a tree entry is invisible to it).
    */
  private def stateWithPinnedPanel(content: String, layerCaching: Boolean = true): AppState =
    DockedPanelFixtures.dockExisting(
      stateWith(content, List(pinnedPanel), layerCaching),
      pinnedId,
      PanelPosition.Left,
      24
    )

  private def floatingPanel: UiSurface =
    UiSurface(
      floatingId,
      SurfaceContent.QuickInfo("hover text"),
      SurfacePresentation.Floating(Some(CursorPosition(1, 1)), SurfacePlacement.AboveCursor)
    )

  private def stateWith(
    content: String,
    surfaces: List[UiSurface],
    layerCaching: Boolean = true
  ): AppState =
    val buffer = Buffer.fromString(bufferId, content)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        config = AppState.initial.persisted.config.withLayerCaching(layerCaching),
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId)
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = surfaces)
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

  "RendererEntryPoints.render" should "not repaint a pinned panel's own buffer when only editor content changed" in {
    val surface = new CountingLayerBufferSurface(120, 40)
    val before  = stateWithPinnedPanel("alpha\nbeta\ngamma")
    // Shared across both render calls below (issue #1677): a real `StateManager` reuses one `RenderCaches` across
    // every frame it renders, and it's exactly that reuse -- not a JVM-wide singleton -- these panel-layer-buffer
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

    surface.newLayerSurfaceCalls.get() shouldBe 1
  }

  it should "repaint a pinned panel's buffer when only its own content changes" in {
    val surface = new CountingLayerBufferSurface(120, 40)
    val before  = stateWithPinnedPanel("alpha\nbeta\ngamma")
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

    val changed          = pinnedPanel.copy(dismissOnMove = true)
    val after            = before.copy(runtime = before.runtime.copy(uiSurfaces = List(changed)))
    val transitionDamage = DamageProducer.forTransition(before, after)
    transitionDamage shouldBe Damage.Surface(pinnedId)

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

  it should "reuse a pinned panel's cached buffer on a truly clean re-render" in {
    val surface = new CountingLayerBufferSurface(120, 40)
    val state   = stateWithPinnedPanel("alpha\nbeta\ngamma")
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

  it should "not repaint an expanded panel's own buffer when only editor content changed" in {
    val surface    = new CountingLayerBufferSurface(120, 40)
    val expandedId = SurfaceId("expanded-outline")
    val docked = DockedPanelFixtures.dock(
      stateWith("alpha\nbeta\ngamma", Nil),
      expandedId,
      SurfaceContent.Outline(Nil),
      PanelPosition.Right,
      22
    )
    val before = DockedPanelFixtures.expand(docked, expandedId)
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

    surface.newLayerSurfaceCalls.get() shouldBe 1
  }

  it should "not let a different render surface reusing the same SurfaceId disturb this surface's own cached panel buffer" in {
    // Renderer's panel layer cache used to be a single JVM-wide slot keyed only by SurfaceId, so two independently
    // rendered surfaces sharing a SurfaceId (as "outline" is, across a dozen specs) could stomp on each other's
    // cached image -- exactly the shape of the flake seen when this spec ran under sbt's default parallel-suite
    // execution alongside another spec painting a same-named panel. This reproduces that cross-surface interaction
    // deterministically, without depending on real thread scheduling.
    val surfaceA = new CountingLayerBufferSurface(120, 40)
    val before   = stateWithPinnedPanel("alpha\nbeta\ngamma")
    // surfaceA's own owner: shared across its two render calls below, the same as every other reuse assertion in
    // this file.
    val cachesA = com.serenity.state.manager.RenderCaches.create()

    RendererEntryPoints.render(
      before,
      cursorVisible = false,
      surfaceA,
      viewport,
      None,
      Damage.Everything,
      cachesA
    )
    surfaceA.newLayerSurfaceCalls.get() shouldBe 1

    // Stand in for a concurrently running render path -- another suite, another window -- painting a panel with the
    // *same* SurfaceId but a different frame shape. A genuinely independent render path has its own owning
    // `StateManager` and therefore its own `RenderCaches`, not `cachesA`.
    val surfaceB     = new CountingLayerBufferSurface(200, 60)
    val wideViewport = ViewportSize(200, 60)
    RendererEntryPoints.render(
      before,
      cursorVisible = false,
      surfaceB,
      wideViewport,
      None,
      Damage.Everything,
      com.serenity.state.manager.RenderCaches.create()
    )

    val after = editContent(before)
    RendererEntryPoints.render(
      after,
      cursorVisible = false,
      surfaceA,
      viewport,
      None,
      DamageProducer.forTransition(before, after),
      cachesA
    )

    surfaceA.newLayerSurfaceCalls.get() shouldBe 1
  }

  it should "not repaint a floating panel's own buffer when only editor content changed" in {
    val surface = new CountingLayerBufferSurface(120, 40)
    val before  = stateWith("alpha\nbeta\ngamma", List(floatingPanel))
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

    surface.newLayerSurfaceCalls.get() shouldBe 1
  }

  it should "repaint a floating panel's buffer when only its own content changes" in {
    val surface = new CountingLayerBufferSurface(120, 40)
    val before  = stateWith("alpha\nbeta\ngamma", List(floatingPanel))
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

    val changed          = floatingPanel.copy(content = SurfaceContent.QuickInfo("different text"))
    val after            = before.copy(runtime = before.runtime.copy(uiSurfaces = List(changed)))
    val transitionDamage = DamageProducer.forTransition(before, after)
    transitionDamage shouldBe Damage.Surface(floatingId)

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

  it should "reuse a panel's cached buffer across frames painted on different surfaces of the same window" in {
    val window      = new Object
    val firstFrame  = new CountingLayerBufferSurface(120, 40, Some(window))
    val secondFrame = new CountingLayerBufferSurface(120, 40, Some(window))
    val before      = stateWithPinnedPanel("alpha\nbeta\ngamma")
    val caches      = com.serenity.state.manager.RenderCaches.create()

    RendererEntryPoints.render(before, cursorVisible = false, firstFrame, viewport, None, Damage.Everything, caches)
    val after = editContent(before)
    RendererEntryPoints.render(
      after,
      cursorVisible = false,
      secondFrame,
      viewport,
      None,
      DamageProducer.forTransition(before, after),
      caches
    )

    firstFrame.newLayerSurfaceCalls.get() shouldBe 1
    secondFrame.newLayerSurfaceCalls.get() shouldBe 0
    caches.frameState.cachedPanelLayersFor(secondFrame).keySet shouldBe Set(pinnedId)
  }

  it should "paint panels straight onto the frame, with no layer buffer, when layer caching is off" in {
    val surface = new CountingLayerBufferSurface(120, 40)
    val before  = stateWith("alpha\nbeta\ngamma", List(floatingPanel), layerCaching = false)
    val caches  = com.serenity.state.manager.RenderCaches.create()

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

    surface.newLayerSurfaceCalls.get() shouldBe 0
    caches.frameState.cachedPanelLayersFor(surface) shouldBe Map.empty
    val drawnText = surface.putStringCalls.map(_.s) ++ surface.drawRunPxCalls.map(_.s)
    drawnText.exists(_.contains("hover text")) shouldBe true
  }

  it should "drop cached panel layers once layer caching is switched off" in {
    val surface = new CountingLayerBufferSurface(120, 40)
    val cached  = stateWithPinnedPanel("alpha\nbeta\ngamma")
    val caches  = com.serenity.state.manager.RenderCaches.create()

    RendererEntryPoints.render(cached, cursorVisible = false, surface, viewport, None, Damage.Everything, caches)
    caches.frameState.cachedPanelLayersFor(surface).keySet shouldBe Set(pinnedId)

    val uncached =
      cached.copy(persisted = cached.persisted.copy(config = cached.persisted.config.withLayerCaching(false)))
    RendererEntryPoints.render(uncached, cursorVisible = false, surface, viewport, None, Damage.Everything, caches)

    caches.frameState.cachedPanelLayersFor(surface) shouldBe Map.empty
  }

  it should "repaint into the panel's previous image rather than allocating a new one" in {
    val surface = new CountingLayerBufferSurface(120, 40)
    val before  = stateWithPinnedPanel("alpha\nbeta\ngamma")
    val caches  = com.serenity.state.manager.RenderCaches.create()

    RendererEntryPoints.render(before, cursorVisible = false, surface, viewport, None, Damage.Everything, caches)
    val firstImage = caches.frameState.cachedPanelLayersFor(surface).get(pinnedId).map(_.image)

    val changed = pinnedPanel.copy(dismissOnMove = true)
    val after   = before.copy(runtime = before.runtime.copy(uiSurfaces = List(changed)))
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

  /** A [[MockRenderSurface]] that also advertises [[LayerBufferSupport]] -- see
    * [[ModalLayerCompositingSpec.CountingLayerBufferSurface]] for why.
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

  private class FlushingLayerSurface(
      width: Int,
      height: Int,
      onFlush: BufferedImage => Unit,
      recycled: Option[BufferedImage]
  ) extends MockRenderSurface(width, height):
    override def flush(): Unit = onFlush(recycled.getOrElse(new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)))

end PanelLayerCompositingSpec
