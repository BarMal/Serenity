package com.serenity.perf

import java.awt.Font
import java.util.concurrent.atomic.AtomicReference

import cats.effect.unsafe.implicits.global
import com.serenity.DockedPanelFixtures
import com.serenity.diagnostics.{FrameCounter, FrameCounts}
import com.serenity.keystroke.events.InsertChar
import com.serenity.state.manager.StateManagerTestFacade.updateState
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, PanelPosition, PixelRect, ViewportSize}
import com.serenity.ui.renderer.{
  FrameOutput,
  Java2DRenderSurface,
  LayerBufferSupport,
  RenderImage,
  RenderSurface,
  RendererFramePlanner,
  ScreenIdentity
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a frame with a docked panel costs in layer buffers (#1869): each panel owns an image the size of its own bounds
  * rather than of the window, the view model of the overlays is built once, and a panel whose pixels the frame kept is
  * not composited again. Pixels must come out the same as with layer caching off.
  */
class PanelLayerGeometrySpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont    = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)
  private val viewport    = ViewportSize(1500 / cellMetrics.charWidth, 1000 / cellMetrics.lineHeight)

  private val windowPx =
    (LaptopFrameBenchmarks.LogicalWidthPx * LaptopFrameBenchmarks.DeviceScale).toLong *
      (LaptopFrameBenchmarks.LogicalHeightPx * LaptopFrameBenchmarks.DeviceScale).toLong

  private val outlineId = SurfaceId("outline")

  /** A laptop-sized window drawn frame after frame over one persisting image, recording every layer image the panels
    * hand back.
    */
  final private class Frames(shown: AppState => AppState):
    val image                 = LaptopFrameBenchmarks.frameImage()
    private val caches        = RenderCaches.create()
    private val screen        = ScreenIdentity(this)
    private val flushedLayers = new AtomicReference(Vector.empty[RenderImage])
    caches.frameCounters.setEnabled(true)

    def layerImages: Vector[RenderImage] = flushedLayers.get()

    def render(frame: AppState, damage: Damage): FrameCounts =
      val state = shown(frame)
      val _ = RendererFramePlanner.renderFrame(
        state,
        cursorVisible = false,
        recordingSurface(),
        viewport,
        caches.authoritativeScene.forState(state, viewport, codeFont, textFont),
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        None,
        Some(FrameOutput(screen, new AtomicReference(None))),
        damage,
        caches = caches
      )
      caches.frameCounters.lastFrame

    def pixels: Vector[Int] =
      Vector.tabulate(image.getWidth * image.getHeight)(i => image.getRGB(i % image.getWidth, i / image.getWidth))

    private def recording(onFlush: RenderImage => Unit): RenderImage => Unit =
      layer =>
        val _ = flushedLayers.updateAndGet(_ :+ layer)
        onFlush(layer)

    private def recordingSurface(): Java2DRenderSurface =
      new Java2DRenderSurface(
        image,
        cellMetrics,
        codeFont,
        _ => (),
        logicalWidthPx = LaptopFrameBenchmarks.LogicalWidthPx,
        logicalHeightPx = LaptopFrameBenchmarks.LogicalHeightPx,
        deviceScaleX = LaptopFrameBenchmarks.DeviceScale,
        deviceScaleY = LaptopFrameBenchmarks.DeviceScale,
        contentPersists = true,
        layerCacheOwnerOverride = Some(screen)
      ):
        override def layerBuffers: Option[LayerBufferSupport] = super.layerBuffers.map { real =>
          new LayerBufferSupport:
            def newLayerSurface(onFlush: RenderImage => Unit, recycled: Option[RenderImage]): RenderSurface =
              real.newLayerSurface(recording(onFlush), recycled)

            override def newBoundedLayerSurface(
              bounds: PixelRect,
              onFlush: RenderImage => Unit,
              recycled: Option[RenderImage]
            ): RenderSurface =
              real.newBoundedLayerSurface(bounds, recording(onFlush), recycled)
        }

  private def withCaching(enabled: Boolean)(state: AppState): AppState =
    state.copy(persisted = state.persisted.copy(config = state.persisted.config.withLayerCaching(enabled)))

  private def withHover(state: AppState): AppState =
    state.copy(runtime =
      state.runtime.copy(uiSurfaces =
        state.runtime.uiSurfaces :+ UiSurface(
          SurfaceId("peek"),
          SurfaceContent.QuickInfo("hover text"),
          SurfacePresentation.Floating(Some(CursorPosition(150, 40)), SurfacePlacement.AboveCursor)
        )
      )
    )

  /** The prose state with an outline docked on its right, before and after typing a character. */
  private def transition: (AppState, AppState) =
    val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager()
    stateManager
      .updateState(DockedPanelFixtures.dock(_, outlineId, SurfaceContent.Outline(Nil), PanelPosition.Right, 40))
      .unsafeRunSync()
    val before = stateManager.getCurrentState.unsafeRunSync()
    stateManager.applyEvent(InsertChar('x')).unsafeRunSync()
    (before, stateManager.getCurrentState.unsafeRunSync())

  private def describe(counts: FrameCounts): String =
    FrameCounter.values.map(counter => s"${counter.label}=${counts(counter)}").mkString(" ")

  "A frame with a docked panel" should "build the overlay view model once, with layer caching on or off" in {
    val (docked, _) = transition
    List(false, true).foreach { caching =>
      val counts = Frames(withCaching(caching)).render(docked, Damage.Everything)

      withClue(s"layer caching $caching, ${describe(counts)}: ")(counts(FrameCounter.OverlayModelBuilds) shouldBe 1L)
    }
  }

  it should "size each panel's layer image to the panel, not to the window" in {
    val (docked, _) = transition
    val frames      = Frames(withCaching(true))

    val counts = frames.render(docked, Damage.Everything)

    withClue(s"${describe(counts)}: ") {
      frames.layerImages should not be empty
      frames.layerImages.foreach(layer => (layer.widthPx.toLong * layer.heightPx) should be <= (windowPx * 3 / 10))
      counts(FrameCounter.LayerBufferPx) should be <= (windowPx * 3 / 10 * frames.layerImages.size)
      counts(FrameCounter.LayerBufferPx) should be > 0L
    }
  }

  it should "reuse the layer image when the panel repaints" in {
    val (docked, _) = transition
    val frames      = Frames(withCaching(true))
    val _           = frames.render(docked, Damage.Everything)

    val repainted = frames.render(docked, Damage.Surface(outlineId))

    withClue(s"${describe(repainted)}: ") {
      repainted(FrameCounter.LayerComposites) should be >= 1L
      repainted(FrameCounter.LayerBufferPx) shouldBe 0L
    }
  }

  it should "not composite a panel over pixels the frame kept" in {
    val (docked, _) = transition
    val frames      = Frames(withCaching(true))
    val first       = frames.render(docked, Damage.Everything)

    val idle = frames.render(docked, Damage.Nothing)

    withClue(s"first ${describe(first)}; idle ${describe(idle)}: ") {
      first(FrameCounter.LayerComposites) should be >= 1L
      idle(FrameCounter.LayerComposites) shouldBe 0L
    }
  }

  it should "composite fewer panels on a typing frame than on the first" in {
    val (docked, typed) = transition
    val frames          = Frames(withCaching(true))
    val first           = frames.render(docked, Damage.Everything)

    val typing = frames.render(typed, DamageProducer.forTransition(docked, typed))

    withClue(s"first ${describe(first)}; typing ${describe(typing)}: ")(
      typing(FrameCounter.LayerComposites) should be < first(FrameCounter.LayerComposites)
    )
  }

  it should "draw the same pixels with layer caching on as with it off" in {
    val (docked, typed) = transition
    val steps = List(
      docked -> Damage.Everything,
      typed  -> DamageProducer.forTransition(docked, typed),
      typed  -> Damage.Nothing,
      docked -> DamageProducer.forTransition(typed, docked)
    )
    val on  = Frames(withCaching(true).andThen(withHover))
    val off = Frames(withCaching(false).andThen(withHover))

    steps.zipWithIndex.foreach { (step, index) =>
      val (frame, damage) = step
      val _               = on.render(frame, damage)
      val _               = off.render(frame, damage)
      withClue(s"frame $index: ")(on.pixels shouldBe off.pixels)
    }
  }

end PanelLayerGeometrySpec
