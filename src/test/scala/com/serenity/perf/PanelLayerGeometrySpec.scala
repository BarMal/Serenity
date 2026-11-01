package com.serenity.perf

import java.awt.Font
import java.util.concurrent.atomic.AtomicReference

import cats.effect.unsafe.implicits.global
import com.serenity.DockedPanelFixtures
import com.serenity.config.{AppConfig, DockedCornerStyle, StatusLinePlacement}
import com.serenity.diagnostics.{FrameCounter, FrameCounts}
import com.serenity.keystroke.events.InsertChar
import com.serenity.state.manager.StateManagerTestFacade.updateState
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.color.RenderColor
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
    private val strayDraws    = new AtomicReference(Vector.empty[String])
    caches.frameCounters.setEnabled(true)

    def layerImages: Vector[RenderImage] = flushedLayers.get()

    def strays: Vector[String] = strayDraws.get()

    /** Notes a draw on the first row that reaches past the last whole cell, with the code that asked for it. */
    private def noteStray(what: String, xPx: Int, yPx: Int, widthPx: Int): Unit =
      if xPx + widthPx >= viewport.width * cellMetrics.charWidth && yPx < cellMetrics.lineHeight then
        val callers = Thread.currentThread.getStackTrace.map(_.toString).filter(_.startsWith("com.serenity.ui.renderer"))
        val _       = strayDraws.updateAndGet(_ :+ s"$what at x=$xPx w=$widthPx <- ${callers.slice(1, 5).mkString(" <- ")}")

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
        layerCacheOwnerOverride = Some(screen),
        textCaches = caches.surfaceText
      ):
        override def putString(x: Int, y: Int, text: String): Unit =
          noteStray(s"putString('${text.take(12)}')", cellMetrics.toPixelX(x), cellMetrics.toPixelY(y), text.length * cellMetrics.charWidth)
          super.putString(x, y, text)

        override def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit =
          noteStray("fillRect", cellMetrics.toPixelX(x), cellMetrics.toPixelY(y), width * cellMetrics.charWidth)
          super.fillRect(x, y, width, height, char)

        override def strokeRect(x: Int, y: Int, width: Int, height: Int, color: RenderColor, strokeWidth: Float): Unit =
          noteStray("strokeRect", cellMetrics.toPixelX(x), cellMetrics.toPixelY(y), width * cellMetrics.charWidth)
          super.strokeRect(x, y, width, height, color, strokeWidth)

        override def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: RenderColor): Unit =
          noteStray("fillPixelRect", xPx, yPx, widthPx)
          super.fillPixelRect(xPx, yPx, widthPx, heightPx, color)

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

  /** Where the two frames differ, as a short report: how many pixels, the box round them, and a few of the pairs. */
  private def differences(on: Frames, off: Frames): Vector[String] =
    val width = on.image.getWidth
    val pairs =
      on.pixels.zip(off.pixels).zipWithIndex.collect {
        case ((withLayers, without), i) if withLayers != without => (i % width, i / width, withLayers, without)
      }
    if pairs.isEmpty then Vector.empty
    else
      val box = s"x ${pairs.map(_._1).min}-${pairs.map(_._1).max}, y ${pairs.map(_._2).min}-${pairs.map(_._2).max}"
      val around =
        for (x, y) <- Vector(
              (2994, 0),
              (2995, 0),
              (2996, 0),
              (2996, 29),
              (2996, 30),
              (2996, 31),
              (2996, 500),
              (2999, 999)
            )
        yield
          val withLayers = on.image.getRGB(x, y) & 0xffffff
          val without    = off.image.getRGB(x, y) & 0xffffff
          f"($x,$y) with layers $withLayers%06x without $without%06x"
      s"${pairs.size} px differ in $box" +: (pairs.take(2).map { (x, y, a, b) =>
        f"($x,$y) with layers ${a & 0xffffff}%06x without ${b & 0xffffff}%06x"
      } ++ around)

  /** For a failing frame: how many pixels still differ when one part of the scene is left out, to say which part. */
  private def withoutOneThing(frame: AppState): Vector[String] =
    def config(change: AppConfig => AppConfig)(state: AppState): AppState =
      state.copy(persisted = state.persisted.copy(config = change(state.persisted.config)))
    val variants: Vector[(String, AppState => AppState)] = Vector(
      "no hover"             -> identity,
      "no pane headers"      -> config(_.withPaneHeaders(false)),
      "no scrollbar"         -> config(_.withScrollbar(false)),
      "no status line"       -> config(_.withStatusLinePlacement(StatusLinePlacement.Off)),
      "no line numbers"      -> config(_.withLineNumbers(false)),
      "square corners"       -> config(_.withUiCornerRadiusPx(0)),
      "floating docked"      -> config(_.withDockedCornerStyle(DockedCornerStyle.Floating))
    )
    variants.map { (label, tweak) =>
      val on  = Frames(withCaching(true).andThen(tweak))
      val off = Frames(withCaching(false).andThen(tweak))
      val _   = on.render(frame, Damage.Everything)
      val _   = off.render(frame, Damage.Everything)
      s"without [$label]: ${differences(on, off).headOption.getOrElse("identical")}"
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
      val found = differences(on, off)
      withClue(s"frame $index: ")(
        (if found.isEmpty then found else found ++ off.strays.take(6) ++ withoutOneThing(frame)) shouldBe empty
      )
    }
  }

end PanelLayerGeometrySpec
