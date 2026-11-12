package com.serenity.perf

import java.awt.Font

import cats.effect.unsafe.implicits.global
import com.serenity.diagnostics.{FrameCounter, FrameCounts}
import com.serenity.keystroke.events.InsertChar
import com.serenity.state.manager.StateManagerTestFacade.updateState
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, PanelPosition, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, RendererFramePlanner, ScreenIdentity, SurfaceSizing}
import com.serenity.{DockedPanelFixtures, SkiaFrames}
import java.util.concurrent.atomic.AtomicReference
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What [[PanelLayerGeometrySpec]] asks of a frame with a docked panel, asked of Skia, which has no layer buffers and
  * paints each panel straight onto the frame. The view model of the overlays is built once; a panel whose pixels the
  * frame kept is not painted again, so an idle frame draws no runs (where Java2D composites no layer); a typing frame
  * draws fewer runs than the first; and pixels come out the same whatever the layer caching setting says, because Skia
  * does not consult it.
  */
class SkiaPanelLayerSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont    = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)
  private val viewport    = ViewportSize(1500 / cellMetrics.charWidth, 1000 / cellMetrics.lineHeight)

  private val outlineId = SurfaceId("outline")

  /** A laptop-sized window drawn frame after frame over one persisting Skia image. */
  final private class Frames(shown: AppState => AppState):

    private val frame = SkiaFrames.newFrame(
      SurfaceSizing.deviceImageDimension(LaptopFrameBenchmarks.LogicalWidthPx, LaptopFrameBenchmarks.DeviceScale),
      SurfaceSizing.deviceImageDimension(LaptopFrameBenchmarks.LogicalHeightPx, LaptopFrameBenchmarks.DeviceScale)
    )

    private val caches = RenderCaches.create()
    private val screen = ScreenIdentity(this)
    caches.frameCounters.setEnabled(true)

    def render(state: AppState, damage: Damage): FrameCounts =
      val shownState = shown(state)
      val surface = SkiaFrames.surfaceOver(
        frame,
        cellMetrics,
        codeFont,
        LaptopFrameBenchmarks.LogicalWidthPx,
        LaptopFrameBenchmarks.LogicalHeightPx,
        LaptopFrameBenchmarks.DeviceScale,
        persists = true,
        caches,
        Some(screen)
      )
      val _ = RendererFramePlanner.renderFrame(
        shownState,
        cursorVisible = false,
        surface,
        viewport,
        caches.authoritativeScene.forState(shownState, viewport, codeFont, textFont),
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

    def pixels: Array[Int] = frame.rgbPixels

  /** Where the two frames differ, as a short report: how many pixels, the box round them, and a few of the pairs. */
  private def differences(on: Frames, off: Frames, width: Int): Vector[String] =
    val (withLayers, without) = (on.pixels, off.pixels)
    val pairs =
      withLayers.indices.collect {
        case i if withLayers(i) != without(i) => (i % width, i / width, withLayers(i), without(i))
      }.toVector
    if pairs.isEmpty then Vector.empty
    else
      val box = s"x ${pairs.map(_._1).min}-${pairs.map(_._1).max}, y ${pairs.map(_._2).min}-${pairs.map(_._2).max}"
      s"${pairs.size} px differ in $box" +:
        pairs.take(4).map((x, y, a, b) => f"($x,$y) with layers ${a & 0xffffff}%06x without ${b & 0xffffff}%06x")

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

  it should "draw no runs when the frame keeps the panel's pixels, as it draws on the first frame" in {
    val (docked, _) = transition
    val frames      = Frames(withCaching(true))
    val first       = frames.render(docked, Damage.Everything)

    val idle = frames.render(docked, Damage.Nothing)

    withClue(s"first ${describe(first)}; idle ${describe(idle)}: ") {
      first(FrameCounter.RunsDrawn) should be > 0L
      idle(FrameCounter.RunsDrawn) shouldBe 0L
    }
  }

  it should "draw fewer runs on a typing frame than on the first" in {
    val (docked, typed) = transition
    val frames          = Frames(withCaching(true))
    val first           = frames.render(docked, Damage.Everything)

    val typing = frames.render(typed, DamageProducer.forTransition(docked, typed))

    withClue(s"first ${describe(first)}; typing ${describe(typing)}: ")(
      typing(FrameCounter.RunsDrawn) should be < first(FrameCounter.RunsDrawn)
    )
  }

  // Skia does not consult the layer caching setting, so the two sides are equal by construction; the test guards that
  // the setting cannot change what Skia draws.
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
    val width =
      SurfaceSizing.deviceImageDimension(LaptopFrameBenchmarks.LogicalWidthPx, LaptopFrameBenchmarks.DeviceScale)

    steps.zipWithIndex.foreach { (step, index) =>
      val (state, damage) = step
      val _               = on.render(state, damage)
      val _               = off.render(state, damage)
      withClue(s"frame $index: ")(differences(on, off, width) shouldBe empty)
    }
  }

end SkiaPanelLayerSpec
