package com.serenity.perf

import java.awt.Font
import java.util.concurrent.atomic.AtomicReference

import cats.effect.unsafe.implicits.global
import com.serenity.diagnostics.{FrameCounter, FrameCounts}
import com.serenity.keystroke.events.InsertChar
import com.serenity.state.manager.StateManagerTestFacade.updateState
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, PanelPosition, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, RendererFramePlanner, ScreenIdentity, SurfaceSizing}
import com.serenity.{DockedPanelFixtures, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a frame with a docked panel owes, asked of Skia, which paints each panel straight onto the frame. The view
  * model of the overlays is built once; a panel whose pixels the frame kept is not painted again, so an idle frame
  * draws no runs; and a typing frame draws fewer runs than the first.
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

  "A frame with a docked panel" should "build the overlay view model once" in {
    val (docked, _) = transition
    val counts      = Frames(identity).render(docked, Damage.Everything)

    withClue(s"${describe(counts)}: ")(counts(FrameCounter.OverlayModelBuilds) shouldBe 1L)
  }

  it should "draw no runs when the frame keeps the panel's pixels, as it draws on the first frame" in {
    val (docked, _) = transition
    val frames      = Frames(identity)
    val first       = frames.render(docked, Damage.Everything)

    val idle = frames.render(docked, Damage.Nothing)

    withClue(s"first ${describe(first)}; idle ${describe(idle)}: ") {
      first(FrameCounter.RunsDrawn) should be > 0L
      idle(FrameCounter.RunsDrawn) shouldBe 0L
    }
  }

  it should "draw fewer runs on a typing frame than on the first" in {
    val (docked, typed) = transition
    val frames          = Frames(identity)
    val first           = frames.render(docked, Damage.Everything)

    val typing = frames.render(typed, DamageProducer.forTransition(docked, typed))

    withClue(s"first ${describe(first)}; typing ${describe(typing)}: ")(
      typing(FrameCounter.RunsDrawn) should be < first(FrameCounter.RunsDrawn)
    )
  }

end SkiaPanelLayerSpec
