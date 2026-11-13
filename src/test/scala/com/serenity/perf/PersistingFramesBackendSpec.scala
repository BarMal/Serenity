package com.serenity.perf

import java.awt.Font
import java.util.concurrent.atomic.AtomicInteger

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.ScrollDown
import com.serenity.perf.LaptopFrameBenchmarks.{MaxScrollRowsDrawn, MaxTypingRowsDrawn}
import com.serenity.rope.Balance
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.{AppState, BufferId, Damage}
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.renderer.{RenderSurface, ScreenIdentity}
import com.serenity.{FrameBackend, FramePixels, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `laptop.render.*` draws a damaged frame over one persisting image and checks it before timing it: no more rows
  * redrawn than the step allows, and pixel for pixel the frame drawn from scratch. That check is the point of the rows,
  * and it has to hold on whichever surface draws the frame, so it is run here on Skia's raster surface, and shown to
  * fail when a damaged row is skipped or the damage widens.
  */
class PersistingFramesBackendSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val backends: List[(String, FrameBackend)] = List("Skia raster" -> SkiaFrames)

  private lazy val (beforeTyping, afterTyping, afterScroll) =
    val (stateManager, _)           = LaptopFrameBenchmarks.proseStateManager()
    val (beforeTyping, afterTyping) = TypingTransition.of(stateManager)
    stateManager.applyEvent(ScrollDown(3)).unsafeRunSync()
    (beforeTyping, afterTyping, stateManager.getCurrentState.unsafeRunSync())

  /** What a planner that skipped the row typed into would report: the first row of the buffer, which did not change,
    * and nothing else. The step's own damage also carries `Chrome`, which on its own makes the planner find the changed
    * row by comparing frames, so keeping it would hide the omission.
    */
  private val skippingTheTypedRow: Damage = Damage.BufferRows(BufferId(1), Set(0))

  private def transition(backend: FrameBackend, before: AppState, after: AppState)(
    damage: (AppState, AppState) => Damage
  ) = LaptopFrameBenchmarks.AlternatingTransition(backend, before, after, damage)

  backends.foreach { (label, backend) =>
    s"A typing frame over $label" should "redraw few rows and equal a frame drawn from scratch" in
      transition(backend, beforeTyping, afterTyping)(DamageProducer.forTransition)
        .verify("typing", MaxTypingRowsDrawn)

    s"A wheel scroll over $label" should "redraw few rows and equal a frame drawn from scratch" in
      transition(backend, afterTyping, afterScroll)(DamageProducer.forTransition)
        .verify("scroll", MaxScrollRowsDrawn)

    s"The check over $label" should "fail when a damaged row is skipped" in {
      skippingTheTypedRow should not be DamageProducer.forTransition(beforeTyping, afterTyping)
      val failure = the[AssertionError] thrownBy
        transition(backend, beforeTyping, afterTyping)((_, _) => skippingTheTypedRow)
          .verify("typing", MaxTypingRowsDrawn)
      failure.getMessage should include("differs from a full frame")
    }

    it should "fail when the damage widens past the rows the step allows" in {
      val failure = the[AssertionError] thrownBy
        transition(backend, beforeTyping, afterTyping)((_, _) => Damage.Everything)
          .verify("typing", MaxTypingRowsDrawn)
      failure.getMessage should include("more than the")
    }
  }

  private lazy val transitions = LaptopFrameBenchmarks.InteractionTransitions(beforeTyping, afterTyping, afterScroll)

  "The Skia raster rows" should "carry the raster marker" in {
    LaptopFrameBenchmarks.interactionBenchmarks(transitions).map(_.name) shouldBe List(
      "laptop.render.typing_frame_2x.skia_raster",
      "laptop.render.scroll_frame_2x.skia_raster",
      "laptop.damage.typing_transition"
    )
  }

  "The Skia raster rows" should "time the same typing and scroll frames under the skia. prefix" in {
    LaptopFrameBenchmarks.skiaInteractionBenchmarks(transitions).map(_.name) shouldBe List(
      "skia.laptop.render.typing_frame_2x",
      "skia.laptop.render.scroll_frame_2x"
    )
  }

  they should "draw nothing with Skia until one is checked, so the rows timed before them never meet Skia's surface" in {
    val created = new AtomicInteger(0)
    val counting = new FrameBackend:
      def newFrame(widthPx: Int, heightPx: Int): FramePixels =
        val _ = created.incrementAndGet()
        SkiaFrames.newFrame(widthPx, heightPx)

      def surfaceOver(
        frame: FramePixels,
        metrics: CellMetrics,
        font: Font,
        logicalWidthPx: Int,
        logicalHeightPx: Int,
        scale: Double,
        persists: Boolean,
        caches: RenderCaches,
        screen: Option[ScreenIdentity],
        onKeptOff: List[PixelRect] => Unit
      ): RenderSurface =
        SkiaFrames.surfaceOver(
          frame,
          metrics,
          font,
          logicalWidthPx,
          logicalHeightPx,
          scale,
          persists,
          caches,
          screen,
          onKeptOff
        )
    val rows = LaptopFrameBenchmarks.skiaInteractionBenchmarks(transitions, counting)
    created.get shouldBe 0
    rows.head.verify()
    created.get should be > 0
  }

  they should "pass their own check before they are timed" in
    LaptopFrameBenchmarks.skiaInteractionBenchmarks(transitions).foreach(_.verify())

end PersistingFramesBackendSpec
