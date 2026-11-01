package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

import cats.effect.unsafe.implicits.global
import com.serenity.ChromeOn
import com.serenity.config.{AppConfig, DockedCornerStyle}
import com.serenity.keystroke.events.{Event, InsertChar, ScrollDown}
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, Java2DRenderSurface, RendererFramePlanner, ScreenIdentity}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The frames `laptop.render.typing_frame_2x` and `scroll_frame_2x` time, checked here as the benchmarks check them: a
  * damaged frame over one persisting image, driven by the damage a live `StateManager` reports for the step, must equal
  * a frame drawn from scratch to the last device pixel. Before #1839 the first column of a kept row lost the one
  * antialiased pixel its first glyph spilled outside the row's band, six pixels in all.
  */
class LaptopFramesMatchColdSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont    = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, codeFont.getSize).deriveFont(codeFont.getSize2D)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)
  private val viewport    = ViewportSize(1500 / cellMetrics.charWidth, 1000 / cellMetrics.lineHeight)

  final private class PersistingImage:
    private val image  = LaptopFrameBenchmarks.frameImage()
    private val caches = RenderCaches.create()
    private val screen = ScreenIdentity(this)

    def render(state: AppState, damage: Damage): BufferedImage =
      val surface = new Java2DRenderSurface(
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
      )
      val _ = RendererFramePlanner.renderFrame(
        state,
        cursorVisible = false,
        surface,
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
      image

  private def differing(drawn: BufferedImage, state: AppState): Seq[(Int, Int)] =
    val cold = LaptopFrameBenchmarks.renderedFrame(
      state,
      RenderCaches.create(),
      LaptopFrameBenchmarks.frameImage(),
      cursorVisible = false
    )
    val diffs = for
      x <- 0 until drawn.getWidth
      y <- 0 until drawn.getHeight
      if (drawn.getRGB(x, y) & 0xffffff) != (cold.getRGB(x, y) & 0xffffff)
    yield (x, y)
    diffs

  private def summary(differences: Seq[(Int, Int)]): String =
    val xs = differences.map(_._1)
    val runs = differences.map(_._2).distinct.sorted.foldLeft(List.empty[(Int, Int)]) {
      case ((from, to) :: rest, y) if y == to + 1 => (from, y) :: rest
      case (acc, y)                               => (y, y) :: acc
    }
    s"${differences.size} pixels differ in x ${xs.minOption.getOrElse(0)}-${xs.maxOption.getOrElse(0)}, " +
      s"y runs ${runs.reverse.take(24).map((from, to) => s"$from-$to").mkString(",")}"

  private val floating: AppConfig = ChromeOn.config.withDockedCornerStyle(DockedCornerStyle.Floating)

  private def transition(event: Event, config: AppConfig = ChromeOn.config): (AppState, AppState) =
    val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager(config = config)
    val before            = stateManager.getCurrentState.unsafeRunSync()
    stateManager.applyEvent(event).unsafeRunSync()
    (before, stateManager.getCurrentState.unsafeRunSync())

  /** One image is drawn into twice, so each frame is compared before the next overwrites it. */
  private def differencesThrough(before: AppState, after: AppState): (Seq[(Int, Int)], Seq[(Int, Int)]) =
    val frames = PersistingImage()
    val _      = frames.render(before, Damage.Everything)
    val there  = differing(frames.render(after, DamageProducer.forTransition(before, after)), after)
    val back   = differing(frames.render(before, DamageProducer.forTransition(after, before)), before)
    (there, back)

  "A typing frame at the laptop's size" should "equal a frame drawn from scratch, there and back" in {
    val (before, after) = transition(InsertChar('x'))

    val (there, back) = differencesThrough(before, after)

    withClue(s"the frame typed into, ${summary(there)}: ")(there shouldBe empty)
    withClue(s"the frame typed back out of, ${summary(back)}: ")(back shouldBe empty)
  }

  "A wheel scroll at the laptop's size" should "equal a frame drawn from scratch, there and back" in {
    val (before, after) = transition(ScrollDown(3))

    val (there, back) = differencesThrough(before, after)

    withClue(s"the frame scrolled to, ${summary(there)}: ")(there shouldBe empty)
    withClue(s"the frame scrolled back from, ${summary(back)}: ")(back shouldBe empty)
  }

  "A typing frame in floating chrome" should "equal a frame drawn from scratch, there and back" in {
    val (before, after) = transition(InsertChar('x'), floating)

    val (there, back) = differencesThrough(before, after)

    withClue(s"the frame typed into, ${summary(there)}: ")(there shouldBe empty)
    withClue(s"the frame typed back out of, ${summary(back)}: ")(back shouldBe empty)
  }

  "A wheel scroll in floating chrome" should "equal a frame drawn from scratch, there and back" in {
    val (before, after) = transition(ScrollDown(3), floating)

    val (there, back) = differencesThrough(before, after)

    withClue(s"the frame scrolled to, ${summary(there)}: ")(there shouldBe empty)
    withClue(s"the frame scrolled back from, ${summary(back)}: ")(back shouldBe empty)
  }
