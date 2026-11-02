package com.serenity.perf

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.renderer.{FontSpec, FrameOutput, RenderSurface, RendererFramePlanner, ScreenIdentity}
import com.serenity.ui.skia.{ShapePlan, SkiaTextCaches}
import org.jetbrains.skia.Font
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The keys a frame will ask the shaped-text cache for are found by running the renderer's own line code against a
  * surface that records them instead of shaping (#2465, step 2), so they are right by construction. Each spec compares
  * what that pass records with what a real frame left in the cache.
  */
class ShapePlanSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private def stateOf(text: String): AppState =
    val (manager, _) = LaptopFrameBenchmarks.stateManagerHolding(text, baseConfig, grid = viewport)
    manager.getCurrentState.unsafeRunSync()

  private val prose = stateOf(BenchmarkFixtures.loremIpsumProse(6))
  private val code  = stateOf(BenchmarkFixtures.largeMultilineDocument(60))
  private val rich  = BenchmarkFixtures.editorStateForRichDocument(BenchmarkFixtures.largeRichTextDocument(30))

  private def renderFrame(state: AppState, surface: RenderSurface, caches: RenderCaches): Unit =
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
      Some(FrameOutput(ScreenIdentity(this), new java.util.concurrent.atomic.AtomicReference(None))),
      Damage.Everything,
      caches = caches
    )

  private def drawnKeys(state: AppState): (RenderCaches, Set[(String, Font)]) =
    val caches = RenderCaches.create()
    val _      = new Pooled(1.0, persists = false, caches).render(state, Damage.Everything)
    (caches, SkiaTextCaches.of(caches.backendCaches).shapedText.heldKeys)

  private def plannedKeys(state: AppState, drawn: RenderCaches): Vector[(String, Font)] =
    val skia = SkiaTextCaches.of(drawn.backendCaches)
    ShapePlan.keysOf(
      skia,
      drawn.styledFonts,
      cellMetrics,
      FontSpec.fromAwt(codeFont),
      windowWidth,
      windowHeight,
      1.0
    )(surface => renderFrame(state, surface, RenderCaches.create(styledFonts = drawn.styledFonts)))

  private val fixtures = List("prose" -> prose, "code" -> code, "rich text" -> rich)

  fixtures.foreach { (name, state) =>
    s"The planned keys of $name" should "equal the keys a real frame asked for" in {
      val (drawn, asked) = drawnKeys(state)
      asked should not be empty

      plannedKeys(state, drawn).toSet shouldBe asked
    }
  }

  "The plan" should "name each key once" in {
    val (drawn, _) = drawnKeys(rich)
    val planned    = plannedKeys(rich, drawn)

    planned.distinct.size shouldBe planned.size
  }

  it should "shape nothing and leave the real caches as they were" in {
    val caches = RenderCaches.create()
    val skia   = SkiaTextCaches.of(caches.backendCaches)

    val planned = ShapePlan.keysOf(
      skia,
      caches.styledFonts,
      cellMetrics,
      FontSpec.fromAwt(codeFont),
      windowWidth,
      windowHeight,
      1.0
    )(surface => renderFrame(prose, surface, RenderCaches.create(styledFonts = caches.styledFonts)))

    planned should not be empty
    skia.shapedText.shapings shouldBe 0L
    skia.shapedText.aheadShapings shouldBe 0L
    skia.shapedText.heldKeys shouldBe empty
  }

  it should "leave a real frame nothing to shape once shaped ahead" in {
    val caches = RenderCaches.create()
    val skia   = SkiaTextCaches.of(caches.backendCaches)
    val planned = ShapePlan.keysOf(
      skia,
      caches.styledFonts,
      cellMetrics,
      FontSpec.fromAwt(codeFont),
      windowWidth,
      windowHeight,
      1.0
    )(surface => renderFrame(rich, surface, RenderCaches.create(styledFonts = caches.styledFonts)))
    try
      skia.ahead.request(planned)
      skia.ahead.awaitIdle(30.seconds) shouldBe true

      val _ = new Pooled(1.0, persists = false, caches).render(rich, Damage.Everything)

      skia.shapedText.shapings shouldBe 0L
    finally skia.ahead.close()
  }
