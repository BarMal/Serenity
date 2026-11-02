package com.serenity.perf

import java.awt.font.TextAttribute
import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import com.serenity.SkiaFrames
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.renderer.{FontSpec, FrameOutput, RenderSurface, RendererFramePlanner, ScreenIdentity}
import com.serenity.ui.skia.{ShapePlan, SkiaTextCaches}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShapePlanCostProbe extends AnyFlatSpec with Matchers with ColdFrameHarness:

  private def stateOf(text: String): AppState =
    val (manager, _) = LaptopFrameBenchmarks.stateManagerHolding(text, baseConfig, grid = viewport)
    manager.getCurrentState.unsafeRunSync()

  private val attrs = Map[TextAttribute, AnyRef](
    TextAttribute.LIGATURES -> TextAttribute.LIGATURES_ON,
    TextAttribute.KERNING   -> TextAttribute.KERNING_ON
  ).asJava
  private val sCode = codeFont.deriveFont(attrs)
  private val sText = textFont.deriveFont(attrs)

  private def renderFrame(state: AppState, surface: RenderSurface, caches: RenderCaches): Unit =
    val _ = RendererFramePlanner.renderFrame(
      state, cursorVisible = false, surface, viewport,
      caches.authoritativeScene.forState(state, viewport, sCode, sText),
      sCode, sText, uiFont, cellMetrics, uiMetrics, None,
      Some(FrameOutput(ScreenIdentity(this), new java.util.concurrent.atomic.AtomicReference(None))),
      Damage.Everything, caches = caches
    )

  private def draw(state: AppState, caches: RenderCaches): Unit =
    val frame = SkiaFrames.newFrame(windowWidth, windowHeight)
    renderFrame(state, SkiaFrames.surfaceOver(frame, cellMetrics, sCode, windowWidth, windowHeight, 1.0, false, caches, None), caches)

  private def plan(state: AppState, real: RenderCaches, planCaches: RenderCaches): Int =
    ShapePlan.keysOf(SkiaTextCaches.of(real.backendCaches), real.styledFonts, cellMetrics, FontSpec.fromAwt(sCode), windowWidth, windowHeight, 1.0)(
      surface => renderFrame(state, surface, planCaches)
    ).size

  private def ms(work: => Unit): Double =
    val t = System.nanoTime(); work; (System.nanoTime() - t) / 1e6

  private def median(xs: Seq[Double]): Double = xs.sorted.apply(xs.size / 2)

  private val out = new StringBuilder

  private def report(name: String, state: AppState): Unit =
    val n = 15
    // warm up the JIT with throwaway rounds
    (1 to 5).foreach { _ => val r = RenderCaches.create(); draw(state, r); plan(state, r, RenderCaches.forPlanning(r)) }
    val coldDraw = (1 to n).map { _ => val r = RenderCaches.create(); ms(draw(state, r)) }
    val warmDraw = { val r = RenderCaches.create(); draw(state, r); (1 to n).map(_ => ms(draw(state, r))) }
    val freshPass = (1 to n).map { _ => val r = RenderCaches.create(); ms { plan(state, r, RenderCaches.create(styledFonts = r.styledFonts)) } }
    val sharedWarm = { val r = RenderCaches.create(); draw(state, r); (1 to n).map(_ => ms(plan(state, r, RenderCaches.forPlanning(r)))) }
    val sceneOnly = (1 to n).map { _ =>
      val r = RenderCaches.create(); val _ = r.authoritativeScene.forState(state, viewport, sCode, sText)
      ms(plan(state, r, RenderCaches.forPlanning(r)))
    }
    val keys = { val r = RenderCaches.create(); plan(state, r, RenderCaches.forPlanning(r)) }
    out.append(f"$name%-10s keys=$keys%3d | real cold draw p50 ${median(coldDraw)}%7.2f ms | real warm draw p50 ${median(warmDraw)}%7.2f | pass fresh-caches p50 ${median(freshPass)}%7.2f | pass shared+drawn p50 ${median(sharedWarm)}%7.2f | pass shared+scene-only(first) p50 ${median(sceneOnly)}%7.2f%n")

  "the probe" should "write the planning pass cost" in {
    report("prose", stateOf(BenchmarkFixtures.loremIpsumProse(6)))
    report("code", stateOf(BenchmarkFixtures.largeMultilineDocument(60)))
    report("rich", BenchmarkFixtures.editorStateForRichDocument(BenchmarkFixtures.largeRichTextDocument(30)))
    Files.writeString(Paths.get("/tmp/claude-0/plan-cost.txt"), out.toString)
    succeed
  }
