package com.serenity.perf

import cats.effect.unsafe.implicits.global
import com.serenity.diagnostics.FrameCounter
import com.serenity.state.models.Damage
import com.serenity.{FrameBackend, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `SkiaFrameBenchmarks` times a window's frame loop, so its frames must keep their surface caches from one frame to
  * the next, as a window does; a frame that starts cold times shaping, not drawing. Its Java2D arm is the same
  * `Frames`, but Java2D's first frame counts no shaping on this content, so only the Skia arm can show a cold cache.
  */
class SkiaFrameBenchmarksCachesSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  /** Text below the combining marks is drawn without shaping, so a frame of plain ASCII counts nothing whichever cache
    * it starts with; the arrows are what the first frame has to shape.
    */
  private val shapedProse = BenchmarkFixtures.loremIpsumProse(paragraphs = 300).replace(" ipsum ", " ipsum \u2192 ")

  private def twoIdenticalFrames(backend: FrameBackend): (Long, Long) =
    val (stateManager, _) = LaptopFrameBenchmarks.stateManagerHolding(shapedProse, cursorLine = 150, cursorColumn = 40)
    val state             = stateManager.getCurrentState.unsafeRunSync()
    val frames = SkiaFrameBenchmarks.Frames(
      backend,
      LaptopFrameBenchmarks.codeFont,
      LaptopFrameBenchmarks.cellMetrics,
      LaptopFrameBenchmarks.viewport
    )
    frames.counting {
      val _      = frames.render(state, Damage.Everything)
      val first  = frames.lastFrame(FrameCounter.ShapingCalls)
      val _      = frames.render(state, Damage.Everything)
      val second = frames.lastFrame(FrameCounter.ShapingCalls)
      (first, second)
    }

  "A Skia raster frame loop" should "shape nothing new when the same frame is drawn again" in {
    val (first, second) = twoIdenticalFrames(SkiaFrames)

    first should be > 0L
    second shouldBe 0L
  }
