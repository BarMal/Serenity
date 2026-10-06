package com.serenity.ui.renderer

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.MockRenderSurface
import com.serenity.state.models.{BufferId, SemanticTokensAvailability}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The annotations drawn on an editor frame are worked out again only when something they are worked out from changed:
  * a scene reused across frames must not pay for them each time (#1810 made them per-frame so a diagnostic arriving
  * without moving a line shows up).
  */
class RendererAnnotationMemoSpec extends AnyFlatSpec with Matchers:

  private val frameState = RendererFrameState(64)

  private val annotations = Map(
    BufferId(1) -> BufferRenderAnnotations(Map.empty, Map.empty, SemanticTokensAvailability.Pending)
  )

  final private class Counting:
    private val count = AtomicInteger(0)
    def calls: Int    = count.get

    def compute(): Map[BufferId, BufferRenderAnnotations] =
      count.incrementAndGet()
      annotations

  "Annotations for a persistent surface" should "be worked out once while their inputs are the same objects" in {
    val surface = new MockRenderSurface(10, 5, true)
    val inputs  = Vector[AnyRef](new Object, new Object)
    val counter = new Counting

    val frames = (1 to 5).map(_ => frameState.annotationsFor(surface, AnnotationInputs(inputs*))(counter.compute()))

    counter.calls shouldBe 1
    all(frames) shouldBe annotations
  }

  it should "be worked out again once any input is a different object" in {
    val surface = new MockRenderSurface(10, 5, true)
    val kept    = new Object
    val counter = new Counting

    frameState.annotationsFor(surface, AnnotationInputs(kept, new Object))(counter.compute())
    frameState.annotationsFor(surface, AnnotationInputs(kept, new Object))(counter.compute())

    counter.calls shouldBe 2
  }

  it should "not be shared with another surface" in {
    val inputs  = AnnotationInputs(new Object)
    val counter = new Counting

    frameState.annotationsFor(new MockRenderSurface(10, 5, true), inputs)(counter.compute())
    frameState.annotationsFor(new MockRenderSurface(10, 5, true), inputs)(counter.compute())

    counter.calls shouldBe 2
  }

  "Annotations for a surface that persists nothing" should "be worked out every frame" in {
    val surface = new MockRenderSurface(10, 5, false)
    val inputs  = AnnotationInputs(new Object)
    val counter = new Counting

    (1 to 3).foreach(_ => frameState.annotationsFor(surface, inputs)(counter.compute()))

    counter.calls shouldBe 3
  }
