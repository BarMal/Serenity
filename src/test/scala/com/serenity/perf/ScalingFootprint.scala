package com.serenity.perf

import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{AppState, BufferId}

/** Heap per open buffer (#2478): the state of one buffer or of twenty, with every one of them drawn once into shared
  * render caches, which is what a session that has visited its tabs keeps resident. The figure per buffer is the
  * difference of the two rows over 19, read in the docs; distinct content per buffer, so the caches cannot share
  * entries between them.
  */
private[perf] object ScalingFootprint:

  import MultiBufferFixture.Kind

  final case class Resident(state: AppState, caches: RenderCaches, frames: Int)

  def scenarios: List[FootprintBenchmarks.Scenario] =
    for
      shown <- List(1, 20)
      kind  <- Kind.values.toList
    yield FootprintBenchmarks.Scenario(
      s"footprint.buffers_${shown}_shown.${kind.label}",
      () => build(kind, shown, MultiBufferFixture.FullSize)
    )

  def build(kind: Kind, shown: Int, size: MultiBufferFixture.Size): Resident =
    val state  = MultiBufferFixture.state(kind, shown, size)
    val caches = RenderCaches.create()
    val image  = LaptopFrameBenchmarks.frameImage()
    (1 to shown).foreach { index =>
      val _ = LaptopFrameBenchmarks.renderedFrame(MultiBufferFixture.switchTo(state, BufferId(index)), caches, image)
    }
    Resident(state, caches, shown)
