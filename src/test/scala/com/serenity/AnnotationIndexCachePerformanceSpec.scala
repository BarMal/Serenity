package com.serenity

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1456: `annotationIndexByBuffer`/`markdownFenceIndexByBuffer` were `lazy val`s that rebuilt a whole-workspace
  * wrapper map -- one entry per buffer -- on first touch, just to reach a single buffer's index, even though the
  * expensive per-buffer index computation itself was already deferred behind a thunk. Since a fresh `AppState` snapshot
  * is produced on essentially every edit (each with its own unforced `lazy val`), that O(buffers) map build repeated on
  * every first lookup against a new snapshot. This logs the real wall-clock cost of that first lookup against a fresh
  * snapshot of a workspace with a large buffer count, in the same spirit as `WordStatisticsPerformanceSpec`, and split
  * out to its own file (as `RendererSnapshotReuseSpec` did for #1477/#1481) to keep that file under the architecture
  * ratchet's line-count target.
  */
class AnnotationIndexCachePerformanceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def buildState: AppState =
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val buffer   = Buffer.newEmpty(bufferId)
    val base     = AppState.initial
    base.copy(persisted =
      base.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        )
      )
    )

  it should "look up one buffer's annotation/fence index in a workspace with many buffers without a per-edit lag spike" in {
    val bufferId = BufferId(0)
    val buffers  = (0 until 20000).map(i => BufferId(i) -> Buffer.newEmpty(BufferId(i))).toMap
    val base     = buildState
    val withManyBuffers =
      base.copy(persisted = base.persisted.copy(buffers = buffers, bufferOrder = buffers.keys.toList))

    // Each timing forces a brand-new `AppState` instance (via `copy`), matching how a real edit produces one --
    // `copy` alone gives every instance its own unforced per-buffer cache, so this measures first-touch cost, not a
    // warm one.
    val timings = (1 to 20).map { _ =>
      val state = withManyBuffers.copy()
      val start = System.nanoTime()
      val index = state.annotationIndex(bufferId).getOrElse(fail("expected an index for a buffer that exists"))
      val fence = state.markdownFenceIndex(bufferId).getOrElse(fail("expected a fence index for a buffer that exists"))
      index.comments shouldBe empty
      fence.ranges shouldBe empty
      (System.nanoTime() - start) / 1000000L
    }

    val average = timings.sum / timings.length
    info(
      s"first-touch annotation+fence index lookup for one buffer among 20k -- average: ${average}ms, max: ${timings.max}ms"
    )

    // A per-buffer lookup must stay independent of workspace size: this is the O(buffers) whole-workspace-map-rebuild
    // regression #1456 fixed (rebuilding a 20k-entry wrapper map on every first touch, per snapshot) actually being
    // caught, not just measured. These bounds are deliberately generous -- this shared/CI hardware is not a reliable
    // stopwatch (see `CommandRunnerRenderPerformanceSpec`'s doc for prior flaky wall-clock assertions here) -- but an
    // O(buffers) rebuild of two 20k-entry maps per lookup, 20 times over, is orders of magnitude past either bound;
    // a real regression trips this even under heavy contention, while a correct O(1) lookup has ample headroom.
    average should be < 500L
    timings.max should be < 2000L
  }
