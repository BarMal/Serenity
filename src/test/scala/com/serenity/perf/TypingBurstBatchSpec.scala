package com.serenity.perf

import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.state.manager.{StateManager, TypedRuns}
import com.serenity.state.models.{Buffer, BufferId}
import com.serenity.ui.layout.WrappedLineCache
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A typing burst queues up faster than one keystroke at a time can drain it (#1985), so the input loop hands the
  * queued keys to `applyEventBatch` together. The batch only drains faster than its keys arrive if the work that serves
  * the final state -- re-wrapping the edited paragraph and centring the cursor on it -- runs once per settled run of
  * typed keys rather than once per key (#1879).
  */
class TypingBurstBatchSpec extends AnyFlatSpec with Matchers:

  private val burstKeys = 200

  private def burst: List[Char] = TypingBurst.letters.take(burstKeys).toList

  private def wraps(stateManager: StateManager): Long =
    stateManager.renderCaches.wrappedLines match
      case cache: WrappedLineCache.Bounded => cache.wrapStats.coldWraps + cache.wrapStats.incrementalWraps
      case other                           => fail(s"the editor's wrap cache keeps no stats: $other")

  private def typedBuffer(editor: (StateManager, BufferId)): Option[Buffer] =
    val (stateManager, bufferId) = editor
    stateManager.getCurrentState.unsafeRunSync().persisted.buffers.get(bufferId)

  /** A prose editor that has already typed one key, so its wrap cache holds the rows around the cursor. */
  private def warmEditor(): (StateManager, BufferId) =
    val editor @ (stateManager, _) =
      LaptopFrameBenchmarks.proseStateManager(config = AppConfig.default.withWordWrap(true))
    val _ = TypingBurst.typeEachAlone(stateManager, List('w')).unsafeRunSync()
    editor

  "A 200-key typing burst applied as one batch" should "re-wrap once per settled run, not once per key" in {
    val (alone, _) = warmEditor()
    val perKey = burst.map { char =>
      val before = wraps(alone)
      val _      = TypingBurst.typeEachAlone(alone, List(char)).unsafeRunSync()
      wraps(alone) - before
    }
    val mostPerKey = perKey.maxOption.getOrElse(0L)

    val (batched, _) = warmEditor()
    val before       = wraps(batched)
    val slices       = TypingBurst.typeAsOneBatch(batched, burst).unsafeRunSync()
    val batchWraps   = wraps(batched) - before
    val settledRuns  = slices + burstKeys / TypedRuns.MaxKeys

    withClue(
      s"keys=$burstKeys slices=$slices runs<=$settledRuns wraps one key at a time=${perKey.sum} (most per key $mostPerKey): "
    ) {
      mostPerKey should be > 0L
      batchWraps should be <= settledRuns * mostPerKey
    }
  }

  it should "leave the same text, cursors and viewport as applying each key on its own" in {
    val alone   = warmEditor()
    val batched = warmEditor()
    val _       = TypingBurst.typeEachAlone(alone(0), burst).unsafeRunSync()
    val _       = TypingBurst.typeAsOneBatch(batched(0), burst).unsafeRunSync()

    val expected = typedBuffer(alone).map(buffer => (buffer.document.content.toString, buffer.editing, buffer.viewport))
    val actual = typedBuffer(batched).map(buffer => (buffer.document.content.toString, buffer.editing, buffer.viewport))
    actual shouldBe expected
  }

  "TypingBurst" should "count every slice the batch was published in" in {
    val (stateManager, _) = warmEditor()
    val slices            = TypingBurst.typeAsOneBatch(stateManager, burst.take(3)).unsafeRunSync()
    slices should (be >= 1L and be <= 3L)
  }

  it should "publish a lone key in the one slice that applied it" in {
    val (stateManager, _) = warmEditor()
    TypingBurst.typeAsOneBatch(stateManager, burst.take(1)).unsafeRunSync() shouldBe 1L
  }
