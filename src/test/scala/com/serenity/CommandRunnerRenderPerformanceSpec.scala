package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, EditorPane, PaneId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for #892/#930: typing into the command runner over a large document must not force a full
  * editor-scene rebuild per keystroke. See MouseTargetCacheSpec for the cache-key-level coverage this depends on; this
  * spec measures the actual per-keystroke render cost end to end.
  *
  * The bounds here are deliberately loose -- they exist to catch a regression back toward the pre-fix cost (measured
  * ~575ms average / ~2.3s max on a 50,000-line document), not to enforce a specific target. The remaining per-keystroke
  * cost (measured ~277ms average / ~700ms max after this fix) has a further, unidentified cause -- see the #892 comment
  * thread.
  */
class CommandRunnerRenderPerformanceSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val AverageBoundMillis = 500L
  private val MaxBoundMillis     = 1200L

  "rendering while the command runner is open over a large document" should "stay well under the pre-fix per-keystroke cost" in {
    val driver = UiScenarioDriver.create("command-runner-render-perf-large-document").unsafeRunSync()

    val bigLine    = "x" * 120
    val bigContent = (1 to 50000).map(_ => bigLine).mkString("\n")
    driver
      .updateState { state =>
        val bufferId = BufferId(1)
        val paneId   = state.layout.activeEditorPaneId.getOrElse(PaneId(1))
        val buffer   = Buffer.fromString(bufferId, bigContent)
        state.copy(
          buffers = Map(bufferId -> buffer),
          bufferOrder = List(bufferId),
          layout = state.layout.copy(
            editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
            activeEditorPaneId = Some(paneId),
            paneOrder = List(paneId)
          )
        )
      }
      .unsafeRunSync()

    driver.dispatch(ToggleCommandRunner).unsafeRunSync()

    val timings = "blur radius".map { char =>
      val start = System.nanoTime()
      driver.dispatch(InsertChar(char)).unsafeRunSync()
      driver.renderFrame(s"keystroke-$char").unsafeRunSync()
      (System.nanoTime() - start) / 1000000L
    }
    val average = timings.sum / timings.length

    info(s"per-keystroke render timings (ms): ${timings.mkString(", ")}")
    info(s"max: ${timings.max}ms, avg: ${average}ms")

    average should be < AverageBoundMillis
    timings.max should be < MaxBoundMillis
  }
