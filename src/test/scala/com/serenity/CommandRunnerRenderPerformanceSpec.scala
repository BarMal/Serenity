package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, EditorPane, PaneId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Diagnostic coverage for #892/#930: typing into the command runner over a large document must not force a full
  * editor-scene rebuild per keystroke. The actual, deterministic regression guard for this lives in
  * MouseTargetCacheSpec (object-identity checks on the cached scene, independent of timing). This spec logs the real
  * per-keystroke wall-clock cost for human review, but does not assert on it.
  *
  * Two earlier versions of this spec asserted on timing and both produced false failures under CI load: an absolute
  * millisecond bound failed on a slower Windows runner even with the fix correctly in place, and a same-run
  * relative-ratio bound (large document vs. small document) failed on a Linux runner under the full suite's
  * parallel-test CPU contention, where a large-document keystroke's larger workload is more exposed to scheduling
  * delays than a tiny one -- a contention artifact, not a regression. Wall-clock timing assertions are fundamentally
  * unreliable on shared CI hardware; MouseTargetCacheSpec's identity-based checks are what actually has to stay green.
  */
class CommandRunnerRenderPerformanceSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "typing into the command runner over a large document" should "report its per-keystroke render cost" in {
    val smallAverage = averageKeystrokeMillis(lineCount = 1, scenarioName = "command-runner-render-perf-small-document")
    val largeAverage =
      averageKeystrokeMillis(lineCount = 50000, scenarioName = "command-runner-render-perf-large-document")

    info(s"small-document average: ${smallAverage}ms, large-document average: ${largeAverage}ms")
  }

  private def averageKeystrokeMillis(lineCount: Int, scenarioName: String): Long =
    val driver = UiScenarioDriver.create(scenarioName).unsafeRunSync()

    val bigLine = "x" * 120
    val content = (1 to lineCount).map(_ => bigLine).mkString("\n")
    driver
      .updateState { state =>
        val bufferId = BufferId(1)
        val paneId   = state.layout.activeEditorPaneId.getOrElse(PaneId(1))
        val buffer   = Buffer.fromString(bufferId, content)
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

    timings.sum / timings.length
