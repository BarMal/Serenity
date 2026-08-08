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
  * This compares the large-document cost against a same-run, same-hardware small-document baseline rather than an
  * absolute wall-clock bound -- an earlier version used fixed millisecond thresholds calibrated to one machine and
  * failed as a false positive on a slower CI runner (Windows: ~1000ms average against a 500ms bound, despite the fix
  * being correctly in place). The regression this guards against is the large document costing disproportionately more
  * per keystroke than a small one on the *same* hardware, which is hardware-speed-independent.
  */
class CommandRunnerRenderPerformanceSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val MaxAllowedRatio     = 8.0
  private val AbsoluteSlackMillis = 50L

  "typing into the command runner over a large document" should "not cost disproportionately more per keystroke than typing over a small one" in {
    val smallAverage = averageKeystrokeMillis(lineCount = 1, scenarioName = "command-runner-render-perf-small-document")
    val largeAverage =
      averageKeystrokeMillis(lineCount = 50000, scenarioName = "command-runner-render-perf-large-document")

    info(s"small-document average: ${smallAverage}ms, large-document average: ${largeAverage}ms")

    largeAverage.toDouble should be < (smallAverage * MaxAllowedRatio + AbsoluteSlackMillis)
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
