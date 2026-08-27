package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, EditorPane, PaneId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for #892/#930: closing the command runner from a nested settings edit must settle its close
  * animation (not hang forever) even with a large document open in the background. The dispatch/settle timings are
  * logged for human review but not asserted on -- see CommandRunnerRenderPerformanceSpec for why wall-clock assertions
  * are unreliable on shared CI hardware. `settled shouldBe true` is the actual regression guard here: it fails if the
  * close animation ever gets stuck rather than converging.
  */
class CommandRunnerCloseAnimationPerformanceSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  "closing the command runner" should "settle its close animation with a large document open" in {
    val driver = UiScenarioDriver.create("command-runner-close-with-large-document").unsafeRunSync()

    val bigLine    = "x" * 120
    val bigContent = (1 to 50000).map(_ => bigLine).mkString("\n")
    driver
      .updateState { state =>
        val bufferId = BufferId(1)
        val paneId   = state.persisted.layout.activeEditorPaneId.getOrElse(PaneId(1))
        val buffer   = Buffer.fromString(bufferId, bigContent)
        state.copy(
          persisted = state.persisted.copy(
            buffers = Map(bufferId -> buffer),
            bufferOrder = List(bufferId),
            layout = state.persisted.layout.copy(
              editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
              activeEditorPaneId = Some(paneId),
              paneOrder = List(paneId)
            )
          )
        )
      }
      .unsafeRunSync()

    driver.dispatch(ToggleCommandRunner).unsafeRunSync()
    "open settings".foreach(char => driver.dispatch(InsertChar(char)).unsafeRunSync())
    driver.dispatch(Enter).unsafeRunSync()
    "blur radius".foreach(char => driver.dispatch(InsertChar(char)).unsafeRunSync())
    driver.dispatch(Enter).unsafeRunSync()

    val dispatchStart = System.nanoTime()
    driver.dispatch(Escape).unsafeRunSync()
    val dispatchMillis = (System.nanoTime() - dispatchStart) / 1000000L

    val settleStart  = System.nanoTime()
    val settled      = driver.advanceToSettled().unsafeRunSync()
    val settleMillis = (System.nanoTime() - settleStart) / 1000000L

    info(s"Escape dispatch took ${dispatchMillis}ms, advanceToSettled took ${settleMillis}ms")

    settled shouldBe true
  }
