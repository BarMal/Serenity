package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, EditorPane, PaneId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Regression coverage for #892/#930: closing the command runner from a nested settings edit must stay fast and
  * must settle its close animation, even with a large document open in the background.
  */
class CommandRunnerCloseAnimationPerformanceSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val SettleBoundMillis = 2000L

  "closing the command runner" should "dispatch and settle quickly with a large document open" in {
    val driver = UiScenarioDriver.create("command-runner-close-with-large-document").unsafeRunSync()

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
    "open settings".foreach(char => driver.dispatch(InsertChar(char)).unsafeRunSync())
    driver.dispatch(Enter).unsafeRunSync()
    "blur radius".foreach(char => driver.dispatch(InsertChar(char)).unsafeRunSync())
    driver.dispatch(Enter).unsafeRunSync()

    val dispatchStart = System.nanoTime()
    driver.dispatch(Escape).unsafeRunSync()
    val dispatchMillis = (System.nanoTime() - dispatchStart) / 1000000L

    val settleStart = System.nanoTime()
    val settled      = driver.advanceToSettled().unsafeRunSync()
    val settleMillis = (System.nanoTime() - settleStart) / 1000000L

    info(s"Escape dispatch took ${dispatchMillis}ms, advanceToSettled took ${settleMillis}ms")

    settled shouldBe true
    dispatchMillis should be < SettleBoundMillis
    settleMillis should be < SettleBoundMillis
  }
