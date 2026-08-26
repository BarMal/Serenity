package com.serenity.testkit

import cats.syntax.all.*
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, Buffer, BufferId, EditorPane, PaneId}
import com.serenity.state.reducers.{Focused, ReducerResult, Transition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The lens laws are stated for the partial case: these target a buffer that may not be there, so get-put and put-put
  * must hold when it is, and every operation must be a no-op when it is not.
  */
class FocusedLensSpec extends AnyFlatSpec with Matchers:

  private given Balance = Balance.default

  private val paneId   = PaneId(1)
  private val bufferId = BufferId(1)

  private val focusedState: AppState =
    val buffer = Buffer.empty(bufferId)
    AppState.initial.copy(
      layout = AppState.initial.layout.copy(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      buffers = Map(bufferId -> buffer)
    )

  private val unfocusedState: AppState =
    focusedState.copy(layout = focusedState.layout.copy(activeEditorPaneId = None))

  private def run(state: AppState)(transition: Transition[Unit]): AppState =
    ReducerResult.fromTransition(state, transition).state

  "Focused.buffer" should "get the buffer under the cursor" in {
    Focused.bufferOf(focusedState).map(_.id) shouldBe Some(bufferId)
  }

  it should "satisfy get-put: writing back what was read changes nothing" in {
    run(focusedState)(Focused.modifyBuffer(identity)) shouldBe focusedState
  }

  it should "satisfy put-get: what was written is what is read back" in {
    val marked = run(focusedState)(Focused.modifyBuffer(b => b.copy(document = b.document.copy(isDirty = true))))

    Focused.bufferOf(marked).map(_.document.isDirty) shouldBe Some(true)
  }

  it should "satisfy put-put: the last write wins" in {
    val twice =
      run(focusedState)(
        Focused.modifyBuffer(b => b.copy(document = b.document.copy(isDirty = true))) *> Focused.modifyBuffer(b =>
          b.copy(document = b.document.copy(isDirty = false))
        )
      )
    val once = run(focusedState)(Focused.modifyBuffer(b => b.copy(document = b.document.copy(isDirty = false))))

    twice shouldBe once
  }

  it should "absorb a missing focus rather than making the caller match" in {
    Focused.bufferOf(unfocusedState) shouldBe None
    run(unfocusedState)(Focused.modifyBuffer(b => b.copy(document = b.document.copy(isDirty = true)))) shouldBe unfocusedState
  }

  it should "leave other buffers untouched" in {
    val otherId    = BufferId(2)
    val withSecond = focusedState.copy(buffers = focusedState.buffers + (otherId -> Buffer.empty(otherId)))

    val updated = run(withSecond)(Focused.modifyBuffer(b => b.copy(document = b.document.copy(isDirty = true))))

    updated.buffers(otherId).document.isDirty shouldBe false
  }

  "Focused.bufferWithId" should "satisfy the same laws for an addressed buffer" in {
    run(focusedState)(Focused.modifyBufferWithId(bufferId)(identity)) shouldBe focusedState

    val marked = run(focusedState)(Focused.modifyBufferWithId(bufferId)(b => b.copy(document = b.document.copy(isDirty = true))))
    marked.buffers(bufferId).document.isDirty shouldBe true

    run(focusedState)(Focused.modifyBufferWithId(BufferId(99))(b => b.copy(document = b.document.copy(isDirty = true)))) shouldBe focusedState
  }

  "Focused.pane" should "satisfy get-put, put-get and put-put" in {
    run(focusedState)(Focused.modifyPane(identity)) shouldBe focusedState

    val cleared = run(focusedState)(Focused.modifyPane(_.copy(bufferId = None)))
    Focused.paneOf(cleared).flatMap(_.bufferId) shouldBe None

    val twice = run(focusedState)(
      Focused.modifyPane(_.copy(bufferId = None)) *> Focused.modifyPane(_.copy(bufferId = Some(bufferId)))
    )
    val once = run(focusedState)(Focused.modifyPane(_.copy(bufferId = Some(bufferId))))
    twice shouldBe once
  }

  it should "absorb a missing pane" in {
    Focused.paneOf(unfocusedState) shouldBe None
    run(unfocusedState)(Focused.modifyPane(_.copy(bufferId = None))) shouldBe unfocusedState
  }

  "Focused" should "read the same buffer through the focused and addressed routes" in {
    Focused.bufferOf(focusedState).map(_.id) shouldBe Focused.bufferOf(focusedState, paneId).map(_.id)
  }

  it should "compose in a for-comprehension without a None arm" in {
    val transition =
      for
        maybeBuffer <- Focused.buffer
        _ <- Focused.modifyBuffer(b => b.copy(document = b.document.copy(isDirty = maybeBuffer.isDefined)))
      yield ()

    run(focusedState)(transition).buffers(bufferId).document.isDirty shouldBe true
    run(unfocusedState)(transition) shouldBe unfocusedState
  }
