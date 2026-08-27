package com.serenity.state.manager

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerEventPipelineSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneA   = PaneId(0)
  private val paneB   = PaneId(1)
  private val bufferA = BufferId(1)
  private val bufferB = BufferId(2)
  private val bufferC = BufferId(3)

  private def stateWith(focusedPane: PaneId): AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(
          bufferA -> Buffer.fromString(bufferA, "a"),
          bufferB -> Buffer.fromString(bufferB, "b"),
          bufferC -> Buffer.fromString(bufferC, "c")
        ),
        bufferOrder = List(bufferA, bufferB, bufferC),
        layout = Layout(
          editorPanes = Map(
            paneA -> EditorPane.withBuffer(paneA, bufferA),
            paneB -> EditorPane.withBuffer(paneB, bufferB)
          ),
          activeEditorPaneId = Some(focusedPane),
          paneOrder = List(paneA, paneB)
        ),
        focus = Focus.EditorPane(focusedPane)
      )
    )

  "StateManagerEventPipeline.candidateLspBufferIds" should
    "return only the focused buffer when focus doesn't change across the event" in {
      val previous = stateWith(paneA)
      val current  = previous

      StateManagerEventPipeline.candidateLspBufferIds(previous, current) shouldBe Set(bufferA)
    }

  it should "include both the previously and newly focused buffer when the event changes focus" in {
    val previous = stateWith(paneA)
    val current  = stateWith(paneB)

    StateManagerEventPipeline.candidateLspBufferIds(previous, current) shouldBe Set(bufferA, bufferB)
  }

  it should "never include a buffer that was never focused, even though it's open" in {
    val previous = stateWith(paneA)
    val current  = stateWith(paneA)

    StateManagerEventPipeline.candidateLspBufferIds(previous, current) should not contain bufferC
  }

  it should "return an empty set when neither state has editor-pane focus" in {
    val previous =
      stateWith(paneA).copy(persisted =
        stateWith(paneA).persisted.copy(focus = Focus.Surface(SurfaceId("command-runner")))
      )
    val current = previous

    StateManagerEventPipeline.candidateLspBufferIds(previous, current) shouldBe empty
  }
