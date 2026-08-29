package com.serenity.state.reducers

import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `Focused.bufferOf` (`#1067`) is the single pane->buffer resolution helper: `state.layout.editorPanes.get(paneId)
  * .flatMap(_.bufferId).flatMap(state.buffers.get)` used to be written out inline at several call sites
  * (`StateManagerViewportCapability`, `ModalEventReducer`, `PinnedPanelMouseHitTesting`) alongside
  * `FileEventReducer.bufferForPane`, which already abstracted the same lookup.
  */
class FocusedSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId    = PaneId(0)
  private val otherPane = PaneId(1)
  private val bufferId  = BufferId(0)

  private val baseState: AppState = AppState.initial

  "Focused.bufferOf(state, paneId)" should "resolve the buffer bound to that pane" in {
    Focused.bufferOf(baseState, paneId) shouldBe Some(baseState.persisted.buffers(bufferId))
  }

  it should "return None for a pane that does not exist" in {
    Focused.bufferOf(baseState, otherPane) shouldBe None
  }

  it should "return None for a pane with no bound buffer" in {
    val emptyPane = EditorPane.withBuffer(paneId, bufferId).copy(bufferId = None)
    val state = baseState.copy(persisted =
      baseState.persisted.copy(layout = baseState.persisted.layout.copy(editorPanes = Map(paneId -> emptyPane)))
    )
    Focused.bufferOf(state, paneId) shouldBe None
  }

  it should "return None when the pane's bufferId is not in buffers" in {
    val danglingPane = EditorPane.withBuffer(paneId, BufferId(999))
    val state = baseState.copy(persisted =
      baseState.persisted.copy(layout = baseState.persisted.layout.copy(editorPanes = Map(paneId -> danglingPane)))
    )
    Focused.bufferOf(state, paneId) shouldBe None
  }

  "Focused.bufferOf(state)" should "resolve the buffer of the active pane" in {
    Focused.bufferOf(baseState) shouldBe Some(baseState.persisted.buffers(bufferId))
  }

  it should "return None when there is no active pane" in {
    val state = baseState.copy(persisted =
      baseState.persisted.copy(layout = baseState.persisted.layout.copy(activeEditorPaneId = None))
    )
    Focused.bufferOf(state) shouldBe None
  }

  it should "agree with Focused.bufferOf(state, paneId) on the active pane" in {
    val activePaneId = baseState.persisted.layout.activeEditorPaneId.getOrElse(fail("expected an active pane"))
    Focused.bufferOf(baseState) shouldBe Focused.bufferOf(baseState, activePaneId)
  }
