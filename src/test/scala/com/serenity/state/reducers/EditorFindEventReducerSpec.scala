package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated coverage for `EditorFindEventReducer` (#1442), focused on behavior not already exercised by
  * `EditorEventReducerSpec`'s extensive `OpenFind`/`FindNext` coverage: `OpenReplace` (untested anywhere at the
  * reducer level), and `FindNext`'s no-op paths when there is nothing to advance to.
  */
class EditorFindEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateWith(text: String, findState: Option[FindState] = None): AppState =
    val buffer = Buffer.fromString(bufferId, text).copy(findState = findState)
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  "OpenReplace" should "open the replace workflow modal with fresh state" in {
    val before = stateWith("alpha beta")

    val after        = EditorEventReducer.reduce(OpenReplace, paneId, before).state
    val modalSurface = after.modalSurface

    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(ReplaceWorkflowState())))
    after.persisted.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "never mutate the buffer" in {
    val before = stateWith("alpha beta")

    val after = EditorEventReducer.reduce(OpenReplace, paneId, before).state.persisted.buffers(bufferId)

    after.document.content.collect() shouldBe "alpha beta"
  }

  "FindNext" should "be a no-op when the buffer has no find state at all" in {
    val before = stateWith("alpha beta", findState = None)

    val result = EditorEventReducer.reduce(FindNext, paneId, before)

    result.state shouldBe before
    result.effects shouldBe Nil
  }

  it should "be a no-op when the stored find state has no results to advance through" in {
    val before = stateWith("alpha beta", findState = Some(FindState("zzz", Nil, 0)))

    val result = EditorEventReducer.reduce(FindNext, paneId, before)

    result.state shouldBe before
    result.effects shouldBe Nil
  }

  "Every find/goto/replace event" should "never record an undo boundary, since none of them edit the buffer" in {
    val before = stateWith("alpha beta", findState = Some(FindState("alpha", List(FindResult(0, 0)), 0)))

    List(OpenGotoLine, OpenFind, OpenReplace, FindNext).foreach { event =>
      withClue(s"$event: ") {
        EditorEventReducer.reduce(event, paneId, before).effects shouldBe Nil
      }
    }
  }
