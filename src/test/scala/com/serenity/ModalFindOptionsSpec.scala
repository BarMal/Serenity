package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, ModalEventReducer, WorkflowEffect}
import com.serenity.ui.widget.TextField
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModalFindOptionsSpec extends AnyFlatSpec with Matchers with FindModalFixtures:

  "ModalEventReducer" should "search again with the flipped option when an option is toggled" in {
    val result = ModalEventReducer.reduce(
      ModalType.Find,
      ModalToggleFindOption(FindOption.MatchCase),
      stateWithFindModal("needle", "Needle needle")
    )

    activeFindModal(result.state) shouldBe Some(
      Modal.Find(TextField.of("needle"), Vector.empty, 0, FindOptions(matchCase = true))
    )
    result.effects should matchPattern {
      case List(
            AppEffect.Workflow(
              WorkflowEffect.RefreshFind(
                FindSearchRequest(_, _, "needle", _, FindOptions(true, false, false), 0, FindSearchPurpose.Seed)
              )
            )
          ) =>
    }
  }

  it should "not search a query that is not a valid regex" in {
    val result = ModalEventReducer.reduce(
      ModalType.Find,
      ModalToggleFindOption(FindOption.Regex),
      stateWithFindModal("ne(dle", "ne(dle")
    )

    result.effects shouldBe Nil
    result.state.persisted.buffers(BufferId(0)).findState shouldBe None
  }

  it should "seed the search from the caret so a capped window holds the matches around it" in {
    val result = ModalEventReducer.reduce(
      ModalType.Find,
      ModalInsertChar('e'),
      stateWithFindModal("needl", "needle\nneedle", CursorPosition(1, 2))
    )

    result.effects should matchPattern {
      case List(AppEffect.Workflow(WorkflowEffect.RefreshFind(FindSearchRequest(_, _, "needle", _, _, 9, _)))) =>
    }
  }

  it should "step back to the previous match with find-previous, wrapping to the last" in {
    val found = completeFind(stateWithFindModal("needle", "needle one\nneedle two\nneedle three"))

    val last   = ModalEventReducer.reduce(ModalType.Find, ModalFindPrevious, found).state
    val second = ModalEventReducer.reduce(ModalType.Find, ModalFindPrevious, last).state

    last.persisted.buffers(BufferId(0)).editing.cursorPositions.head shouldBe CursorPosition(2, 0)
    second.persisted.buffers(BufferId(0)).editing.cursorPositions.head shouldBe CursorPosition(1, 0)
    activeFindModal(second).collect { case find: Modal.Find => find.currentIndex } shouldBe Some(1)
  }
