package com.serenity.state.components

import com.serenity.keystroke.events.{
  Enter,
  Escape,
  Event,
  InsertChar,
  MoveLeft,
  MoveRight,
  MoveUp,
  NewLine,
  ReverseTabKey,
  TabKey
}
import com.serenity.lsp.LspEffect
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, LspQueueEffect, NoticeReducer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The keys of a notice that asks a question (#1847): moving the highlight, choosing, and dismissing are its own;
  * everything else is the editor's, which keeps typing while a language server waits.
  */
class NoticePromptComponentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val promptId  = NoticePromptId(3)
  private val component = NoticePromptComponent()

  private val asked: AppState =
    NoticeReducer.shown(
      AppState.initial,
      Notice(
        NoticeLevel.Info,
        "Scala language server: New build detected.",
        prompt = Some(NoticePrompt(promptId, List("Import build", "Not now")))
      ),
      0L
    )

  private def highlight(state: AppState): Option[Int] =
    NoticeReducer.visible(state).flatMap(_.prompt).headOption.flatMap(_.highlighted)

  private def applied(result: ComponentResult, state: AppState): AppState =
    result match
      case ComponentResult.StateChange(update) => update(state)
      case other                               => fail(s"expected a state change, got $other")

  private def moved(state: AppState, event: Event): AppState = applied(component.processEvent(event, state), state)

  private def answer(result: ComponentResult): (AppState, List[AppEffect]) =
    result match
      case ComponentResult.ReducerUpdate(reduced) => (reduced.state, reduced.effects)
      case other                                  => fail(s"expected an answer, got $other")

  private def chosen(index: Int): AppEffect =
    AppEffect.LspQueue(LspQueueEffect.Enqueue(LspEffect.MessageRequestAnswered(promptId, Some(index))))

  "A question notice" should "move its highlight right with Right and Tab, and left with Left and Shift+Tab" in {
    highlight(applied(component.processEvent(MoveRight, asked), asked)) shouldBe Some(0)
    highlight(applied(component.processEvent(TabKey, asked), asked)) shouldBe Some(0)
    highlight(applied(component.processEvent(MoveLeft, asked), asked)) shouldBe Some(1)
    highlight(applied(component.processEvent(ReverseTabKey, asked), asked)) shouldBe Some(1)
  }

  it should "answer with the highlighted action on Enter, closing itself and returning focus" in {
    val onSecond = moved(moved(asked, MoveRight), MoveRight)

    val (closed, effects) = answer(component.processEvent(Enter, onSecond))

    effects shouldBe List(chosen(1))
    NoticeReducer.visible(closed) shouldBe empty
    closed.persisted.focus shouldBe AppState.initial.persisted.focus
    answer(component.processEvent(NewLine, onSecond))._2 shouldBe List(chosen(1))
  }

  it should "leave Enter to the editor while nothing is highlighted" in {
    component.processEvent(Enter, asked) shouldBe ComponentResult.Unhandled
  }

  it should "answer with no choice on Escape" in {
    val (closed, effects) = answer(component.processEvent(Escape, asked))

    effects shouldBe List(
      AppEffect.LspQueue(LspQueueEffect.Enqueue(LspEffect.MessageRequestAnswered(promptId, None)))
    )
    NoticeReducer.visible(closed) shouldBe empty
  }

  it should "pass typing and cursor movement on to the editor" in {
    component.processEvent(InsertChar('x'), asked) shouldBe ComponentResult.Unhandled
    component.processEvent(MoveUp, asked) shouldBe ComponentResult.Unhandled
  }

  it should "ignore every key when no question holds focus" in {
    component.processEvent(MoveRight, AppState.initial) shouldBe ComponentResult.Unhandled
  }
