package com.serenity.state.reducers

import scala.concurrent.duration.*

import com.serenity.lsp.LspEffect
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A notice that asks a question (#1847): it takes focus so its actions can be chosen from the keyboard, waits for the
  * answer however low its level, and hands the answer back to the language server that asked.
  */
class NoticePromptReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val start    = 1_000_000_000L
  private val promptId = NoticePromptId(7)
  private val editor   = AppState.initial.persisted.focus

  private def question(id: NoticePromptId = promptId, actions: List[String] = List("Import build", "Not now")): Notice =
    Notice(
      NoticeLevel.Info,
      "Scala language server: New build detected.",
      prompt = Some(NoticePrompt(id, actions))
    )

  private def error(id: Int): Notice =
    Notice(NoticeLevel.Error, s"Couldn't save file$id.md.", topic = Some(NoticeTopic.FileSave(BufferId(id))))

  private def asked: AppState = NoticeReducer.shown(AppState.initial, question(), start)

  private def highlight(state: AppState): Option[Int] =
    NoticeReducer.visible(state).flatMap(_.prompt).headOption.flatMap(_.highlighted)

  "A notice with actions" should "take focus, as a surface its keys can be routed to" in {
    val shown   = asked
    val surface = shown.runtime.uiSurfaces.lastOption.getOrElse(fail("no surface was added"))

    surface.focusPolicy shouldBe SurfaceFocusPolicy.Focusable
    shown.persisted.focus shouldBe Focus.Surface(surface.id)
    NoticeReducer.visible(shown) shouldBe List(question())
  }

  it should "highlight nothing until the user moves, so a stray Enter chooses nothing" in {
    highlight(asked) shouldBe None
  }

  it should "wait for its answer however long it takes" in {
    val patient = NoticeReducer.expired(asked, start + 1.day.toNanos)

    NoticeReducer.visible(patient) shouldBe List(question())
  }

  it should "not be pushed out by the notices that follow it" in {
    val crowded = (1 to 3).foldLeft(asked)((state, id) => NoticeReducer.shown(state, error(id), start))

    NoticeReducer.visible(crowded) shouldBe List(question(), error(2), error(3))
  }

  it should "leave focus on the newest question, and return to the earlier one when that is answered" in {
    val first         = asked
    val second        = NoticeReducer.shown(first, question(NoticePromptId(8)), start)
    val firstSurface  = first.runtime.uiSurfaces.last.id
    val secondSurface = second.runtime.uiSurfaces.last.id

    second.persisted.focus shouldBe Focus.Surface(secondSurface)

    val back = NoticeReducer.answered(second, NoticePromptId(8), None).state
    back.persisted.focus shouldBe Focus.Surface(firstSurface)
    NoticeReducer.visible(back).flatMap(_.prompt).map(_.id) shouldBe List(promptId)
  }

  "Moving the highlight" should "start at the first action going forwards and the last going backwards" in {
    highlight(NoticeReducer.highlighted(asked, promptId, 1)) shouldBe Some(0)
    highlight(NoticeReducer.highlighted(asked, promptId, -1)) shouldBe Some(1)
  }

  it should "wrap around the actions" in {
    val forwards = LazyList.iterate(asked)(NoticeReducer.highlighted(_, promptId, 1)).drop(1).take(3).map(highlight)
    forwards.toList shouldBe List(Some(0), Some(1), Some(0))

    val onFirst = NoticeReducer.highlighted(NoticeReducer.highlighted(asked, promptId, 1), promptId, -1)
    highlight(onFirst) shouldBe Some(1)
  }

  it should "leave the state alone for a question that is not on screen" in {
    val state = asked

    NoticeReducer.highlighted(state, NoticePromptId(99), 1) should be theSameInstanceAs state
  }

  "Answering" should "close the question, return focus, and tell the language server which action was chosen" in {
    val result = NoticeReducer.answered(asked, promptId, Some(1))

    NoticeReducer.visible(result.state) shouldBe empty
    result.state.persisted.focus shouldBe editor
    result.effects shouldBe List(
      AppEffect.LspQueue(LspQueueEffect.Enqueue(LspEffect.MessageRequestAnswered(promptId, Some(1))))
    )
  }

  it should "report a dismissal as no choice" in {
    NoticeReducer.answered(asked, promptId, None).effects shouldBe List(
      AppEffect.LspQueue(LspQueueEffect.Enqueue(LspEffect.MessageRequestAnswered(promptId, None)))
    )
  }

  it should "not pass on an action the question never offered" in {
    NoticeReducer.answered(asked, promptId, Some(2)).effects shouldBe List(
      AppEffect.LspQueue(LspQueueEffect.Enqueue(LspEffect.MessageRequestAnswered(promptId, None)))
    )
  }

  it should "do nothing for a question already withdrawn, so a late answer cannot answer twice" in {
    val state  = asked
    val result = NoticeReducer.answered(state, NoticePromptId(99), Some(0))

    result.state should be theSameInstanceAs state
    result.effects shouldBe empty
  }

  "Withdrawing a question" should "close it and return focus without telling anyone" in {
    val withdrawn = NoticeReducer.withdrawn(asked, promptId)

    NoticeReducer.visible(withdrawn) shouldBe empty
    withdrawn.persisted.focus shouldBe editor
  }

  it should "leave other notices and an unknown question alone" in {
    val shown = NoticeReducer.shown(asked, error(1), start)

    NoticeReducer.visible(NoticeReducer.withdrawn(shown, promptId)) shouldBe List(error(1))
    NoticeReducer.withdrawn(shown, NoticePromptId(99)) should be theSameInstanceAs shown
  }
