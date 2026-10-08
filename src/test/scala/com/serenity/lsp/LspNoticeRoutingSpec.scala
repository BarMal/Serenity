package com.serenity.lsp

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{Enter, Escape, InsertChar, MoveRight}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.state.reducers.NoticeReducer
import com.serenity.testkit.AwaitCondition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** What a language server's question does on its way through a composed `StateManager` (#1847): the notice it is shown
  * as takes the keyboard, and the key that answers it comes back out of the LSP effect stream.
  */
class LspNoticeRoutingSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val promptId = NoticePromptId(5)

  private val question = Notice(
    NoticeLevel.Info,
    "Scala language server: New build detected.",
    prompt = Some(NoticePrompt(promptId, List("Import build", "Not now")))
  )

  private def asked: StateManager =
    val manager =
      StateManager.apply(LoggerFactory[IO].getLogger(using LoggerName("LspNoticeRoutingSpec"))).unsafeRunSync()
    manager.lspEffectSource.notices.show(question).unsafeRunSync()
    awaitQuestion(manager)(_.nonEmpty)
    manager

  private def awaitQuestion(manager: StateManager)(ready: List[Notice] => Boolean): List[Notice] =
    AwaitCondition
      .awaitValue(manager.getCurrentState.map(NoticeReducer.visible))(ready)
      .timeout(10.seconds)
      .unsafeRunSync()

  private def answered(manager: StateManager): List[LspEffect] =
    manager.lspEffectSource.lspEffectStream.take(1).timeout(5.seconds).compile.toList.unsafeRunSync()

  "A language server's question" should "take the keyboard as soon as it is shown" in {
    val manager = asked

    manager.getCurrentState.unsafeRunSync().activeSurface.map(_.content) match
      case Some(SurfaceContent.Notice(shown, _)) => shown shouldBe question
      case other                                 => fail(s"the question does not hold focus: $other")
  }

  it should "send the highlighted action back to the language server on Enter" in {
    val manager = asked

    manager.applyEvent(MoveRight).unsafeRunSync()
    manager.applyEvent(MoveRight).unsafeRunSync()
    manager.applyEvent(Enter).unsafeRunSync()

    answered(manager) shouldBe List(LspEffect.MessageRequestAnswered(promptId, Some(1)))
    awaitQuestion(manager)(_.isEmpty) shouldBe empty
    manager.getCurrentState.unsafeRunSync().persisted.focus shouldBe a[Focus.EditorPane]
  }

  it should "send a dismissal back on Escape" in {
    val manager = asked

    manager.applyEvent(Escape).unsafeRunSync()

    answered(manager) shouldBe List(LspEffect.MessageRequestAnswered(promptId, None))
    awaitQuestion(manager)(_.isEmpty) shouldBe empty
  }

  it should "stay open, and answer nothing, while the user types or presses Enter for the editor" in {
    val manager = asked

    manager.applyEvent(InsertChar('x')).unsafeRunSync()
    manager.applyEvent(Enter).unsafeRunSync()

    NoticeReducer.visible(manager.getCurrentState.unsafeRunSync()) shouldBe List(question)
    manager.lspEffectSource.lspEffectStream.interruptAfter(300.millis).compile.toList.unsafeRunSync() shouldBe empty
  }

  it should "leave the screen without an answer when its server stops waiting" in {
    val manager = asked

    manager.lspEffectSource.notices.withdraw(promptId).unsafeRunSync()

    awaitQuestion(manager)(_.isEmpty) shouldBe empty
    manager.lspEffectSource.lspEffectStream.interruptAfter(300.millis).compile.toList.unsafeRunSync() shouldBe empty
  }
