package com.serenity

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.keystroke.events.{CursorPeekOtherKeyPressed, InsertChar}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.{AppState, Focus}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** A commit's follow-up work (#1845) -- validation fix-ups, analysis scheduling, the commit observer -- runs once per
  * dispatch that changes the app state, and not at all for one that leaves it as it was.
  */
class StateManagerCommitFollowUpSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val logger = LoggerFactory[IO].getLogger(using LoggerName("StateManagerCommitFollowUpSpec"))

  private def editorWithEmptyBuffer: IO[StateManager] =
    for
      stateManager <- StateManager(logger)
      bufferId     <- stateManager.createNewEmptyBuffer
      state        <- stateManager.getCurrentState
      _            <- stateManager.setBufferForPane(state.persisted.layout.editorPanes.keys.head, bufferId)
    yield stateManager

  private def focusedText(state: AppState): Option[String] =
    state.persisted.focus match
      case Focus.EditorPane(paneId) =>
        state.persisted.layout.editorPanes
          .get(paneId)
          .flatMap(_.bufferId)
          .flatMap(state.persisted.buffers.get)
          .map(_.document.content.toString)
      case _ => None

  private def observedCommits(work: StateManager => IO[Unit]): (Int, AppState) =
    val program = for
      stateManager <- editorWithEmptyBuffer
      observed     <- Ref.of[IO, Int](0)
      _            <- stateManager.runtimeLifecycle.observeCommits((_, _) => observed.update(_ + 1))
      _            <- work(stateManager)
      count        <- observed.get
      state        <- stateManager.getCurrentState
    yield (count, state)

    program.unsafeRunSync()

  "A typed character" should "reach the commit observer exactly once" in {
    val (commits, state) = observedCommits(_.applyEvent(InsertChar('a')))

    focusedText(state) shouldBe Some("a")
    commits shouldBe 1
  }

  "An event that leaves the state unchanged" should "not reach the commit observer" in {
    val (commits, state) = observedCommits(_.applyEvent(CursorPeekOtherKeyPressed))

    state.persisted.config.surfaceConfig.commandRunnerCursorPeekEnabled shouldBe false
    commits shouldBe 0
  }

  "A commit of the state already committed" should "skip its follow-up work" in {
    val (commits, _) = observedCommits(_.updateStateValidated(identity))

    commits shouldBe 0
  }
