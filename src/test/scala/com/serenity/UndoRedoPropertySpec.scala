package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Property-based coverage for undo/redo (#1472), complementing `UndoRedoSpec`'s hand-picked scenarios.
  *
  * Scoped to a single buffer and a single cursor: `UndoState.pushUndo`/`pushRedo` and `HistoryEntry.BufferEdit`'s
  * `restore` make no assumption about cursor count, but a generator that also spreads edits across multiple cursors or
  * panes would mostly be asserting `EditorTextEditReducer`'s multi-cursor edit semantics rather than the undo/redo
  * round-trip itself. `UndoRedoSpec` already covers the multi-cursor cases by hand.
  */
class UndoRedoPropertySpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  // Each successful case spins up a fresh StateManager and drives it through up to 30 edits and as many undo/redo
  // steps, so keep the sample count well below ScalaCheck's 100-case default rather than let the suite balloon.
  given generatorConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 30)

  private val logger = LoggerFactory[IO].getLogger(using LoggerName("UndoRedoPropertySpec"))

  private def genEdit: Gen[Event] =
    Gen.frequency(
      6 -> Gen.alphaNumChar.map(InsertChar.apply),
      2 -> Gen.const(DeleteBackward),
      1 -> Gen.const(NewLine)
    )

  private def genEdits: Gen[List[Event]] = Gen.choose(1, 30).flatMap(n => Gen.listOfN(n, genEdit))

  private def genStartingContent: Gen[String] =
    Gen.choose(0, 10).flatMap(n => Gen.listOfN(n, Gen.alphaNumChar).map(_.mkString))

  private def freshStateManager(): StateManager =
    StateManager
      .apply(logger, policy = SessionManager.SessionPolicy())
      .unsafeRunSync()

  private def contentOf(stateManager: StateManager, bufferId: BufferId): String =
    stateManager.getCurrentState
      .unsafeRunSync()
      .persisted
      .buffers
      .get(bufferId)
      .map(_.document.content.collect())
      .getOrElse("")

  property(
    "undoing every applied edit restores the original content, and redoing every undo restores the edited content"
  ) {
    forAll(genStartingContent, genEdits) { (startingContent, edits) =>
      val stateManager = freshStateManager()
      val bufferId     = stateManager.bufferManager.createBuffer(startingContent, None).unsafeRunSync()
      val paneId       = stateManager.getCurrentState.unsafeRunSync().persisted.layout.editorPanes.keys.head
      stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
      if startingContent.nonEmpty then stateManager.setCursorPosition(paneId, 0, startingContent.length).unsafeRunSync()

      edits.foreach(event => stateManager.applyEvent(event).unsafeRunSync())
      val editedContent = contentOf(stateManager, bufferId)

      // The undo stack holds at most one entry per applied edit (consecutive InsertChars coalesce into fewer), so
      // undoing `edits.size` times is always enough to drain it -- further undos beyond that are a documented no-op.
      (1 to edits.size).foreach(_ => stateManager.applyEvent(Undo).unsafeRunSync())
      contentOf(stateManager, bufferId) shouldBe startingContent

      (1 to edits.size).foreach(_ => stateManager.applyEvent(Redo).unsafeRunSync())
      contentOf(stateManager, bufferId) shouldBe editedContent
    }
  }

  property("undoing partway and redoing the same number of steps returns to the fully edited content") {
    forAll(genStartingContent, genEdits) { (startingContent, edits) =>
      val stateManager = freshStateManager()
      val bufferId     = stateManager.bufferManager.createBuffer(startingContent, None).unsafeRunSync()
      val paneId       = stateManager.getCurrentState.unsafeRunSync().persisted.layout.editorPanes.keys.head
      stateManager.setBufferForPane(paneId, bufferId).unsafeRunSync()
      if startingContent.nonEmpty then stateManager.setCursorPosition(paneId, 0, startingContent.length).unsafeRunSync()

      edits.foreach(event => stateManager.applyEvent(event).unsafeRunSync())
      val editedContent = contentOf(stateManager, bufferId)

      val partialSteps = edits.size / 2
      (1 to partialSteps).foreach(_ => stateManager.applyEvent(Undo).unsafeRunSync())
      (1 to partialSteps).foreach(_ => stateManager.applyEvent(Redo).unsafeRunSync())
      contentOf(stateManager, bufferId) shouldBe editedContent
    }
  }
