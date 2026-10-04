package com.serenity.state.manager

import com.serenity.keystroke.events.InsertChar
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEventReducer
import com.serenity.state.undo.UndoState
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1954: a smart-punctuation substitution is its own undo step, so the first undo restores the literal characters
  * typed and keeps the rest of the typing run, as Word's AutoFormat does.
  */
class SmartPunctuationUndoSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def modelWith(text: String, cursor: CursorPosition): Model =
    val buffer = Buffer.fromString(bufferId, text).copy(editing = EditingStateFixtures(cursors = List(cursor)))
    val state  = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    val config = state.persisted.config.withSmartPunctuation(true)
    Model(state.copy(persisted = state.persisted.copy(config = config)), UndoState())

  private def typed(text: String, model: Model): Model =
    text.foldLeft(model) { (current, char) =>
      val reduced = EditorEventReducer.reduce(InsertChar(char), paneId, current.app)
      ModelCommit.applyModelEffects(current.copy(app = reduced.state), reduced.effects)
    }

  private def text(model: Model): String =
    model.app.persisted.buffers(bufferId).document.content.collect()

  "Undo after a smart-punctuation substitution" should "restore the literal characters and keep the typing run" in {
    val afterTyping = typed("so-", modelWith("", CursorPosition(0, 0)))
    val dashed      = typed("-", afterTyping)
    text(dashed) shouldBe "so–"

    val undone = UndoRecording.undone(dashed).value
    text(undone) shouldBe "so--"
  }

  it should "undo the rest of the typing run on the next undo" in {
    val dashed = typed("so--", modelWith("", CursorPosition(0, 0)))

    val undoneTwice = UndoRecording.undone(dashed).flatMap(UndoRecording.undone).value
    text(undoneTwice) shouldBe ""
  }
