package com.serenity.perf

import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{DeleteBackward, DeleteForward, Event, ExtendSelectionRight, MoveLeft, MoveRight}
import com.serenity.state.models.{AppState, CursorPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866 (PR A2): a base character and the combining marks on it are one character to the caret, a selection and a
  * deletion, whatever width the grid gives them.
  */
class CombiningMarkEditingSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val acute      = "\u0301"
  private val circumflex = "\u0302"

  private def session(text: String, column: Int) =
    val (manager, id)   = LaptopFrameBenchmarks.stateManagerHolding(text, AppConfig.conventional, cursorColumn = column)
    def state: AppState = manager.getCurrentState.unsafeRunSync()
    def apply(event: Event): Unit = manager.applyEvent(event).unsafeRunSync()
    def buffer                    = state.persisted.buffers(id)
    (apply, () => buffer.editing.cursors.head, () => buffer.document.content.getLine(0).getOrElse(""))

  "Moving the caret" should "step over a base and its mark together, both ways" in {
    val (apply, cursor, _) = session(s"e${acute}x", 0)
    apply(MoveRight)
    cursor().position shouldBe CursorPosition(0, 2)
    apply(MoveLeft)
    cursor().position shouldBe CursorPosition(0, 0)
  }

  it should "step over a base with several marks together" in {
    val (apply, cursor, _) = session(s"e$acute${circumflex}x", 0)
    apply(MoveRight)
    cursor().position shouldBe CursorPosition(0, 3)
  }

  "Extending a selection" should "take a base and its mark together" in {
    val (apply, cursor, _) = session(s"e${acute}x", 0)
    apply(ExtendSelectionRight)
    cursor().selection.map(selection => (selection.anchor, selection.focus)) shouldBe
      Some((CursorPosition(0, 0), CursorPosition(0, 2)))
  }

  "Deleting" should "remove a base and its mark together, backwards" in {
    val (apply, cursor, line) = session(s"e${acute}x", 2)
    apply(DeleteBackward)
    line() shouldBe "x"
    cursor().position shouldBe CursorPosition(0, 0)
  }

  it should "remove a base and its mark together, forwards" in {
    val (apply, _, line) = session(s"e${acute}x", 0)
    apply(DeleteForward)
    line() shouldBe "x"
  }

  it should "remove a wide character and the mark on it together" in {
    val (apply, _, line) = session(s"中${acute}x", 0)
    apply(DeleteForward)
    line() shouldBe "x"
  }
