package com.serenity.state.undo

import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Undo and redo swap whole content back in without replaying the edit, so annotations have to be carried across the
  * difference or they stay where the intervening edit left them -- typing a character and undoing it must not leave a
  * placeholder one column off.
  */
class BufferSnapshotAnnotationTrackingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def buffer(text: String, annotations: Annotations): Buffer =
    Buffer.fromString(BufferId(0), text).copy(annotations = annotations)

  private def placeholderAt(line: Int, column: Int): Annotations =
    Annotations(placeholders = List(Placeholder(CursorPosition(line, column), "n")))

  "BufferSnapshot.restoreInto" should "move a placeholder back when an insertion before it is undone" in {
    val snapshot = BufferSnapshot.fromBuffer(buffer("abc def", placeholderAt(0, 4)))
    val edited   = buffer("Xabc def", placeholderAt(0, 5))

    snapshot.restoreInto(edited).annotations shouldBe placeholderAt(0, 4)
  }

  it should "move a placeholder forward again when the undone insertion is redone" in {
    val snapshot = BufferSnapshot.fromBuffer(buffer("Xabc def", placeholderAt(0, 5)))
    val undone   = buffer("abc def", placeholderAt(0, 4))

    snapshot.restoreInto(undone).annotations shouldBe placeholderAt(0, 5)
  }

  it should "move a placeholder back when a deletion before it is undone" in {
    val snapshot = BufferSnapshot.fromBuffer(buffer("abc def", placeholderAt(0, 4)))
    val edited   = buffer("bc def", placeholderAt(0, 3))

    snapshot.restoreInto(edited).annotations shouldBe placeholderAt(0, 4)
  }

  it should "move a placeholder down again when an undone newline is restored" in {
    val snapshot = BufferSnapshot.fromBuffer(buffer("\nabc def", placeholderAt(1, 4)))
    val undone   = buffer("abc def", placeholderAt(0, 4))

    snapshot.restoreInto(undone).annotations shouldBe placeholderAt(1, 4)
  }

  it should "move a comment range back when an insertion before it is undone" in {
    val comment  = DocumentComment(CursorPosition(0, 4), CursorPosition(0, 7), "note")
    val snapshot = BufferSnapshot.fromBuffer(buffer("abc def", Annotations(documentComments = List(comment))))
    val shifted  = DocumentComment(CursorPosition(0, 5), CursorPosition(0, 8), "note")
    val edited   = buffer("Xabc def", Annotations(documentComments = List(shifted)))

    snapshot.restoreInto(edited).annotations.documentComments shouldBe List(comment)
  }

  it should "leave annotations alone when the restored content is identical" in {
    val annotations = placeholderAt(0, 4)
    val snapshot    = BufferSnapshot.fromBuffer(buffer("abc def", annotations))

    snapshot.restoreInto(buffer("abc def", annotations)).annotations shouldBe annotations
  }
