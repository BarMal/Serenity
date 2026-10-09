package com.serenity.state.manager

import java.nio.file.Paths

import com.serenity.io.DocumentRevision
import com.serenity.rope.Balance
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.text.{LineEnding, LineEndingCounts}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1964: changing the line ending is an undo step, as in VS Code. */
class LineEndingUndoSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val mixed = LineEndingCounts.of("a\r\nb\r\nc\n")

  private def modelWith(document: Document => Document): (Model, BufferId) =
    val (opened, bufferId) = EditorState.createNewEmptyBuffer(AppState.initial)
    val buffer             = opened.persisted.buffers(bufferId)
    val onDisk = document(
      buffer.document.copy(filePath = Some(Paths.get("/notes/draft.txt")), isNewEmpty = false, isDirty = false)
    )
    val app = opened.copy(persisted =
      opened.persisted.copy(buffers = opened.persisted.buffers.updated(bufferId, buffer.copy(document = onDisk)))
    )
    (Model(app, UndoState()), bufferId)

  private def documentOf(model: Model, bufferId: BufferId): Document = model.app.persisted.buffers(bufferId).document

  "Changing the line ending" should "be one undo step that restores the ending and a clean buffer" in {
    val (model, bufferId) = modelWith(identity)

    val changed = LineEndingChoice.changed(model, bufferId, LineEnding.Crlf)
    documentOf(changed, bufferId).lineEnding shouldBe LineEnding.Crlf
    documentOf(changed, bufferId).isDirty shouldBe true
    changed.undo.undoStack.size shouldBe 1

    val undone = UndoRecording.undone(changed).getOrElse(fail("expected an undo step"))
    documentOf(undone, bufferId).lineEnding shouldBe LineEnding.Lf
    documentOf(undone, bufferId).isDirty shouldBe false
  }

  it should "be redone to the new ending and a dirty buffer" in {
    val (model, bufferId) = modelWith(identity)
    val undone = UndoRecording
      .undone(LineEndingChoice.changed(model, bufferId, LineEnding.Cr))
      .getOrElse(fail("expected an undo step"))

    val redone = UndoRecording.redone(undone).getOrElse(fail("expected a redo step"))

    documentOf(redone, bufferId).lineEnding shouldBe LineEnding.Cr
    documentOf(redone, bufferId).isDirty shouldBe true
  }

  it should "bring a mixed file's pending state back on undo" in {
    val (model, bufferId) = modelWith(_.copy(lineEnding = LineEnding.Crlf, mixedLineEndings = Some(mixed)))

    val undone = UndoRecording
      .undone(LineEndingChoice.changed(model, bufferId, LineEnding.Lf))
      .getOrElse(fail("expected an undo step"))

    documentOf(undone, bufferId).lineEnding shouldBe LineEnding.Crlf
    documentOf(undone, bufferId).mixedLineEndings shouldBe Some(mixed)
  }

  it should "leave a buffer dirty when undone after a save wrote the new ending" in {
    val (model, bufferId) = modelWith(_.copy(revision = Some(DocumentRevision("before"))))
    val changed           = LineEndingChoice.changed(model, bufferId, LineEnding.Crlf)
    val saved = changed.copy(app =
      changed.app.copy(persisted =
        changed.app.persisted.copy(buffers =
          changed.app.persisted.buffers.updatedWith(bufferId)(
            _.map(buffer =>
              buffer.copy(document = buffer.document.copy(isDirty = false, revision = Some(DocumentRevision("after"))))
            )
          )
        )
      )
    )

    val undone = UndoRecording.undone(saved).getOrElse(fail("expected an undo step"))

    documentOf(undone, bufferId).lineEnding shouldBe LineEnding.Lf
    documentOf(undone, bufferId).isDirty shouldBe true
  }

  it should "record nothing when the ending does not change" in {
    val (model, bufferId) = modelWith(identity)

    LineEndingChoice.changed(model, bufferId, LineEnding.Lf).undo.undoStack shouldBe Vector.empty
  }

  it should "be forgotten with its buffer" in {
    val (model, bufferId) = modelWith(identity)
    val changed           = LineEndingChoice.changed(model, bufferId, LineEnding.Crlf)

    changed.undo.retainingBuffers(_ != bufferId).undoStack shouldBe Vector.empty
  }
