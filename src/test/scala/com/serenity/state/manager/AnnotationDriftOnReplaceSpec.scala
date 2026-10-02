package com.serenity.state.manager

import java.nio.file.Path

import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** When a buffer's whole text is swapped -- reloaded from disk, or replaced by a formatter or file watcher -- its
  * annotations must keep their place relative to the text around the change, and none may be left pointing past the end
  * of a document that has shrunk.
  */
class AnnotationDriftOnReplaceSpec extends AnnotationDriftFixtures:

  "Reloading a buffer from disk" should "carry bookmarks, placeholders and comments across text inserted above them" in {
    val state = stateWith("abc def\nsecond")

    val reloaded = reload(state, diskBuffer("NEW\nabc def\nsecond"))

    annotationsOf(reloaded) shouldBe Annotations(
      bookmarks = List(CursorPosition(2, 0)),
      documentComments = List(DocumentComment(CursorPosition(1, 4), CursorPosition(1, 7), "note")),
      placeholders = List(Placeholder(CursorPosition(2, 2), "n"))
    )
  }

  it should "pull annotations back inside a file that has shrunk" in {
    val state = stateWith("abc def\nsecond")

    val reloaded = reload(state, diskBuffer("abc"))

    annotationsOf(reloaded).placeholders shouldBe List(Placeholder(CursorPosition(0, 3), "n"))
    annotationsOf(reloaded).bookmarks shouldBe List(CursorPosition(0, 3))
  }

  "Replacing a buffer's content in bulk" should "carry annotations across text inserted above them" in {
    val state = stateWith("abc def\nsecond")

    val replaced = EditorTransitions.bufferContentReplaced(state, bufferId, "NEW\nabc def\nsecond")

    replaced.map(_.buffer.annotations) shouldBe Some(
      Annotations(
        bookmarks = List(CursorPosition(2, 0)),
        documentComments = List(DocumentComment(CursorPosition(1, 4), CursorPosition(1, 7), "note")),
        placeholders = List(Placeholder(CursorPosition(2, 2), "n"))
      )
    )
  }

  it should "leave annotations alone when the text is unchanged" in {
    val state = stateWith("abc def\nsecond")

    EditorTransitions.bufferContentReplaced(state, bufferId, "abc def\nsecond").map(_.buffer.annotations) shouldBe
      Some(state.persisted.buffers(bufferId).annotations)
  }

  "Clamping a buffer to its content" should "drop a placeholder that is past the end" in {
    val buffer = Buffer
      .fromString(bufferId, "one line")
      .copy(annotations = Annotations(placeholders = List(Placeholder(CursorPosition(5, 0), "gone"))))

    buffer.clampedToContent.annotations.placeholders shouldBe Nil
  }

  it should "keep a placeholder that is still inside the content" in {
    val kept   = Placeholder(CursorPosition(0, 2), "kept")
    val buffer = Buffer.fromString(bufferId, "one line").copy(annotations = Annotations(placeholders = List(kept)))

    buffer.clampedToContent.annotations.placeholders shouldBe List(kept)
  }

/** Shared setup: a file-backed buffer carrying one of each positional annotation. */
abstract class AnnotationDriftFixtures extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  protected val bufferId: BufferId = BufferId(0)
  protected val path: Path         = Path.of("/workspace/manuscript.md")

  protected def diskDocument(text: String): Document =
    Buffer.fromString(bufferId, text).document.copy(filePath = Some(path))

  protected def diskBuffer(text: String): Buffer =
    Buffer.fromString(bufferId, text).copy(document = diskDocument(text))

  protected def reload(state: AppState, disk: Buffer): AppState =
    FileResults.reloaded(state, bufferId, path, state.persisted.buffers(bufferId).document.content, disk)

  protected def stateWith(text: String): AppState =
    val initial = AppState.initial
    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(
        document = diskDocument(text),
        annotations = Annotations(
          bookmarks = List(CursorPosition(1, 0)),
          documentComments = List(DocumentComment(CursorPosition(0, 4), CursorPosition(0, 7), "note")),
          placeholders = List(Placeholder(CursorPosition(1, 2), "n"))
        )
      )
    initial.copy(persisted = initial.persisted.copy(buffers = initial.persisted.buffers.updated(bufferId, buffer)))

  protected def annotationsOf(state: AppState): Annotations = state.persisted.buffers(bufferId).annotations
