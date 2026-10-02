package com.serenity

import com.serenity.rope.Balance
import com.serenity.session.SessionBuffer
import com.serenity.session.given
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Notes are keyed by what they describe, stored on the buffer they annotate, and point at hidden buffers that hold
  * their text.
  */
class NotesStateSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val chapterKey = NoteKey.Chapter(HeadingIdentity("the storm", 0))
  private val keywordKey = NoteKey.Keyword("Elizabeth")

  private val notes = Map(
    chapterKey -> Notes(BufferId(5), Some(BufferId(6))),
    keywordKey -> Notes(BufferId(7))
  )

  private def withNotes(buffer: Buffer, entries: Map[NoteKey, Notes]): Buffer =
    buffer.copy(annotations = buffer.annotations.copy(notes = entries))

  "Notes.bufferIds" should "list the overview first, then the extra notes when there are any" in {
    Notes(BufferId(1), Some(BufferId(2))).bufferIds shouldBe List(BufferId(1), BufferId(2))
    Notes(BufferId(1)).bufferIds shouldBe List(BufferId(1))
  }

  "A buffer's notes" should "round-trip through the session" in {
    val buffer   = withNotes(Buffer.fromString(BufferId(0), "# The Storm"), notes)
    val restored = SessionBuffer.toBuffer(SessionBuffer.fromBuffer(buffer))

    restored.annotations.notes shouldBe notes
  }

  it should "be written in a stable order, chapters before keywords" in {
    val reversed = Map(keywordKey -> Notes(BufferId(7)), chapterKey -> Notes(BufferId(5)))
    val buffer   = withNotes(Buffer.fromString(BufferId(0), "# The Storm"), reversed)

    SessionBuffer.fromBuffer(buffer).notes.map(_.kind) shouldBe List("chapter", "keyword")
  }

  "A session buffer written before notes existed" should "decode with no notes" in {
    val decoded = _root_.io.circe.parser
      .parse(
        """{"id":1,"filePath":null,"isDirty":false,"language":null,"isNewEmpty":false,
          |"cursors":[{"line":0,"column":0}],
          |"viewport":{"leftColumn":0,"topLine":0,"visibleColumns":80,"visibleLines":24}}""".stripMargin
      )
      .flatMap(_.as[SessionBuffer])

    decoded.map(_.notes) shouldBe Right(Nil)
  }

  private def stateNotingBuffer(noteBuffer: BufferId, others: List[Buffer]): AppState =
    val initial    = AppState.initial
    val manuscript = withNotes(initial.persisted.buffers(BufferId(0)), Map(chapterKey -> Notes(noteBuffer)))
    initial.copy(persisted =
      initial.persisted.copy(buffers =
        initial.persisted.buffers.updated(BufferId(0), manuscript) ++ others.map(buffer => buffer.id -> buffer)
      )
    )

  private def noteErrors(state: AppState): List[String] =
    AppStateValidation.validationErrors(state).filter(_.contains("note"))

  "State validation" should "accept a note whose buffer is hidden" in {
    val hidden = Buffer.fromString(BufferId(5), "outline").copy(hidden = true)

    noteErrors(stateNotingBuffer(BufferId(5), List(hidden))) shouldBe Nil
  }

  it should "reject a note that points at a buffer that does not exist" in {
    noteErrors(stateNotingBuffer(BufferId(9), Nil)) should not be empty
  }

  it should "reject a note that points at a buffer the user can see as a document" in {
    val visible = Buffer.fromString(BufferId(5), "outline")

    noteErrors(stateNotingBuffer(BufferId(5), List(visible))) should not be empty
  }
