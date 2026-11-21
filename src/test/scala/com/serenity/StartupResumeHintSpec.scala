package com.serenity

import java.nio.file.Paths

import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The start page says how many documents a resume would bring back with text that is not on disk. */
class StartupResumeHintSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private def hintLine(unsaved: Int): Option[String] =
    StartupPageContent
      .createStartPage(sessionExists = true, resumeIdentifier = Some("notes.md"), resumeUnsaved = unsaved)
      .resume
      .map(_.line)

  "The resume line" should "name the session's file and leave out unsaved when there is none" in {
    hintLine(0) shouldBe Some("Resume last session — notes.md")
  }

  it should "say how many documents are unsaved" in {
    hintLine(1) shouldBe Some("Resume last session — notes.md (1 unsaved)")
    hintLine(2) shouldBe Some("Resume last session — notes.md (2 unsaved)")
  }

  it should "be absent when there is no session" in {
    StartupPageContent.createStartPage(sessionExists = false).resume shouldBe None
  }

  private def sessionWith(buffers: List[Buffer]): AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = buffers.map(b => b.id -> b).toMap, bufferOrder = buffers.map(_.id))
    )

  private def file(id: Int, dirty: Boolean): Buffer =
    Buffer(BufferId(id), Document(Rope("text"), filePath = Some(Paths.get(s"f$id.md")), isDirty = dirty))

  "The session's unsaved count" should "count the buffers that hold text not on disk" in {
    val untitledWithText = Buffer(BufferId(3), Document(Rope("draft"), filePath = None, isDirty = true))

    StartupPageContent.sessionUnsavedCount(sessionWith(List(file(1, dirty = true), file(2, dirty = false)))) shouldBe 1
    StartupPageContent.sessionUnsavedCount(sessionWith(List(file(1, dirty = true), untitledWithText))) shouldBe 2
    StartupPageContent.sessionUnsavedCount(sessionWith(List(file(1, dirty = false)))) shouldBe 0
  }
