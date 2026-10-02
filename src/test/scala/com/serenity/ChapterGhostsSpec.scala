package com.serenity

import com.serenity.document.ChapterGhosts
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The ghost is a note's overview shown, faded, on the blank lines under a chapter that has no prose yet. It is only
  * ever computed from the manuscript and its notes: nothing is written into the document.
  */
class ChapterGhostsSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val manuscriptId = BufferId(0)
  private val noteId       = BufferId(5)
  private val storm        = NoteKey.Chapter(HeadingIdentity("storm", 0))

  private def manuscript(text: String, notes: Map[NoteKey, Notes]): Buffer =
    val base = Buffer.fromString(manuscriptId, text)
    base.copy(
      document = base.document.copy(language = Some(LanguageId.Markdown)),
      annotations = Annotations(notes = notes)
    )

  private def noteBuffer(text: String): Buffer = Buffer.fromString(noteId, text).copy(hidden = true)

  private def ghosts(text: String, noteText: String, notes: Map[NoteKey, Notes] = Map(storm -> Notes(noteId))) =
    ChapterGhosts.byLine(manuscript(text, notes), Map(noteId -> noteBuffer(noteText)))

  "A chapter's ghost" should "put the overview on the blank lines under an empty chapter" in {
    ghosts("# Chapter 1: Storm\n\n\n# Chapter 2: Calm\nthe shore", "Gale hits\nLiz leaves") shouldBe
      Map(1 -> "Gale hits", 2 -> "Liz leaves")
  }

  it should "show only as many overview lines as there are blank lines" in {
    ghosts("# Chapter 1: Storm\n\n# Chapter 2: Calm\nthe shore", "Gale hits\nLiz leaves") shouldBe
      Map(1 -> "Gale hits")
  }

  it should "treat a line of only spaces as blank" in {
    ghosts("# Chapter 1: Storm\n   \n# Chapter 2: Calm\nthe shore", "Gale hits") shouldBe Map(1 -> "Gale hits")
  }

  it should "show under an empty last chapter" in {
    ghosts("# Chapter 1: Storm\n\n", "Gale hits").get(1) shouldBe Some("Gale hits")
  }

  it should "disappear once the chapter has prose" in {
    ghosts("# Chapter 1: Storm\n\nThe sea rose.\n\n# Chapter 2: Calm", "Gale hits") shouldBe Map.empty
  }

  it should "disappear when prose is below the blank lines, not only directly under the heading" in {
    ghosts("# Chapter 1: Storm\n\n\n\nThe sea rose.", "Gale hits") shouldBe Map.empty
  }

  it should "have no room when the heading is followed directly by another heading" in {
    ghosts("# Chapter 1: Storm\n# Chapter 2: Calm\nthe shore", "Gale hits") shouldBe Map.empty
  }

  it should "keep a blank line inside a multi-line overview but trim its ends" in {
    ghosts("# Chapter 1: Storm\n\n\n\n# Chapter 2: Calm", "\nGale hits\n\nLiz leaves\n\n") shouldBe
      Map(1 -> "Gale hits", 2 -> "", 3 -> "Liz leaves")
  }

  it should "show nothing for an empty overview" in {
    ghosts("# Chapter 1: Storm\n\n\n# Chapter 2: Calm", "  \n\n") shouldBe Map.empty
  }

  it should "show nothing when the note's buffer is missing" in {
    ChapterGhosts.byLine(
      manuscript("# Chapter 1: Storm\n\n# Chapter 2: Calm", Map(storm -> Notes(noteId))),
      Map.empty
    ) shouldBe Map.empty
  }

  it should "show nothing for a note whose chapter has been retitled or deleted" in {
    ghosts("# Chapter 1: Gale\n\n# Chapter 2: Calm", "Gale hits") shouldBe Map.empty
  }

  it should "ignore keyword notes" in {
    ghosts(
      "# Chapter 1: Storm\n\n\n# Chapter 2: Calm",
      "Gale hits",
      Map(NoteKey.Keyword("Elizabeth") -> Notes(noteId))
    ) shouldBe Map.empty
  }

  it should "give each empty chapter its own overview" in {
    val calm  = NoteKey.Chapter(HeadingIdentity("calm", 0))
    val notes = Map[NoteKey, Notes](storm -> Notes(noteId), calm -> Notes(BufferId(6)))
    val text  = "# Chapter 1: Storm\n\n# Chapter 2: Calm\n\n"

    ChapterGhosts.byLine(
      manuscript(text, notes),
      Map(noteId -> noteBuffer("Gale hits"), BufferId(6) -> noteBuffer("Still water").copy(id = BufferId(6)))
    ) shouldBe Map(1 -> "Gale hits", 3 -> "Still water")
  }

  it should "never appear in a hidden buffer" in {
    val note = manuscript("# Chapter 1: Storm\n\n\n# Chapter 2: Calm", Map(storm -> Notes(noteId))).copy(hidden = true)

    ChapterGhosts.byLine(note, Map(noteId -> noteBuffer("Gale hits"))) shouldBe Map.empty
  }

  it should "show nothing when there are no notes" in {
    ChapterGhosts.byLine(manuscript("# Chapter 1: Storm\n\n\n", Map.empty), Map.empty) shouldBe Map.empty
  }
