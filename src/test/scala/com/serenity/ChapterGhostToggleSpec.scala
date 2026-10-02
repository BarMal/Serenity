package com.serenity

import com.serenity.command.{CommandIntent, CommandRegistry, ViewIntent}
import com.serenity.rope.Balance
import com.serenity.state.manager.{ChapterNoteTransitions, DamageProducer}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Chapter ghosts are a view over the notes, so hiding them must never touch the notes themselves -- it only changes
  * whether the faded text is painted.
  */
class ChapterGhostToggleSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Chapter ghosts" should "be visible by default" in {
    AppState.initial.runtime.chapterGhostsVisible shouldBe true
  }

  it should "toggle off and back on" in {
    val off = ChapterNoteTransitions.toggleGhosts(AppState.initial)

    off.runtime.chapterGhostsVisible shouldBe false
    ChapterNoteTransitions.toggleGhosts(off).runtime.chapterGhostsVisible shouldBe true
  }

  it should "redraw every pane when toggled, since each may hold a ghost" in {
    val before = AppState.initial
    val after  = ChapterNoteTransitions.toggleGhosts(before)

    Damage.isEverything(DamageProducer.forTransition(before, after)) shouldBe true
  }

  it should "leave the notes alone when hidden" in {
    val initial = AppState.initial
    val notes   = Map[NoteKey, Notes](NoteKey.Keyword("Elizabeth") -> Notes(BufferId(5)))
    val buffer  = initial.persisted.buffers(BufferId(0))
    val withNotes = initial.copy(persisted =
      initial.persisted.copy(buffers =
        initial.persisted.buffers.updated(BufferId(0), buffer.copy(annotations = Annotations(notes = notes)))
      )
    )

    ChapterNoteTransitions.toggleGhosts(withNotes).persisted.buffers shouldBe withNotes.persisted.buffers
  }

  it should "be toggled by a command" in {
    CommandRegistry.withToggleUI.findCommand("toggle-chapter-ghosts").map(_.intent) shouldBe
      Some(CommandIntent.View(ViewIntent.ToggleChapterGhosts))
  }
