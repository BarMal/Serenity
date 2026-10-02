package com.serenity

import com.serenity.command.CommandRegistry
import com.serenity.config.{HotkeyAction, HotkeyConfig}
import com.serenity.keystroke.events.{OpenChapterNote, ToggleNotesPin}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.AppEventReducer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Ctrl+G is Go to Line, so opening a chapter's note and pinning the notes pane sit on the shifted letters. */
class ChapterNoteHotkeySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val platforms = List("Linux", "Windows", "Mac OS X")

  private def modifierFor(osName: String): String = if osName == "Mac OS X" then "meta" else "ctrl"

  private def rendered(osName: String, action: HotkeyAction): List[String] =
    HotkeyConfig.defaultBindingsFor(osName)(action).map(_.render)

  private val manuscript: AppState =
    val initial = AppState.initial
    val buffer  = initial.persisted.buffers(BufferId(0))
    initial.copy(persisted =
      initial.persisted.copy(buffers =
        initial.persisted.buffers.updated(
          BufferId(0),
          buffer.copy(document =
            buffer.document.copy(content = Rope("# Chapter 1: Storm\nthe sea"), language = Some(LanguageId.Markdown))
          )
        )
      )
    )

  "Open Chapter Note" should "be bound to Ctrl+Shift+N (Cmd+Shift+N on macOS)" in
    platforms.foreach(os => rendered(os, HotkeyAction.OpenChapterNote) shouldBe List(s"${modifierFor(os)}+shift+n"))

  it should "open the note for the chapter under the cursor when its event is reduced" in {
    val reduced = AppEventReducer.reduce(OpenChapterNote, manuscript, CommandRegistry.withToggleUI).state

    reduced.runtime.notesPane should not be empty
  }

  "Pin/Unpin Notes" should "be bound to Ctrl+Shift+L (Cmd+Shift+L on macOS)" in
    platforms.foreach(os => rendered(os, HotkeyAction.ToggleNotesPin) shouldBe List(s"${modifierFor(os)}+shift+l"))

  it should "pin a notes pane that is following when its event is reduced" in {
    val opened = AppEventReducer.reduce(OpenChapterNote, manuscript, CommandRegistry.withToggleUI).state
    val pinned = AppEventReducer.reduce(ToggleNotesPin, opened, CommandRegistry.withToggleUI).state

    pinned.runtime.notesPane.map(_.pinned) shouldBe Some(true)
  }

  "The chapter-note keys" should "collide with nothing and leave Go to Line alone" in
    platforms.foreach { os =>
      val bindings = HotkeyConfig.defaultBindingsFor(os)

      bindings(HotkeyAction.GoToLine).map(_.render) shouldBe List(s"${modifierFor(os)}+g")
      HotkeyConfig.validate(bindings) shouldBe Right(())
    }
