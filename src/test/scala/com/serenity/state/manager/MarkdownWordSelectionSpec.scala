package com.serenity.state.manager

import com.serenity.config.{AppConfig, MarkdownViewMode}
import com.serenity.keystroke.events.MouseClick
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A double click selects what the writer sees: where the view hides Markdown markers, the word does not reach them. */
class MarkdownWordSelectionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateWith(source: String, mode: MarkdownViewMode, caret: CursorPosition): AppState =
    val base     = AppState.initial
    val buffer   = Buffer.fromString(bufferId, source).copy(editing = EditingState(List(caret)))
    val markdown = buffer.copy(document = buffer.document.copy(language = Some(LanguageId.Markdown)))
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(bufferId -> markdown),
        config = AppConfig.default.withMarkdownViewMode(mode)
      )
    )

  private def selectionAfterDoubleClick(state: AppState, at: CursorPosition): Option[Selection] =
    val buffer = state.persisted.buffers(bufferId)
    MouseTransition
      .run(state)(MouseHitTesting.editorClick(MouseClick(0, 0, clickCount = 2), Some((paneId, buffer, at))))
      ._1
      .state
      .persisted
      .buffers(bufferId)
      .primarySelection

  private def selectedIn(source: String, mode: MarkdownViewMode, word: String): Option[String] =
    val column = source.indexOf(word) + 1
    val state  = stateWith(source, mode, CursorPosition(0, column))
    selectionAfterDoubleClick(state, CursorPosition(0, column)).map { selection =>
      source.substring(selection.anchor.column, selection.focus.column)
    }

  "A double click in read mode" should "select only the word inside bold markers" in {
    selectedIn("some **bold** text", MarkdownViewMode.Read, "bold") shouldBe Some("bold")
  }

  it should "select only the word inside italic markers" in {
    selectedIn("some *em* text", MarkdownViewMode.Read, "em") shouldBe Some("em")
  }

  it should "select only the word inside code backticks" in {
    selectedIn("some `code` text", MarkdownViewMode.Read, "code") shouldBe Some("code")
  }

  it should "select only the word inside strikethrough markers" in {
    selectedIn("some ~~strike~~ text", MarkdownViewMode.Read, "strike") shouldBe Some("strike")
  }

  it should "select the offsets of the inner word" in {
    val state = stateWith("some **bold** text", MarkdownViewMode.Read, CursorPosition(0, 0))

    selectionAfterDoubleClick(state, CursorPosition(0, 8)) shouldBe
      Some(Selection(CursorPosition(0, 7), CursorPosition(0, 11)))
  }

  it should "reach across hidden markers to visible punctuation that follows the word" in {
    selectedIn("see **bold**, now", MarkdownViewMode.Read, "bold") shouldBe Some("bold**,")
  }

  it should "select an unmarked word as before" in {
    selectedIn("some **bold** text", MarkdownViewMode.Read, "text") shouldBe Some("text")
  }

  "A double click in source mode" should "keep the trailing markers in the word" in {
    selectedIn("some **bold** text", MarkdownViewMode.Source, "bold") shouldBe Some("bold**")
  }

  "A double click in live preview" should "keep the trailing markers on the line the caret is on" in {
    selectedIn("some **bold** text", MarkdownViewMode.LivePreview, "bold") shouldBe Some("bold**")
  }

  it should "select only the word on a line whose markers are hidden" in {
    val state = stateWith("plain\nsome **bold** text", MarkdownViewMode.LivePreview, CursorPosition(0, 0))

    selectionAfterDoubleClick(state, CursorPosition(1, 8)) shouldBe
      Some(Selection(CursorPosition(1, 7), CursorPosition(1, 11)))
  }
