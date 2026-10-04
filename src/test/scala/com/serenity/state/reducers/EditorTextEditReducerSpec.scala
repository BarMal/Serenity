package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.undo.HistoryEntry
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated coverage for `EditorTextEditReducer` (#1442), focused on behavior specific to this module rather than
  * re-asserting the multi-cursor/multi-selection dispatch already covered end-to-end by `EditorEventReducerSpec`:
  * `ReverseTabKey`'s per-line unindent rule (`unindentLine`), the "nothing to do" cases that must leave the undo stack
  * untouched, and the selection-takes-precedence rule the four deletion events share.
  */
class EditorTextEditReducerSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateWith(text: String, cursor: CursorPosition, selection: Option[Selection] = None): AppState =
    val buffer =
      Buffer
        .fromString(bufferId, text)
        .copy(editing = EditingStateFixtures(cursors = List(cursor), selection = selection))
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def stateWithSmartPunctuation(text: String, cursor: CursorPosition): AppState =
    val before = stateWith(text, cursor)
    before.copy(persisted = before.persisted.copy(config = before.persisted.config.withSmartPunctuation(true)))

  private def stateWithSmartPunctuationIn(language: LanguageId, text: String, cursor: CursorPosition): AppState =
    val before = stateWithSmartPunctuation(text, cursor)
    val buffer = before.persisted.buffers(bufferId)
    val tagged = buffer.copy(document = buffer.document.copy(language = Some(language)))
    before.copy(persisted = before.persisted.copy(buffers = Map(bufferId -> tagged)))

  private def textAfterTyping(typed: String, state: AppState): String =
    val after = typed.foldLeft(state)((current, c) => EditorEventReducer.reduce(InsertChar(c), paneId, current).state)
    after.persisted.buffers(bufferId).document.content.collect()

  private def stateWithMarkdown(text: String, cursor: CursorPosition): AppState =
    val before         = stateWith(text, cursor)
    val buffer         = before.persisted.buffers(bufferId)
    val markdownBuffer = buffer.copy(document = buffer.document.copy(language = Some(LanguageId.Markdown)))
    before.copy(persisted = before.persisted.copy(buffers = Map(bufferId -> markdownBuffer)))

  private def bufferAfter(event: TextEntryEvent, state: AppState): Buffer =
    EditorEventReducer.reduce(event, paneId, state).state.persisted.buffers(bufferId)

  private def recordedBoundary(event: TextEntryEvent, state: AppState): Option[UndoEffect.RecordBoundary] =
    EditorEventReducer.reduce(event, paneId, state).effects.collectFirst {
      case AppEffect.Undo(boundary: UndoEffect.RecordBoundary) => boundary
    }

  "ReverseTabKey" should "remove a literal leading tab character in preference to spaces" in {
    val before = stateWith("\tabc", CursorPosition(0, 4))

    bufferAfter(ReverseTabKey, before).document.content.collect() shouldBe "abc"
  }

  it should "remove only the spaces actually present when fewer than a full indent level's worth lead the line" in {
    val before = stateWith("  abc", CursorPosition(0, 5)) // two leading spaces, less than TabInsertion's four

    val after = bufferAfter(ReverseTabKey, before)
    after.document.content.collect() shouldBe "abc"
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 3))
  }

  it should "remove at most one full indent level's worth of leading spaces, not every leading space" in {
    val before = stateWith("        abc", CursorPosition(0, 11)) // eight leading spaces, two indent levels

    val after = bufferAfter(ReverseTabKey, before)
    after.document.content.collect() shouldBe "    abc"
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 7))
  }

  it should "leave the line and the cursor untouched when it has no leading whitespace" in {
    val before = stateWith("abc", CursorPosition(0, 2))

    val after = bufferAfter(ReverseTabKey, before)
    after.document.content.collect() shouldBe "abc"
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 2))
  }

  it should "record no undo boundary when every targeted line has nothing to unindent" in {
    val before = stateWith("abc", CursorPosition(0, 2))

    recordedBoundary(ReverseTabKey, before) shouldBe None
  }

  it should "record an undo boundary when at least one targeted line is actually unindented" in {
    val before = stateWith("    abc", CursorPosition(0, 6))

    recordedBoundary(ReverseTabKey, before) should not be None
  }

  it should "unindent only the lines a multi-line selection spans, leaving lines outside it alone" in {
    val before = stateWith(
      "    one\n    two\n    three",
      CursorPosition(1, 4),
      selection = Some(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
    )

    bufferAfter(ReverseTabKey, before).document.content.collect() shouldBe "one\ntwo\n    three"
  }

  "TabKey with a multi-line selection" should "indent every selected line by one full indent level" in {
    val before = stateWith(
      "one\ntwo\nthree",
      CursorPosition(1, 3),
      selection = Some(Selection(CursorPosition(0, 0), CursorPosition(1, 3)))
    )

    bufferAfter(TabKey, before).document.content.collect() shouldBe "    one\n    two\nthree"
  }

  "InsertChar without a selection or extra cursors" should "insert one character at the single cursor" in {
    val before = stateWith("hllo", CursorPosition(0, 1))

    val after = bufferAfter(InsertChar('e'), before)
    after.document.content.collect() shouldBe "hello"
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 2))
  }

  it should "insert a literal hyphen when smart punctuation is disabled (the default)" in {
    val before = stateWith("a", CursorPosition(0, 1))

    bufferAfter(InsertChar('-'), before).document.content.collect() shouldBe "a-"
  }

  "InsertChar with smart punctuation enabled" should "turn a second consecutive hyphen into an en dash" in {
    val before = stateWithSmartPunctuation("a-", CursorPosition(0, 2))

    val after = bufferAfter(InsertChar('-'), before)
    after.document.content.collect() shouldBe "a–"
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 2))
  }

  it should "turn three hyphens typed mid-line into a single em dash, never an em dash and a hyphen" in {
    val before = stateWithSmartPunctuation("a ", CursorPosition(0, 2))

    textAfterTyping("---", before) shouldBe "a —"
  }

  it should "leave three hyphens typed at the start of a line literal, as a thematic break or front-matter fence" in {
    val before = stateWithSmartPunctuation("title\n", CursorPosition(1, 0))

    textAfterTyping("---", before) shouldBe "title\n---"
  }

  it should "leave a Markdown table delimiter row literal" in {
    val before = stateWithSmartPunctuationIn(LanguageId.Markdown, "", CursorPosition(0, 0))

    textAfterTyping("|---|", before) shouldBe "|---|"
  }

  it should "not substitute in a code buffer" in {
    val before = stateWithSmartPunctuationIn(LanguageId.Scala, "x ", CursorPosition(0, 2))

    textAfterTyping("-- \"s\" ...", before) shouldBe "x -- \"s\" ..."
  }

  it should "substitute in Markdown prose outside code" in {
    val before = stateWithSmartPunctuationIn(LanguageId.Markdown, "a", CursorPosition(0, 1))

    textAfterTyping("--", before) shouldBe "a–"
  }

  it should "not substitute inside a Markdown code span" in {
    val before = stateWithSmartPunctuationIn(LanguageId.Markdown, "run `a", CursorPosition(0, 6))

    textAfterTyping("--\"", before) shouldBe "run `a--\""
  }

  it should "not substitute inside a fenced Markdown code block" in {
    val before = stateWithSmartPunctuationIn(LanguageId.Markdown, "```\na\n```", CursorPosition(1, 1))

    textAfterTyping("--", before) shouldBe "```\na--\n```"
  }

  it should "turn a third consecutive period into a true ellipsis" in {
    val before = stateWithSmartPunctuation("a..", CursorPosition(0, 3))

    bufferAfter(InsertChar('.'), before).document.content.collect() shouldBe "a…"
  }

  it should "open a curly double quote at the start of a line" in {
    val before = stateWithSmartPunctuation("", CursorPosition(0, 0))

    bufferAfter(InsertChar('"'), before).document.content.collect() shouldBe "“"
  }

  it should "close a curly double quote right after a word" in {
    val before = stateWithSmartPunctuation("hello", CursorPosition(0, 5))

    bufferAfter(InsertChar('"'), before).document.content.collect() shouldBe "hello”"
  }

  "NewLine in a Markdown buffer" should "leave chapter headings alone when they're already in sequence" in {
    val before = stateWithMarkdown("# Chapter 1\n\n# Chapter 2\n", CursorPosition(2, 11))

    bufferAfter(NewLine, before).document.content.collect() shouldBe "# Chapter 1\n\n# Chapter 2\n\n"
  }

  it should "resequence chapter headings left out of order by the edit" in {
    val before = stateWithMarkdown("# Chapter 5\n\n# Chapter 9\n", CursorPosition(2, 11))

    val after = bufferAfter(NewLine, before)
    after.document.content.collect() shouldBe "# Chapter 1\n\n# Chapter 2\n\n"
  }

  it should "not renumber a non-Markdown buffer even with a matching heading" in {
    val before = stateWith("# Chapter 5\n\n# Chapter 9\n", CursorPosition(2, 11))

    bufferAfter(NewLine, before).document.content.collect() shouldBe "# Chapter 5\n\n# Chapter 9\n\n"
  }

  it should "record a single undo boundary covering both the newline and the renumbering" in {
    val before = stateWithMarkdown("# Chapter 5\n\n# Chapter 9\n", CursorPosition(2, 11))

    EditorEventReducer.reduce(NewLine, paneId, before).effects.collect {
      case AppEffect.Undo(boundary: UndoEffect.RecordBoundary) => boundary
    } should have size 1
  }

  it should "keep the cursor on the newly inserted line after a renumbering edit lands elsewhere" in {
    val before = stateWithMarkdown("# Chapter 5\n\n# Chapter 9\n", CursorPosition(2, 11))

    bufferAfter(NewLine, before).editing.cursorPositions shouldBe List(CursorPosition(3, 0))
  }

  it should "record the literal keystroke with the typing run and the substitution as an undo step of its own" in {
    val before = stateWithSmartPunctuation("a-", CursorPosition(0, 2))

    val boundaries = EditorEventReducer.reduce(InsertChar('-'), paneId, before).effects.collect {
      case AppEffect.Undo(boundary: UndoEffect.RecordBoundary) => boundary
    }
    val snapshotTexts = boundaries.map(_.entry).collect {
      case edit: HistoryEntry.BufferEdit => edit.snapshot.content.collect()
    }
    boundaries.map(_.groupable) shouldBe List(true, false)
    snapshotTexts shouldBe List("a-", "a--")
  }

  /** All four deletion events share one selection arm (`deleteSelectedRanges`) ahead of their own without-a-selection
    * logic -- an active selection means "delete the selection", never "also delete a word/character on top of it".
    */
  "DeleteWordBackward with an active selection" should "delete the selection rather than a word before it" in {
    val before = stateWith(
      "alpha beta",
      CursorPosition(0, 10),
      selection = Some(Selection(CursorPosition(0, 8), CursorPosition(0, 10)))
    )

    val after = bufferAfter(DeleteWordBackward, before)
    after.document.content.collect() shouldBe "alpha be"
    after.primarySelection shouldBe None
  }

  "DeleteWordForward with an active selection" should "delete the selection rather than a word after it" in {
    val before = stateWith(
      "alpha beta",
      CursorPosition(0, 0),
      selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 2)))
    )

    val after = bufferAfter(DeleteWordForward, before)
    after.document.content.collect() shouldBe "pha beta"
    after.primarySelection shouldBe None
  }

  "DeleteWordBackward without a selection" should "leave the buffer untouched at the start of the document" in {
    val before = stateWith("alpha", CursorPosition(0, 0))

    bufferAfter(DeleteWordBackward, before).document.content.collect() shouldBe "alpha"
  }
