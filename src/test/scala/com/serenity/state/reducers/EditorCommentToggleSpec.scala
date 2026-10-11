package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Toggling line and block comments from the language's own comment tokens: the comment goes at the block's shallowest
  * indent, blank lines are left alone, the selection survives, and the whole toggle is one undo step.
  */
class EditorCommentToggleSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateOf(
    text: String,
    cursors: List[CursorPosition],
    selection: Option[Selection] = None,
    language: Option[LanguageId] = Some(LanguageId.Scala)
  ): AppState =
    val buffer = Buffer.fromString(bufferId, text)
    val seeded = buffer.copy(
      document = buffer.document.copy(language = language),
      editing = EditingStateFixtures(cursors = cursors, selection = selection)
    )
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> seeded)))

  private def caretAt(text: String, line: Int, column: Int, language: Option[LanguageId] = Some(LanguageId.Scala)) =
    stateOf(text, List(CursorPosition(line, column)), language = language)

  private def selecting(
    text: String,
    from: (Int, Int),
    to: (Int, Int),
    language: Option[LanguageId] = Some(LanguageId.Scala)
  ): AppState =
    val anchor = CursorPosition(from._1, from._2)
    val focus  = CursorPosition(to._1, to._2)
    stateOf(text, List(focus), Some(Selection(anchor, focus)), language)

  private def reduce(event: TextEntryEvent, state: AppState): ReducerResult =
    EditorEventReducer.reduce(event, paneId, state)

  private def bufferOf(result: ReducerResult): Buffer = result.state.persisted.buffers(bufferId)

  private def textOf(result: ReducerResult): String = bufferOf(result).document.content.collect()

  private def selectionOf(result: ReducerResult): Option[Selection] = bufferOf(result).primarySelection

  private def undoEntries(result: ReducerResult): Int =
    result.effects.count {
      case AppEffect.Undo(_: UndoEffect.RecordBoundary) => true
      case _                                            => false
    }

  private def shouldLeaveUntouched(result: ReducerResult, text: String): Unit =
    textOf(result) shouldBe text
    bufferOf(result).document.isDirty shouldBe false
    undoEntries(result) shouldBe 0

  "ToggleLineComment" should "comment the cursor's line and keep the caret on the same text" in {
    val result = reduce(ToggleLineComment, caretAt("val a = 1", 0, 4))

    textOf(result) shouldBe "// val a = 1"
    bufferOf(result).editing.cursorPositions shouldBe List(CursorPosition(0, 7))
    bufferOf(result).document.isDirty shouldBe true
  }

  it should "uncomment a commented line, taking the space after the token with it" in {
    val result = reduce(ToggleLineComment, caretAt("// val a = 1", 0, 7))

    textOf(result) shouldBe "val a = 1"
    bufferOf(result).editing.cursorPositions shouldBe List(CursorPosition(0, 4))
  }

  it should "uncomment a token that has no space after it" in {
    textOf(reduce(ToggleLineComment, caretAt("//val a = 1", 0, 0))) shouldBe "val a = 1"
  }

  it should "put the token after the line's indentation" in {
    textOf(reduce(ToggleLineComment, caretAt("    val a = 1", 0, 8))) shouldBe "    // val a = 1"
    textOf(reduce(ToggleLineComment, caretAt("    // val a = 1", 0, 11))) shouldBe "    val a = 1"
  }

  it should "comment every selected line at the shallowest indent and leave blank lines alone" in {
    val result = reduce(ToggleLineComment, selecting("    a\n\n  b", from = (0, 0), to = (2, 3)))

    textOf(result) shouldBe "  //   a\n\n  // b"
    undoEntries(result) shouldBe 1
  }

  it should "uncomment every selected line when all non-blank lines are commented" in {
    val result = reduce(ToggleLineComment, selecting("  // a\n\n  // b", from = (0, 0), to = (2, 6)))

    textOf(result) shouldBe "  a\n\n  b"
  }

  it should "comment all lines, even the commented ones, when only some are commented" in {
    val result = reduce(ToggleLineComment, selecting("// a\nb", from = (0, 0), to = (1, 1)))

    textOf(result) shouldBe "// // a\n// b"
  }

  it should "leave out the last line when the selection ends at its very start" in {
    val result = reduce(ToggleLineComment, selecting("a\nb\nc", from = (0, 0), to = (2, 0)))

    textOf(result) shouldBe "// a\n// b\nc"
  }

  it should "keep the selection over the same lines so a second toggle undoes the first" in {
    val original = selecting("a\n  b\nc", from = (0, 0), to = (2, 1))
    val once     = reduce(ToggleLineComment, original)

    textOf(once) shouldBe "// a\n//   b\n// c"
    selectionOf(once) shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(2, 4)))

    val twice = reduce(ToggleLineComment, once.state)
    textOf(twice) shouldBe "a\n  b\nc"
    selectionOf(twice) shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(2, 1)))
  }

  it should "comment a blank line where the caret is" in {
    val result = reduce(ToggleLineComment, caretAt("a\n\nb", 1, 0))

    textOf(result) shouldBe "a\n// \nb"
    bufferOf(result).editing.cursorPositions shouldBe List(CursorPosition(1, 3))
  }

  it should "toggle each line holding a cursor once" in {
    val state  = stateOf("one\ntwo\nthree", List(CursorPosition(0, 1), CursorPosition(2, 0), CursorPosition(2, 4)))
    val result = reduce(ToggleLineComment, state)

    textOf(result) shouldBe "// one\ntwo\n// three"
    undoEntries(result) shouldBe 1
  }

  it should "use the language's own token" in {
    textOf(reduce(ToggleLineComment, caretAt("x = 1", 0, 0, Some(LanguageId.Python)))) shouldBe "# x = 1"
    textOf(reduce(ToggleLineComment, caretAt("x = 1", 0, 0, Some(LanguageId.Yaml)))) shouldBe "# x = 1"
    textOf(reduce(ToggleLineComment, caretAt("f x = x", 0, 0, Some(LanguageId.Haskell)))) shouldBe "-- f x = x"
    textOf(reduce(ToggleLineComment, caretAt("select 1", 0, 0, Some(LanguageId.Sql)))) shouldBe "-- select 1"
  }

  it should "wrap each line in the block delimiters when the language has no line comment" in {
    val markdown = reduce(ToggleLineComment, caretAt("some prose", 0, 0, Some(LanguageId.Markdown)))
    textOf(markdown) shouldBe "<!-- some prose -->"
    textOf(reduce(ToggleLineComment, markdown.state)) shouldBe "some prose"

    textOf(reduce(ToggleLineComment, caretAt("a { b: c }", 0, 0, Some(LanguageId.Css)))) shouldBe "/* a { b: c } */"
  }

  it should "change nothing without a language or for a language with no comments" in {
    shouldLeaveUntouched(reduce(ToggleLineComment, caretAt("plain", 0, 0, language = None)), "plain")
    shouldLeaveUntouched(reduce(ToggleLineComment, caretAt("{}", 0, 0, Some(LanguageId.JsonLang))), "{}")
  }

  "ToggleBlockComment" should "wrap a selection within a line and keep the selected text selected" in {
    val result = reduce(ToggleBlockComment, selecting("a b c", from = (0, 2), to = (0, 3)))

    textOf(result) shouldBe "a /* b */ c"
    selectionOf(result) shouldBe Some(Selection(CursorPosition(0, 5), CursorPosition(0, 6)))
    undoEntries(result) shouldBe 1
  }

  it should "unwrap a selection that covers the whole comment" in {
    val result = reduce(ToggleBlockComment, selecting("a /* b */ c", from = (0, 2), to = (0, 9)))

    textOf(result) shouldBe "a b c"
  }

  it should "unwrap a selection sitting just inside the delimiters" in {
    val result = reduce(ToggleBlockComment, selecting("a /* b */ c", from = (0, 5), to = (0, 6)))

    textOf(result) shouldBe "a b c"
    selectionOf(result) shouldBe Some(Selection(CursorPosition(0, 2), CursorPosition(0, 3)))
  }

  it should "wrap a selection across lines in a single comment" in {
    val result = reduce(ToggleBlockComment, selecting("x\ny\nz", from = (0, 0), to = (2, 1)))

    textOf(result) shouldBe "/* x\ny\nz */"
  }

  it should "wrap the text of the cursor's line, after its indentation, when nothing is selected" in {
    val wrapped = reduce(ToggleBlockComment, caretAt("  foo()  ", 0, 4))

    textOf(wrapped) shouldBe "  /* foo() */  "
    textOf(reduce(ToggleBlockComment, caretAt("  /* foo() */  ", 0, 6))) shouldBe "  foo()  "
  }

  it should "use the language's block delimiters" in {
    textOf(reduce(ToggleBlockComment, caretAt("<p/>", 0, 0, Some(LanguageId.Html)))) shouldBe "<!-- <p/> -->"
    textOf(reduce(ToggleBlockComment, caretAt("f x", 0, 0, Some(LanguageId.Haskell)))) shouldBe "{- f x -}"
    textOf(reduce(ToggleBlockComment, caretAt("f()", 0, 0, Some(LanguageId.Lua)))) shouldBe "--[[ f() ]]"
  }

  it should "change nothing in a language without block comments, or on a blank line" in {
    shouldLeaveUntouched(reduce(ToggleBlockComment, caretAt("x = 1", 0, 0, Some(LanguageId.Python))), "x = 1")
    shouldLeaveUntouched(reduce(ToggleBlockComment, caretAt("   ", 0, 1)), "   ")
    shouldLeaveUntouched(reduce(ToggleBlockComment, caretAt("plain", 0, 0, language = None)), "plain")
  }
end EditorCommentToggleSpec
