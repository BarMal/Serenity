package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.keystroke.translators.SingleLineFormTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalEventReducer
import com.serenity.ui.widget.TextField
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Find's search contract (#1957): options, the result cap, caret-relative windows, navigation that never lands on text
  * an edit has changed, painted match highlights and the refresh an open find needs after an edit.
  */
class FindSearchSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val findId   = SurfaceId("find")

  private def search(text: String, query: String, options: FindOptions = FindOptions.default, anchor: Int = 0) =
    FindSearch.search(Rope(text), query, options, anchor)

  private def stateWith(text: String, caret: CursorPosition = CursorPosition(0, 0), find: Option[Modal.Find] = None) =
    val buffer = AppState.initial.persisted.buffers(bufferId)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(buffers =
        AppState.initial.persisted.buffers.updated(
          bufferId,
          buffer.copy(document = buffer.document.copy(content = Rope(text)), editing = EditingState(List(caret)))
        )
      ),
      runtime = AppState.initial.runtime.copy(uiSurfaces = find.toList.map { modal =>
        UiSurface(
          findId,
          SurfaceContent.ModalWorkflow(modal),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      })
    )

  private def withFindState(state: AppState, findState: FindState): AppState =
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(bufferId, state.persisted.buffers(bufferId).copy(findState = Some(findState)))
      )
    )

  "FindSearch" should "match case-insensitively by default and exactly when match case is on" in {
    search("Needle needle NEEDLE", "needle").results shouldBe Vector(
      FindResult(0, 0),
      FindResult(0, 7),
      FindResult(0, 14)
    )
    search("Needle needle NEEDLE", "needle", FindOptions(matchCase = true)).results shouldBe Vector(FindResult(0, 7))
  }

  it should "match only UAX #29 whole words when whole word is on" in {
    val text = "cat concatenate cat's cats cat."

    search(text, "cat", FindOptions(wholeWord = true)).results shouldBe Vector(FindResult(0, 0), FindResult(0, 27))
  }

  it should "match a regular expression, with ^ anchoring at each line start" in {
    val text = "needle one\nneeedle two\nthe needle"

    search(text, "^ne+dle", FindOptions(regex = true)).results shouldBe Vector(FindResult(0, 0), FindResult(1, 0))
    search(text, "ne+dle", FindOptions()).results shouldBe Vector.empty
  }

  it should "report an invalid regex as an error rather than throwing, and find nothing for it" in {
    FindPattern.compile("ne(dle", FindOptions(regex = true)).left.toOption.map(_.message) shouldBe defined
    FindPattern.compile("ne(dle", FindOptions()).isRight shouldBe true
    search("ne(dle needle", "ne(dle", FindOptions(regex = true)) shouldBe FindMatches.empty
  }

  it should "cap the matches it keeps and say there are more" in {
    val matches = search("x " * (FindSearch.MatchLimit + 500), "x")

    matches.results.length shouldBe FindSearch.MatchLimit
    matches.capped shouldBe true
    FindResultSet.normalized("x", matches.results, 0, matches.capped).matchCountLabel shouldBe "1000+ matches"
  }

  it should "keep the capped window nearest the anchor so the first match at or after the caret is in it" in {
    val lines   = (0 until 3000).map(line => s"needle $line").mkString("\n")
    val anchor  = Rope(lines).lineColumnToOffset(2500, 0)
    val matches = search(lines, "needle", anchor = anchor)

    matches.capped shouldBe true
    matches.results.length shouldBe FindSearch.MatchLimit
    matches.results should contain(FindResult(2500, 0))
    matches.results shouldBe matches.results.sorted
  }

  it should "wrap past the end of the document when the window starts below the last match" in {
    search("needle\nmiddle\nneedle", "needle", anchor = 10).results shouldBe Vector(FindResult(0, 0), FindResult(2, 0))
  }

  "FindNavigation.step" should "go to the first match after a mid-document caret, not the first in the document" in {
    val content = Rope("needle one\nmiddle\nneedle two\nneedle three")
    val stored  = FindState("needle", Vector(FindResult(0, 0), FindResult(2, 0), FindResult(3, 0)), 0)

    val next = FindNavigation.step(content, stored, CursorPosition(1, 2), FindDirection.Forward)

    next.flatMap(_.resultSet.selectedResult) shouldBe Some(FindResult(2, 0))
  }

  it should "step back to the previous match and wrap to the last one" in {
    val content = Rope("needle one\nmiddle\nneedle two\nneedle three")
    val stored  = FindState("needle", Vector(FindResult(0, 0), FindResult(2, 0), FindResult(3, 0)), 1)

    FindNavigation
      .step(content, stored, CursorPosition(2, 0), FindDirection.Backward)
      .flatMap(_.resultSet.selectedResult) shouldBe Some(FindResult(0, 0))
    FindNavigation
      .step(content, stored.copy(currentIndex = 0), CursorPosition(0, 0), FindDirection.Backward)
      .flatMap(_.resultSet.selectedResult) shouldBe Some(FindResult(3, 0))
  }

  it should "never land on text an edit has changed, re-finding stale stored results instead" in {
    // Stored against "needle\nneedle\nneedle"; an edit turned the second line into "noodle" and shifted the third.
    val content = Rope("xx\nneedle\nnoodle\nneedle")
    val stored  = FindState("needle", Vector(FindResult(0, 0), FindResult(1, 0), FindResult(2, 0)), 1)

    val next   = FindNavigation.step(content, stored, CursorPosition(1, 0), FindDirection.Forward)
    val target = next.flatMap(_.resultSet.selectedResult)

    target shouldBe Some(FindResult(3, 0))
    next.map(_.results) shouldBe Some(Vector(FindResult(1, 0), FindResult(3, 0)))
  }

  it should "step past the end of a capped window onto the true next match" in {
    val lines   = (0 until 2500).map(line => s"needle $line").mkString("\n")
    val content = Rope(lines)
    val window  = FindSearch.search(content, "needle", FindOptions.default, anchor = 0)
    val last    = window.results.length - 1
    val stored  = FindState("needle", window.results, last, FindOptions.default, window.capped)

    val next = FindNavigation.step(content, stored, CursorPosition(last, 0), FindDirection.Forward)

    next.flatMap(_.resultSet.selectedResult) shouldBe Some(FindResult(last + 1, 0))
    next.exists(_.capped) shouldBe true
  }

  it should "use the find options when stepping" in {
    val stored = FindState("cat", Vector(FindResult(0, 0)), 0, FindOptions(wholeWord = true))

    FindNavigation
      .step(Rope("cat concatenate cat"), stored, CursorPosition(0, 0), FindDirection.Forward)
      .flatMap(_.resultSet.selectedResult) shouldBe Some(FindResult(0, 16))
  }

  "FindHighlights" should "paint the active buffer's matches only while a find surface is open" in {
    val findState = FindState("needle", Vector(FindResult(0, 0), FindResult(1, 4)), 1)
    val closed    = withFindState(stateWith("needle\nthe needle"), findState)
    val open = withFindState(
      stateWith("needle\nthe needle", find = Some(Modal.Find(TextField.of("needle"), findState.results, 1))),
      findState
    )

    FindHighlights.paintedFindState(closed, bufferId) shouldBe None
    FindHighlights.paintedFindState(open, bufferId) shouldBe Some(findState)
  }

  it should "highlight each visible match with its extent, mark the current one and drop one an edit invalidated" in {
    val findState = FindState("needle", Vector(FindResult(0, 0), FindResult(1, 4), FindResult(2, 0)), 1)

    FindHighlights.onLines(Rope("needle\nthe needle\nnoodle"), findState, Set(0, 1, 2)) shouldBe Map(
      0 -> List(FindHighlight(CursorPosition(0, 0), CursorPosition(0, 6), current = false)),
      1 -> List(FindHighlight(CursorPosition(1, 4), CursorPosition(1, 10), current = true))
    )
  }

  "DamageProducer" should "repaint the rows whose painted find matches change" in {
    val modal  = Some[Modal.Find](Modal.Find(TextField.of("needle"), Vector.empty, 0))
    val base   = stateWith("needle\nx\nneedle\ny\nneedle", find = modal)
    val before = withFindState(base, FindState("needle", Vector(FindResult(0, 0), FindResult(2, 0)), 0))
    val after  = withFindState(base, FindState("needle", Vector(FindResult(0, 0), FindResult(2, 0)), 1))

    DamageProducer.forTransition(before, after) shouldBe Damage.BufferRows(bufferId, Set(0, 2))
  }

  "An open find" should "be refreshed when the document it searches changes underneath it, and only then" in {
    val modal  = Some[Modal.Find](Modal.Find(TextField.of("needle"), Vector(FindResult(0, 0)), 0))
    val before = stateWith("needle", find = modal)
    val edited = stateWith("xx needle", find = modal)

    ModalEventReducer.findRefreshDue(before, before) shouldBe None
    ModalEventReducer.findRefreshDue(before, edited).map(request => (request.query, request.purpose)) shouldBe Some(
      ("needle", FindSearchPurpose.Refresh)
    )
  }

  it should "apply a refresh without moving the caret" in {
    val modal = Some[Modal.Find](Modal.Find(TextField.of("needle"), Vector(FindResult(0, 0)), 0))
    val state = stateWith("xx needle\nneedle", caret = CursorPosition(0, 1), find = modal)
    val request =
      ModalEventReducer.findRefreshDue(stateWith("needle", find = modal), state).getOrElse(fail("no refresh"))
    val matches = FindSearch.search(request.content, request.query, request.options, request.anchor)

    val refreshed = ModalEventReducer.applyFindSearchResults(state, request, matches.results, matches.capped)

    refreshed.persisted.buffers(bufferId).editing.cursorPositions shouldBe List(CursorPosition(0, 1))
    refreshed.persisted.buffers(bufferId).findState shouldBe Some(
      FindState("needle", Vector(FindResult(0, 3), FindResult(1, 0)), 0)
    )
  }

  "The find keymap" should "bind Alt+C/W/R to the option toggles and F3/Shift+F3 to next/previous" in {
    val translator     = new SingleLineFormTranslator()
    def alt(key: Char) = KeyStrokeInfo(InputKey.Character, Some(key), Set(Modifier.Alt))

    translator.translate(alt('c')) shouldBe ModalToggleFindOption(FindOption.MatchCase)
    translator.translate(alt('w')) shouldBe ModalToggleFindOption(FindOption.WholeWord)
    translator.translate(alt('r')) shouldBe ModalToggleFindOption(FindOption.Regex)
    translator.translate(KeyStrokeInfo(InputKey.F3, None, Set.empty)) shouldBe ModalFindNext
    translator.translate(KeyStrokeInfo(InputKey.F3, None, Set(Modifier.Shift))) shouldBe ModalFindPrevious
  }
