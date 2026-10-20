package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.richtext.{DocumentFeature, RichTextDocument, RichTextParagraph}
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Input-method composition (#2008): pre-edit lives in `Runtime.composition` and nowhere else until the input method
  * commits, a commit goes through `InsertText`, and a composition is refused where the editor takes no text.
  */
class ImeReducerSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateWith(text: String, cursors: List[CursorPosition]): AppState =
    val buffer =
      Buffer.fromString(bufferId, text).copy(editing = EditingStateFixtures(cursors = cursors, selection = None))
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))

  private def single(text: String, column: Int): AppState = stateWith(text, List(CursorPosition(0, column)))

  private def reduced(event: TextEntryEvent, state: AppState): ReducerResult =
    EditorEventReducer.reduce(event, paneId, state)

  private def stateAfter(events: List[TextEntryEvent], state: AppState): AppState =
    events.foldLeft(state)((current, event) => reduced(event, current).state)

  private def contentOf(state: AppState): String = state.persisted.buffers(bufferId).document.content.collect()

  private val Clauses = List(CompositionClause(0, 2, selected = true), CompositionClause(2, 4, selected = false))

  private def composing(text: String = "かんじ"): ImeComposing =
    ImeComposing(text, List(CompositionClause(0, text.length, selected = true)), Some(text.length))

  "ImeComposing" should "record the pre-edit with the anchor and version it started at" in {
    val before = single("hello", 2)
    val after  = reduced(ImeComposing("かんじ", Clauses, Some(3)), before).state

    after.runtime.composition.value shouldBe Composition(
      paneId = paneId,
      bufferId = bufferId,
      anchor = CursorPosition(0, 2),
      bufferVersion = before.persisted.buffers(bufferId).document.contentVersion,
      text = "かんじ",
      clauses = Clauses,
      caret = Some(3)
    )
  }

  it should "leave everything persisted exactly as it was, so nothing that watches the buffer can see it" in {
    val before = single("hello", 2)
    val result = reduced(composing(), before)

    result.state.persisted shouldBe before.persisted
    result.effects shouldBe empty
  }

  it should "not dirty the buffer, move its version, or record an undo step" in {
    val before = single("hello", 2)
    val buffer = reduced(composing(), before).state.persisted.buffers(bufferId)

    buffer.document.isDirty shouldBe false
    buffer.document.contentVersion shouldBe before.persisted.buffers(bufferId).document.contentVersion
    buffer.document.content.collect() shouldBe "hello"
  }

  it should "keep the first anchor and version while the composition continues, and replace the text" in {
    val first = reduced(composing("か"), single("hello", 2)).state
    val moved = first.persisted.buffers(bufferId).copy(editing = EditingStateFixtures(List(CursorPosition(0, 4)), None))
    val during = first.copy(persisted = first.persisted.copy(buffers = Map(bufferId -> moved)))
    val after  = reduced(composing("かん"), during).state

    after.runtime.composition.value.anchor shouldBe CursorPosition(0, 2)
    after.runtime.composition.value.text shouldBe "かん"
  }

  it should "end the composition when the text is empty" in {
    val started = reduced(composing(), single("hello", 2)).state

    reduced(ImeComposing("", Nil, None), started).state.runtime.composition shouldBe None
  }

  it should "be ignored in a pane that shows no buffer" in {
    val initial = AppState.initial
    val empty   = EditorPane.withBuffer(paneId, bufferId).copy(bufferId = None)
    val state = initial.copy(persisted =
      initial.persisted.copy(
        buffers = Map.empty,
        layout = initial.persisted.layout.copy(editorPanes = Map(paneId -> empty))
      )
    )

    reduced(composing(), state).state.runtime.composition shouldBe None
  }

  "ImeCancelled" should "drop the pre-edit and change nothing else" in {
    val started = reduced(composing(), single("hello", 2)).state
    val result  = reduced(ImeCancelled, started)

    result.state.runtime.composition shouldBe None
    result.state.persisted shouldBe started.persisted
    result.effects shouldBe empty
  }

  "ImeCommit" should "insert the text at the cursor and end the composition" in {
    val started = reduced(composing(), single("hello", 2)).state
    val after   = reduced(ImeCommit("漢字"), started).state

    contentOf(after) shouldBe "he漢字llo"
    after.runtime.composition shouldBe None
    after.persisted.buffers(bufferId).document.isDirty shouldBe true
  }

  it should "insert once, as one undo step" in {
    val started = reduced(composing(), single("hello", 2)).state
    val result  = reduced(ImeCommit("漢字"), started)

    result.effects.collect { case AppEffect.Undo(_: UndoEffect.RecordBoundary) => 1 } should have size 1
    result.state.persisted.buffers(bufferId).document.contentVersion shouldBe
      started.persisted.buffers(bufferId).document.contentVersion + 1
  }

  it should "insert a code point outside the BMP whole" in {
    contentOf(reduced(ImeCommit("𠮷"), single("ab", 1)).state) shouldBe "a𠮷b"
  }

  it should "insert at every cursor, though the pre-edit showed at the primary one" in {
    val state = stateWith("ab\ncd", List(CursorPosition(0, 1), CursorPosition(1, 1)))
    val after = stateAfter(List(composing(), ImeCommit("字")), state)

    contentOf(after) shouldBe "a字b\nc字d"
  }

  it should "go to the recorded anchor when the cursor has moved but the buffer has not changed" in {
    val started = reduced(composing(), single("hello", 1)).state
    val moved =
      started.persisted.buffers(bufferId).copy(editing = EditingStateFixtures(List(CursorPosition(0, 4)), None))
    val during = started.copy(persisted = started.persisted.copy(buffers = Map(bufferId -> moved)))

    contentOf(reduced(ImeCommit("字"), during).state) shouldBe "h字ello"
  }

  it should "go to the current cursor when the buffer has changed since the composition began" in {
    val started = reduced(composing(), single("hello", 1)).state
    val buffer  = started.persisted.buffers(bufferId)
    val edited = buffer.copy(
      document = buffer.document.withContent(Rope("hello!")),
      editing = EditingStateFixtures(List(CursorPosition(0, 4)), None)
    )
    val during = started.copy(persisted = started.persisted.copy(buffers = Map(bufferId -> edited)))

    contentOf(reduced(ImeCommit("字"), during).state) shouldBe "hell字o!"
  }

  it should "insert at the cursor when no composition was recorded" in {
    contentOf(reduced(ImeCommit("字"), single("hello", 5)).state) shouldBe "hello字"
  }

  it should "change nothing for empty text, but still end the composition" in {
    val started = reduced(composing(), single("hello", 2)).state
    val result  = reduced(ImeCommit(""), started)

    result.state.runtime.composition shouldBe None
    result.state.persisted shouldBe started.persisted
    result.effects shouldBe empty
  }

  it should "create the buffer for a pane that shows none" in {
    val initial = AppState.initial
    val empty   = EditorPane.withBuffer(paneId, bufferId).copy(bufferId = None)
    val state = initial.copy(persisted =
      initial.persisted.copy(
        buffers = Map.empty,
        layout = initial.persisted.layout.copy(editorPanes = Map(paneId -> empty))
      )
    )

    reduced(ImeCommit("字"), state).state.persisted.buffers.values.map(_.document.content.collect()).toList shouldBe
      List("字")
  }

  "A composition on an opaque block" should "be refused: nothing recorded, the platform told to end it, the user told why" in {
    val result = reduced(composing(), opaqueState(List(CursorPosition(2, 0))))

    result.state.runtime.composition shouldBe None
    result.state.runtime.compositionRefused shouldBe true
    result.effects shouldBe List(AppEffect.Ime(ImeEffect.Refused))
  }

  it should "be refused when any cursor, not only the primary, is on the block" in {
    val result = reduced(composing(), opaqueState(List(CursorPosition(1, 2), CursorPosition(2, 0))))

    result.state.runtime.compositionRefused shouldBe true
    result.effects shouldBe List(AppEffect.Ime(ImeEffect.Refused))
  }

  it should "not be refused on a line beside the block" in {
    val result = reduced(composing(), opaqueState(List(CursorPosition(1, 2), CursorPosition(3, 1))))

    result.state.runtime.compositionRefused shouldBe false
    result.state.runtime.composition should not be empty
  }

  it should "ignore further composing until the input method commits or cancels, saying nothing more" in {
    val refused = reduced(composing(), opaqueState(List(CursorPosition(2, 0)))).state
    val again   = reduced(composing("かな"), refused)

    again.state shouldBe refused
    again.effects shouldBe empty
  }

  it should "drop the text of the commit that ends it and clear the refusal" in {
    val state   = opaqueState(List(CursorPosition(2, 0)))
    val refused = reduced(composing(), state).state
    val after   = reduced(ImeCommit("字"), refused)

    after.state.persisted shouldBe state.persisted
    after.state.runtime.compositionRefused shouldBe false
    after.effects shouldBe empty
  }

  it should "clear the refusal when the input method cancels" in {
    val refused = reduced(composing(), opaqueState(List(CursorPosition(2, 0)))).state

    reduced(ImeCancelled, refused).state.runtime.compositionRefused shouldBe false
  }

  private def opaqueState(cursors: List[CursorPosition]): AppState =
    val document = RichTextDocument(
      List(
        RichTextParagraph.plain("before"),
        RichTextParagraph.plain("middle"),
        RichTextParagraph.block("<w:tbl/>", DocumentFeature.Tables),
        RichTextParagraph.plain("after")
      )
    )
    val buffer = Buffer(
      id = bufferId,
      document = Document(content = Rope(document.plainText)),
      editing = EditingStateFixtures(cursors = cursors, selection = None),
      richText = RichTextState().withSyncedDocument(Some(document), contentVersion = 0L)
    )
    AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
