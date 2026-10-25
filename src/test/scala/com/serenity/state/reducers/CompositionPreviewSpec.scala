package com.serenity.state.reducers

import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What is drawn while an input method composes (#2008): a copy of the buffer with the pre-edit spliced in, so layout,
  * wrapping and carets are the real ones, and the real state untouched.
  */
class CompositionPreviewSpec extends AnyFlatSpec with Matchers with OptionValues:

  given Balance = Balance.default

  private val revisions = new CompositionRevisions

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def composition(
    anchor: CursorPosition,
    text: String,
    caret: Option[Int] = None,
    version: Long = 0L
  ): Composition =
    Composition(
      paneId,
      bufferId,
      anchor,
      version,
      text,
      List(CompositionClause(0, text.length, selected = true)),
      caret
    )

  private def stateWith(text: String, cursors: List[CursorPosition], composing: Option[Composition]): AppState =
    val buffer =
      Buffer.fromString(bufferId, text).copy(editing = EditingStateFixtures(cursors = cursors, selection = None))
    val base = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    base.copy(runtime = base.runtime.copy(composition = composing))

  private def bufferOf(state: AppState): Buffer = state.persisted.buffers(bufferId)

  "CompositionPreview.displayed" should "return the state itself when nothing is composing" in {
    val state = stateWith("hello", List(CursorPosition(0, 2)), None)

    CompositionPreview.displayed(state, revisions) shouldBe theSameInstanceAs(state)
  }

  it should "splice the pre-edit into the text at the anchor" in {
    val state = stateWith("hello", List(CursorPosition(0, 2)), Some(composition(CursorPosition(0, 2), "かん")))

    bufferOf(CompositionPreview.displayed(state, revisions)).document.content.collect() shouldBe "heかんllo"
  }

  it should "leave the real state exactly as it was" in {
    val state = stateWith("hello", List(CursorPosition(0, 2)), Some(composition(CursorPosition(0, 2), "かん")))
    val _     = CompositionPreview.displayed(state, revisions)

    bufferOf(state).document.content.collect() shouldBe "hello"
    bufferOf(state).document.isDirty shouldBe false
  }

  it should "put the primary cursor at the composition's caret" in {
    val state =
      stateWith("hello", List(CursorPosition(0, 2)), Some(composition(CursorPosition(0, 2), "かんじ", Some(1))))

    bufferOf(CompositionPreview.displayed(state, revisions)).cursorList.head.position shouldBe CursorPosition(0, 3)
  }

  it should "put the primary cursor at the end of the pre-edit when the caret is unknown" in {
    val state = stateWith("hello", List(CursorPosition(0, 2)), Some(composition(CursorPosition(0, 2), "かんじ")))

    bufferOf(CompositionPreview.displayed(state, revisions)).cursorList.head.position shouldBe CursorPosition(0, 5)
  }

  it should "move the other cursors along with the text they sit in" in {
    val state = stateWith(
      "hello\nworld",
      List(CursorPosition(0, 2), CursorPosition(0, 5), CursorPosition(1, 3)),
      Some(composition(CursorPosition(0, 2), "かん"))
    )

    bufferOf(CompositionPreview.displayed(state, revisions)).cursorList.toList.map(_.position) shouldBe
      List(CursorPosition(0, 4), CursorPosition(0, 7), CursorPosition(1, 3))
  }

  it should "keep the buffer's dirty flag as it was" in {
    val state = stateWith("hello", List(CursorPosition(0, 2)), Some(composition(CursorPosition(0, 2), "か")))

    bufferOf(CompositionPreview.displayed(state, revisions)).document.isDirty shouldBe false
  }

  it should "give each pre-edit a revision of its own, apart from every real one" in {
    val first  = stateWith("hello", List(CursorPosition(0, 2)), Some(composition(CursorPosition(0, 2), "か")))
    val second = stateWith("hello", List(CursorPosition(0, 2)), Some(composition(CursorPosition(0, 2), "かん")))
    val one    = bufferOf(CompositionPreview.displayed(first, revisions)).document.contentVersion
    val two    = bufferOf(CompositionPreview.displayed(second, revisions)).document.contentVersion

    one should not be two
    one should be > bufferOf(first).document.contentVersion + 1000L
  }

  it should "show the same revision again for the same pre-edit, so a repeated frame sees no change" in {
    val state = stateWith("hello", List(CursorPosition(0, 2)), Some(composition(CursorPosition(0, 2), "か")))

    bufferOf(CompositionPreview.displayed(state, revisions)).document.contentVersion shouldBe
      bufferOf(CompositionPreview.displayed(state, revisions)).document.contentVersion
  }

  it should "show nothing for a composition begun against an older version of the buffer" in {
    val state =
      stateWith("hello", List(CursorPosition(0, 2)), Some(composition(CursorPosition(0, 2), "か", version = 7L)))

    CompositionPreview.displayed(state, revisions) shouldBe theSameInstanceAs(state)
  }

  it should "show nothing for an empty pre-edit or one with a line break" in {
    val empty  = stateWith("hello", List(CursorPosition(0, 2)), Some(composition(CursorPosition(0, 2), "")))
    val broken = stateWith("hello", List(CursorPosition(0, 2)), Some(composition(CursorPosition(0, 2), "a\nb")))

    CompositionPreview.displayed(empty, revisions) shouldBe theSameInstanceAs(empty)
    CompositionPreview.displayed(broken, revisions) shouldBe theSameInstanceAs(broken)
  }

  it should "show nothing while the composition is refused or its buffer is gone" in {
    val gone    = stateWith("hello", List(CursorPosition(0, 2)), Some(composition(CursorPosition(0, 2), "か")))
    val without = gone.copy(persisted = gone.persisted.copy(buffers = Map.empty))

    CompositionPreview.displayed(without, revisions) shouldBe theSameInstanceAs(without)
  }

  "CompositionPreview.spanOf" should "name the pre-edit's columns on the displayed buffer" in {
    val state = stateWith(
      "hello",
      List(CursorPosition(0, 2)),
      Some(composition(CursorPosition(0, 2), "かんじ", Some(1)))
    )
    val shown = CompositionPreview.displayed(state, revisions)

    CompositionPreview.spanOf(shown, bufferOf(shown)).value shouldBe CompositionPreview.Span(
      line = 0,
      startColumn = 2,
      endColumn = 5,
      clauses = List(CompositionClause(0, 3, selected = true))
    )
  }

  it should "name nothing on a buffer that is not the displayed copy" in {
    val state = stateWith("hello", List(CursorPosition(0, 2)), Some(composition(CursorPosition(0, 2), "か")))

    CompositionPreview.spanOf(state, bufferOf(state)) shouldBe None
  }
