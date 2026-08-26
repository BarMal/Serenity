package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MultiCursorEditingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(0)

  private def stateWithBuffer(
    content: String,
    cursors: List[CursorPosition],
    selections: List[Selection] = Nil,
    viewport: Viewport = AppState.initial.buffers(bufferId).viewport
  ): AppState =
    val buffer = AppState.initial
      .buffers(bufferId)
      .copy(
        content = com.serenity.rope.Rope(content),
        cursors = if selections.nonEmpty then selections.map(_.focus) else cursors,
        selection = selections.headOption,
        selections = selections,
        viewport = viewport
      )
    AppState.initial.copy(buffers = AppState.initial.buffers.updated(bufferId, buffer))

  private def reduce(event: EditorEvent, state: AppState): Buffer =
    VerticalNavSupport.dispatch(event, paneId, state).state.buffers(bufferId)

  "Multi-cursor editing" should "sort and deduplicate cursors before inserting text" in {
    val state = stateWithBuffer(
      "abcd",
      List(CursorPosition(0, 3), CursorPosition(0, 1), CursorPosition(0, 1))
    )

    val buffer = reduce(InsertChar('X'), state)

    buffer.content.collect() shouldBe "aXbcXd"
    buffer.cursors shouldBe List(CursorPosition(0, 2), CursorPosition(0, 5))
    buffer.allSelections shouldBe Nil
  }

  it should "paste multiline text at every cursor and place each cursor at its own insertion end" in {
    val state = stateWithBuffer(
      "ab\ncd",
      List(CursorPosition(0, 1), CursorPosition(1, 1))
    ).copy(clipboard = Some("\nX"))

    val buffer = reduce(Paste, state)

    buffer.content.collect() shouldBe "a\nXb\nc\nXd"
    buffer.cursors shouldBe List(CursorPosition(1, 1), CursorPosition(3, 1))
  }

  it should "delete backward across line boundaries for every cursor" in {
    val state = stateWithBuffer(
      "abc\ndef\nghi",
      List(CursorPosition(1, 0), CursorPosition(2, 3))
    )

    val buffer = reduce(DeleteBackward, state)

    buffer.content.collect() shouldBe "abcdef\ngh"
    buffer.cursors shouldBe List(CursorPosition(0, 3), CursorPosition(1, 2))
  }

  it should "delete forward across line boundaries and preserve cursors whose delete was a no-op" in {
    val state = stateWithBuffer(
      "ab\ncd",
      List(CursorPosition(0, 2), CursorPosition(1, 2))
    )

    val buffer = reduce(DeleteForward, state)

    buffer.content.collect() shouldBe "abcd"
    buffer.cursors shouldBe List(CursorPosition(0, 2), CursorPosition(0, 4))
  }

  it should "leave the buffer clean when every multi-cursor delete is a no-op" in {
    val state = stateWithBuffer(
      "abc",
      List(CursorPosition(0, 0), CursorPosition(0, 0))
    )

    val buffer = reduce(DeleteBackward, state)

    buffer.content.collect() shouldBe "abc"
    buffer.cursors shouldBe List(CursorPosition(0, 0), CursorPosition(0, 0))
    buffer.isDirty shouldBe false
  }

  it should "collapse cursor collisions after horizontal movement" in {
    val state = stateWithBuffer(
      "abc",
      List(CursorPosition(0, 0), CursorPosition(0, 1))
    )

    val buffer = reduce(MoveLeft, state)

    buffer.cursors shouldBe List(CursorPosition(0, 0))
    buffer.allSelections shouldBe Nil
  }

  it should "move every cursor left across line boundaries" in {
    val state = stateWithBuffer(
      "abc\ndef",
      List(CursorPosition(1, 0), CursorPosition(1, 2))
    )

    val buffer = reduce(MoveLeft, state)

    buffer.cursors shouldBe List(CursorPosition(0, 3), CursorPosition(1, 1))
  }

  it should "move every cursor right across line boundaries" in {
    val state = stateWithBuffer(
      "abc\ndef",
      List(CursorPosition(0, 3), CursorPosition(1, 2))
    )

    val buffer = reduce(MoveRight, state)

    buffer.cursors shouldBe List(CursorPosition(1, 0), CursorPosition(1, 3))
  }

  it should "move every cursor left by a word" in {
    val state = stateWithBuffer(
      "foo bar\nbaz qux",
      List(CursorPosition(0, 7), CursorPosition(1, 7))
    )

    val buffer = reduce(MoveWordLeft, state)

    buffer.cursors shouldBe List(CursorPosition(0, 4), CursorPosition(1, 4))
  }

  it should "move every cursor right by a word" in {
    val state = stateWithBuffer(
      "foo bar\nbaz qux",
      List(CursorPosition(0, 0), CursorPosition(1, 0))
    )

    val buffer = reduce(MoveWordRight, state)

    buffer.cursors shouldBe List(CursorPosition(0, 4), CursorPosition(1, 4))
  }

  it should "collapse a multi-selection to its focuses and move each one word left" in {
    val state = stateWithBuffer(
      "foo bar\nbaz qux",
      cursors = Nil,
      selections = List(
        Selection(CursorPosition(0, 5), CursorPosition(0, 7)),
        Selection(CursorPosition(1, 5), CursorPosition(1, 7))
      )
    )

    val buffer = reduce(MoveWordLeft, state)

    buffer.cursors shouldBe List(CursorPosition(0, 4), CursorPosition(1, 4))
    buffer.allSelections shouldBe Nil
  }

  it should "replace overlapping selections once as a merged editing range" in {
    val first  = Selection(CursorPosition(0, 1), CursorPosition(0, 4))
    val second = Selection(CursorPosition(0, 3), CursorPosition(0, 5))
    val state = stateWithBuffer(
      "abcdef",
      Nil,
      selections = List(first, second)
    )

    val buffer = reduce(InsertChar('X'), state)

    buffer.content.collect() shouldBe "aXf"
    buffer.cursors shouldBe List(CursorPosition(0, 2))
    buffer.allSelections shouldBe Nil
  }

  it should "replace reversed multiline selections in document order" in {
    val first  = Selection(CursorPosition(1, 2), CursorPosition(0, 1))
    val second = Selection(CursorPosition(2, 5), CursorPosition(2, 0))
    val state = stateWithBuffer(
      "alpha\nbravo\ncharlie",
      Nil,
      selections = List(second, first)
    )

    val buffer = reduce(InsertChar('X'), state)

    buffer.content.collect() shouldBe "aXavo\nXie"
    buffer.cursors shouldBe List(CursorPosition(0, 2), CursorPosition(1, 1))
    buffer.allSelections shouldBe Nil
  }

  it should "delete overlapping multi-selections once" in {
    val first  = Selection(CursorPosition(0, 1), CursorPosition(0, 4))
    val second = Selection(CursorPosition(0, 3), CursorPosition(0, 5))
    val state = stateWithBuffer(
      "abcdef",
      Nil,
      selections = List(first, second)
    )

    val buffer = reduce(DeleteForward, state)

    buffer.content.collect() shouldBe "af"
    buffer.cursors shouldBe List(CursorPosition(0, 1))
    buffer.allSelections shouldBe Nil
  }

  it should "clear in-flight vertical state after non-vertical multi-cursor movement" in {
    val state = stateWithBuffer(
      "abcdef\nxy\nabcdef",
      List(CursorPosition(0, 3), CursorPosition(0, 5))
    )

    val afterDown = reduce(MoveDown, state)
    afterDown.multiCursorVerticalStates should not be empty

    val afterLeft = reduce(MoveLeft, state.copy(buffers = state.buffers.updated(bufferId, afterDown)))

    afterLeft.multiCursorVerticalStates shouldBe Nil
  }

  /** Multi-selection movement collapses the selections to their focuses and hands the collapsed buffer to the
    * multi-cursor arms, which is an adjustment the state it travels with does not carry. Any arm that reads the buffer
    * back out of the state instead of using the one it was handed loses the collapse.
    */
  "Collapsing multiple selections for movement" should "survive the multi-cursor arm reading the buffer back" in {
    val state = stateWithBuffer(
      "abcdef",
      Nil,
      List(Selection(CursorPosition(0, 1), CursorPosition(0, 3)), Selection(CursorPosition(0, 4), CursorPosition(0, 5)))
    )

    val buffer = reduce(MoveLeft, state)

    buffer.cursors shouldBe List(CursorPosition(0, 2), CursorPosition(0, 4))
    buffer.selection shouldBe None
    buffer.selections shouldBe Nil
  }

  it should "survive it for vertical movement too" in {
    val state = stateWithBuffer(
      "abcdef\nabcdef",
      Nil,
      List(Selection(CursorPosition(1, 1), CursorPosition(1, 3)), Selection(CursorPosition(1, 4), CursorPosition(1, 5)))
    )

    val buffer = reduce(MoveUp, state)

    buffer.cursors shouldBe List(CursorPosition(0, 3), CursorPosition(0, 5))
    buffer.selections shouldBe Nil
  }

  /** With word wrap on, vertical movement lands each cursor through the layout snapshot (`snap.moveVertical`) rather
    * than the logical-line fallback the word-wrap-off cases above take. That snapshot is identical across cursors and
    * is built once for the whole move; this pins that every cursor still advances and its vertical state is retained.
    */
  "Word-wrapped multi-cursor vertical movement" should "advance every cursor down one line" in {
    val state = stateWithBuffer(
      "abcdef\nghijkl\nmnopqr\nstuvwx",
      List(CursorPosition(0, 2), CursorPosition(1, 4))
    )
    val wrapped = state.copy(config = state.config.withWordWrap(true))

    val buffer = reduce(MoveDown, wrapped)

    buffer.cursors.map(_.line) shouldBe List(1, 2)
    buffer.multiCursorVerticalStates should have size 2
  }
end MultiCursorEditingSpec
