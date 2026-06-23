package com.serenity

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.keystroke.events.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.EditorEventReducer
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, TextLayoutSnapshot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EditorEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def stateWithCommentedText(
    text: String,
    cursor: CursorPosition,
    comment: DocumentComment
  ): (PaneId, BufferId, AppState) =
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val state = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope(text),
            cursors = List(cursor),
            documentComments = List(comment)
          )
      )
    )

    (paneId, bufferId, state)

  final case class CountingRope(delegate: Rope, collectCount: AtomicInteger) extends Rope:
    override def weight: Int =
      delegate.weight

    override def height: Int =
      delegate.height

    override def isWeightBalanced: Boolean =
      delegate.isWeightBalanced

    override def isHeightBalanced: Boolean =
      delegate.isHeightBalanced

    override def rebalance: Rope =
      delegate.rebalance

    override def index(i: Int): Option[Char] =
      delegate.index(i)

    override def splitAt(index: Int): Option[(Rope, Rope)] =
      delegate.splitAt(index)

    override def collect(): String =
      collectCount.incrementAndGet()
      delegate.collect()

  "EditorEventReducer" should "insert characters into the focused pane buffer" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)

    val updatedState = EditorEventReducer.reduce(InsertChar('x'), paneId, initialState).state
    val bufferId     = updatedState.layout.editorPanes(paneId).bufferId.get
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "x"
    buffer.cursors.head shouldBe com.serenity.state.models.CursorPosition(0, 1)
    buffer.isDirty shouldBe true
  }

  it should "insert characters at every cursor position when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abcd"),
            cursors = List(CursorPosition(0, 1), CursorPosition(0, 3))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(InsertChar('X'), paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "aXbcXd"
    buffer.cursors shouldBe List(CursorPosition(0, 2), CursorPosition(0, 5))
  }

  it should "move document comments after inserted text before them" in {
    val comment = DocumentComment(CursorPosition(0, 4), CursorPosition(0, 7), "note")
    val (paneId, bufferId, initialState) =
      stateWithCommentedText("abc def", CursorPosition(0, 0), comment)

    val updatedState = EditorEventReducer.reduce(InsertChar('X'), paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "Xabc def"
    buffer.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 5), CursorPosition(0, 8), "note")
    )
  }

  it should "move document comments down after newlines inserted before them" in {
    val comment = DocumentComment(CursorPosition(0, 4), CursorPosition(0, 7), "note")
    val (paneId, bufferId, initialState) =
      stateWithCommentedText("abc def", CursorPosition(0, 0), comment)

    val updatedState = EditorEventReducer.reduce(NewLine, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "\nabc def"
    buffer.documentComments shouldBe List(
      DocumentComment(CursorPosition(1, 4), CursorPosition(1, 7), "note")
    )
  }

  it should "move document comments left after deleted text before them" in {
    val comment = DocumentComment(CursorPosition(0, 4), CursorPosition(0, 7), "note")
    val (paneId, bufferId, initialState) =
      stateWithCommentedText("abc def", CursorPosition(0, 0), comment)

    val updatedState = EditorEventReducer.reduce(DeleteForward, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "bc def"
    buffer.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 3), CursorPosition(0, 6), "note")
    )
  }

  it should "expand document comments when text is inserted inside them" in {
    val comment = DocumentComment(CursorPosition(0, 4), CursorPosition(0, 7), "note")
    val (paneId, bufferId, initialState) =
      stateWithCommentedText("abc def", CursorPosition(0, 5), comment)

    val updatedState = EditorEventReducer.reduce(InsertChar('X'), paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "abc dXef"
    buffer.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 4), CursorPosition(0, 8), "note")
    )
  }

  it should "move document comments when a selected line is indented" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val comment  = DocumentComment(CursorPosition(0, 0), CursorPosition(0, 4), "note")
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("beta"),
            cursors = List(CursorPosition(0, 4)),
            selection = Some(Selection(CursorPosition(0, 0), CursorPosition(0, 4))),
            documentComments = List(comment)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(TabKey, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "    beta"
    buffer.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 4), CursorPosition(0, 8), "note")
    )
  }

  it should "move document comments when a line before them is cut" in {
    val comment = DocumentComment(CursorPosition(1, 0), CursorPosition(1, 4), "note")
    val (paneId, bufferId, initialState) =
      stateWithCommentedText("alpha\nbeta", CursorPosition(0, 0), comment)

    val updatedState = EditorEventReducer.reduce(Cut, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "beta"
    buffer.documentComments shouldBe List(
      DocumentComment(CursorPosition(0, 0), CursorPosition(0, 4), "note")
    )
  }

  it should "insert newlines at every cursor position when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abcd"),
            cursors = List(CursorPosition(0, 1), CursorPosition(0, 3))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(NewLine, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "a\nbc\nd"
    buffer.cursors shouldBe List(CursorPosition(1, 0), CursorPosition(2, 0))
  }

  it should "insert fixed spaces at every cursor position when tab is pressed with multiple cursors" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abcd"),
            cursors = List(CursorPosition(0, 1), CursorPosition(0, 3))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(TabKey, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "a    bc    d"
    buffer.cursors shouldBe List(CursorPosition(0, 5), CursorPosition(0, 11))
  }

  it should "remove one indentation level when reverse-tab is pressed with a single cursor" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("    abc"),
            cursors = List(CursorPosition(0, 6))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(ReverseTabKey, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "abc"
    buffer.cursors shouldBe List(CursorPosition(0, 2))
  }

  it should "delete backward at every cursor position when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("1a2b3"),
            cursors = List(CursorPosition(0, 2), CursorPosition(0, 4))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteBackward, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "123"
    buffer.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 2))
  }

  it should "delete forward at every cursor position when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("1a2b3"),
            cursors = List(CursorPosition(0, 1), CursorPosition(0, 3))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteForward, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "123"
    buffer.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 2))
  }

  it should "delete the previous word for a single cursor" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta gamma"),
            cursors = List(CursorPosition(0, 16))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteWordBackward, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "alpha beta "
    buffer.cursors shouldBe List(CursorPosition(0, 11))
  }

  it should "delete the next word for a single cursor" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta gamma"),
            cursors = List(CursorPosition(0, 6))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteWordForward, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "alpha gamma"
    buffer.cursors shouldBe List(CursorPosition(0, 6))
  }

  it should "delete the previous word once when multiple cursors overlap the same word" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta gamma"),
            cursors = List(CursorPosition(0, 8), CursorPosition(0, 10))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteWordBackward, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "alpha  gamma"
    buffer.cursors shouldBe List(CursorPosition(0, 6))
  }

  it should "materialise buffer text once while calculating multi-cursor word deletions" in {
    val paneId       = PaneId(0)
    val bufferId     = BufferId(0)
    val collectCount = AtomicInteger(0)
    val content      = CountingRope(Rope("alpha beta gamma"), collectCount)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = content,
            cursors = List(CursorPosition(0, 8), CursorPosition(0, 10))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteWordBackward, paneId, initialState).state
    collectCount.get() shouldBe 1

    val buffer = updatedState.buffers(bufferId)
    buffer.content.collect() shouldBe "alpha  gamma"
    buffer.cursors shouldBe List(CursorPosition(0, 6))
  }

  it should "delete the next word once when multiple cursors overlap the same word" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha beta gamma"),
            cursors = List(CursorPosition(0, 6), CursorPosition(0, 8))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteWordForward, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "alpha gamma"
    buffer.cursors shouldBe List(CursorPosition(0, 6))
  }

  it should "paste clipboard content at every cursor position when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("ab"),
            cursors = List(CursorPosition(0, 0), CursorPosition(0, 2))
          )
      ),
      clipboard = Some("Z")
    )

    val updatedState = EditorEventReducer.reduce(Paste, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "ZabZ"
    buffer.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 4))
  }

  it should "replace every active selection when inserting with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = com.serenity.state.models.Selection(CursorPosition(0, 0), CursorPosition(0, 3))
    val second   = com.serenity.state.models.Selection(CursorPosition(0, 8), CursorPosition(0, 11))
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abc def ghi"),
            cursors = List(first.focus, second.focus),
            selection = Some(first),
            selections = List(first, second)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(InsertChar('X'), paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "X def X"
    buffer.cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 7))
    buffer.allSelections shouldBe Nil
  }

  it should "indent selected lines when tab is pressed with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(0, 2), CursorPosition(1, 2))
    val second   = Selection(CursorPosition(2, 0), CursorPosition(2, 5))
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha\nbeta\ngamma"),
            cursors = List(first.focus, second.focus),
            selection = Some(first),
            selections = List(first, second)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(TabKey, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "    alpha\n    beta\n    gamma"
    buffer.cursors shouldBe List(CursorPosition(1, 6), CursorPosition(2, 9))
    buffer.allSelections shouldBe Nil
  }

  it should "replace every active selection when pasting with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(0, 0), CursorPosition(0, 3))
    val second   = Selection(CursorPosition(0, 8), CursorPosition(0, 11))
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abc def ghi"),
            cursors = List(first.focus, second.focus),
            selection = Some(first),
            selections = List(first, second)
          )
      ),
      clipboard = Some("ZZ")
    )

    val updatedState = EditorEventReducer.reduce(Paste, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "ZZ def ZZ"
    buffer.cursors shouldBe List(CursorPosition(0, 2), CursorPosition(0, 9))
    buffer.allSelections shouldBe Nil
  }

  it should "delete every active selection when delete backward is pressed with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = com.serenity.state.models.Selection(CursorPosition(0, 0), CursorPosition(0, 3))
    val second   = com.serenity.state.models.Selection(CursorPosition(0, 8), CursorPosition(0, 11))
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abc def ghi"),
            cursors = List(first.focus, second.focus),
            selection = Some(first),
            selections = List(first, second)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteBackward, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe " def "
    buffer.cursors shouldBe List(CursorPosition(0, 0), CursorPosition(0, 5))
    buffer.allSelections shouldBe Nil
  }

  it should "unindent selected lines when reverse-tab is pressed with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(0, 4), CursorPosition(1, 2))
    val second   = Selection(CursorPosition(2, 0), CursorPosition(2, 6))
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("    alpha\n  beta\n\tgamma"),
            cursors = List(first.focus, second.focus),
            selection = Some(first),
            selections = List(first, second)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(ReverseTabKey, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "alpha\nbeta\ngamma"
    buffer.cursors shouldBe List(CursorPosition(1, 0), CursorPosition(2, 5))
    buffer.allSelections shouldBe Nil
  }

  it should "delete every active selection when delete forward is pressed with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(0, 0), CursorPosition(0, 3))
    val second   = Selection(CursorPosition(0, 8), CursorPosition(0, 11))
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abc def ghi"),
            cursors = List(first.focus, second.focus),
            selection = Some(first),
            selections = List(first, second)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteForward, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe " def "
    buffer.cursors shouldBe List(CursorPosition(0, 0), CursorPosition(0, 5))
    buffer.allSelections shouldBe Nil
  }

  it should "delete every active selection when deleting the next word with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(0, 0), CursorPosition(0, 3))
    val second   = Selection(CursorPosition(0, 8), CursorPosition(0, 11))
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abc def ghi"),
            cursors = List(first.focus, second.focus),
            selection = Some(first),
            selections = List(first, second)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(DeleteWordForward, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe " def "
    buffer.cursors shouldBe List(CursorPosition(0, 0), CursorPosition(0, 5))
    buffer.allSelections shouldBe Nil
  }

  it should "move every selection focus by a page when multiple selections are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(1, 0), CursorPosition(1, 1))
    val second   = Selection(CursorPosition(3, 0), CursorPosition(3, 1))
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("0\n1\n2\n3\n4\n5"),
            cursors = List(first.focus, second.focus),
            selection = Some(first),
            selections = List(first, second),
            viewport = AppState.initial.buffers(bufferId).viewport.copy(visibleLines = 2)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(PageDown, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(3, 0), CursorPosition(5, 0))
    buffer.allSelections shouldBe Nil
  }

  it should "move every selection focus to the start of the file when multiple selections are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(1, 0), CursorPosition(1, 1))
    val second   = Selection(CursorPosition(3, 0), CursorPosition(3, 1))
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("0\n1\n2\n3\n4\n5"),
            cursors = List(first.focus, second.focus),
            selection = Some(first),
            selections = List(first, second)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToStartOfFile, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(0, 0))
    buffer.allSelections shouldBe Nil
  }

  it should "move every selection focus to the end of the file when multiple selections are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(1, 0), CursorPosition(1, 1))
    val second   = Selection(CursorPosition(3, 0), CursorPosition(3, 1))
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("0\n1\n2\n3\n45"),
            cursors = List(first.focus, second.focus),
            selection = Some(first),
            selections = List(first, second)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToEndOfFile, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(4, 2))
    buffer.allSelections shouldBe Nil
  }

  it should "move every cursor left when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abcd"),
            cursors = List(CursorPosition(0, 2), CursorPosition(0, 4))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveLeft, paneId, initialState).state
    updatedState.buffers(bufferId).cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 3))
  }

  it should "unindent every cursor line when reverse-tab is pressed with multiple cursors" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("    one\n  two\n\tthree"),
            cursors = List(CursorPosition(0, 4), CursorPosition(1, 2), CursorPosition(2, 6))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(ReverseTabKey, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "one\ntwo\nthree"
    buffer.cursors shouldBe List(CursorPosition(0, 0), CursorPosition(1, 0), CursorPosition(2, 5))
  }

  it should "move every cursor right when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abcd"),
            cursors = List(CursorPosition(0, 0), CursorPosition(0, 2))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveRight, paneId, initialState).state
    updatedState.buffers(bufferId).cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 3))
  }

  it should "move every cursor to line start when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abcd"),
            cursors = List(CursorPosition(0, 2), CursorPosition(0, 4))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToStart, paneId, initialState).state
    updatedState.buffers(bufferId).cursors shouldBe List(CursorPosition(0, 0))
  }

  it should "move every cursor to line end when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abcd"),
            cursors = List(CursorPosition(0, 0), CursorPosition(0, 2))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToEnd, paneId, initialState).state
    updatedState.buffers(bufferId).cursors shouldBe List(CursorPosition(0, 4))
  }

  it should "move every cursor down while preserving per-cursor columns when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abcd\nwxyz"),
            cursors = List(CursorPosition(0, 1), CursorPosition(0, 3))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveDown, paneId, initialState).state
    updatedState.buffers(bufferId).cursors shouldBe List(CursorPosition(1, 1), CursorPosition(1, 3))
  }

  it should "move every cursor up while preserving per-cursor columns when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abcd\nwxyz"),
            cursors = List(CursorPosition(1, 1), CursorPosition(1, 3))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveUp, paneId, initialState).state
    updatedState.buffers(bufferId).cursors shouldBe List(CursorPosition(0, 1), CursorPosition(0, 3))
  }

  it should "move every cursor to the start of the file when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha\nbeta\ngamma"),
            cursors = List(CursorPosition(0, 3), CursorPosition(2, 4))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToStartOfFile, paneId, initialState).state
    updatedState.buffers(bufferId).cursors shouldBe List(CursorPosition(0, 0))
  }

  it should "move every cursor to the end of the file when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha\nbeta\ngamma"),
            cursors = List(CursorPosition(0, 1), CursorPosition(1, 2))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToEndOfFile, paneId, initialState).state
    updatedState.buffers(bufferId).cursors shouldBe List(CursorPosition(2, 5))
  }

  it should "move every cursor up by a visible page when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("0\n1\n2\n3\n4\n5"),
            cursors = List(CursorPosition(3, 0), CursorPosition(5, 0)),
            viewport = AppState.initial.buffers(bufferId).viewport.copy(topLine = 2, visibleLines = 2)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(PageUp, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(1, 0), CursorPosition(3, 0))
    buffer.viewport.topLine shouldBe 0
  }

  it should "select the whole buffer when select-all is pressed with multiple cursors" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha\nbeta"),
            cursors = List(CursorPosition(0, 1), CursorPosition(1, 2))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(SelectAll, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(1, 4))
    buffer.selection shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
  }

  it should "select the whole buffer when select-all is pressed with multiple selections" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val first    = Selection(CursorPosition(0, 1), CursorPosition(0, 3))
    val second   = Selection(CursorPosition(1, 0), CursorPosition(1, 2))
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha\nbeta"),
            cursors = List(first.focus, second.focus),
            selection = Some(first),
            selections = List(first, second)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(SelectAll, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(1, 4))
    buffer.selection shouldBe Some(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
    buffer.allSelections shouldBe List(Selection(CursorPosition(0, 0), CursorPosition(1, 4)))
  }

  it should "extend a selection horizontally with shift navigation" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abcd"),
            cursors = List(CursorPosition(0, 1))
          )
      )
    )

    val firstState  = EditorEventReducer.reduce(ExtendSelectionRight, paneId, initialState).state
    val secondState = EditorEventReducer.reduce(ExtendSelectionRight, paneId, firstState).state
    val buffer      = secondState.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(0, 3))
    buffer.selection shouldBe Some(Selection(CursorPosition(0, 1), CursorPosition(0, 3)))
    buffer.allSelections shouldBe List(Selection(CursorPosition(0, 1), CursorPosition(0, 3)))
  }

  it should "extend a selection vertically with shift navigation" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abc\ndef"),
            cursors = List(CursorPosition(1, 1))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(ExtendSelectionUp, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(0, 1))
    buffer.selection shouldBe Some(Selection(CursorPosition(1, 1), CursorPosition(0, 1)))
    buffer.selection.map(_.start) shouldBe Some(CursorPosition(0, 1))
    buffer.selection.map(_.end) shouldBe Some(CursorPosition(1, 1))
  }

  it should "open the goto-line modal from editor events even when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha\nbeta"),
            cursors = List(CursorPosition(0, 1), CursorPosition(1, 2))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(OpenGotoLine, paneId, initialState).state
    val modalSurface = updatedState.modalSurface

    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("")))
    updatedState.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "open find from editor events even when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha\nbeta"),
            cursors = List(CursorPosition(0, 1), CursorPosition(1, 2))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(OpenFind, paneId, initialState).state
    val modalSurface = updatedState.modalSurface

    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.Find("", Nil, 0)))
    modalSurface.map(_.presentation) shouldBe Some(
      SurfacePresentation.Floating(Some(CursorPosition(0, 1)), SurfacePlacement.BelowCursor)
    )
    updatedState.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "seed find from the active buffer's existing find state" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha\nbeta\nalpha"),
            cursors = List(CursorPosition(2, 0)),
            findState = Some(FindState("alpha", List(FindResult(0, 0), FindResult(2, 0)), 1))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(OpenFind, paneId, initialState).state
    val modalSurface = updatedState.modalSurface

    modalSurface.map(_.content) shouldBe Some(
      SurfaceContent.ModalWorkflow(Modal.Find("alpha", List(FindResult(0, 0), FindResult(2, 0)), 1))
    )
    modalSurface.map(_.presentation) shouldBe Some(
      SurfacePresentation.Floating(Some(CursorPosition(2, 0)), SurfacePlacement.BelowCursor)
    )
    updatedState.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "advance find-next from the stored query even when multiple cursors are active" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("match alpha\nbeta\nmatch gamma"),
            cursors = List(CursorPosition(0, 1), CursorPosition(2, 2)),
            findState = Some(FindState("match", List(FindResult(0, 0), FindResult(2, 0)), 0)),
            viewport = AppState.initial.buffers(bufferId).viewport.copy(visibleLines = 2)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(FindNext, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(2, 0))
    buffer.findState shouldBe Some(FindState("match", List(FindResult(0, 0), FindResult(2, 0)), 1))
  }

  it should "advance find-next through multiple occurrences on one line by column" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("needle then needle"),
            cursors = List(CursorPosition(0, 0)),
            findState = Some(FindState("needle", List(FindResult(0, 0), FindResult(0, "needle then ".length)), 0))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(FindNext, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(0, "needle then ".length))
    buffer.findState shouldBe Some(
      FindState("needle", List(FindResult(0, 0), FindResult(0, "needle then ".length)), 1)
    )
  }

  it should "scroll wrapped text to the selected find-next visual row" in {
    val paneId       = PaneId(0)
    val bufferId     = BufferId(0)
    val prefix       = List.fill(80)("wrapped").mkString(" ")
    val content      = s"first $prefix needle"
    val needleColumn = content.indexOf("needle")
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope(content),
            cursors = List(CursorPosition(0, 0)),
            findState = Some(FindState("needle", List(FindResult(0, needleColumn)), 0)),
            viewport = Viewport(0, 0, visibleLines = 3, visibleColumns = 12)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(FindNext, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)
    val cursor       = buffer.cursors.head
    val font         = FontLoader.previewTextFont(updatedState.config.fontConfig)
    val metrics      = CellMetrics.fromFont(font)
    val snapshot =
      TextLayoutSnapshot.fromBuffer(buffer, buffer.viewport.visibleColumns * metrics.charWidth, font)

    cursor shouldBe CursorPosition(0, needleColumn)
    buffer.viewport.topLine shouldBe 0
    buffer.viewport.topVisualLine should be > 0
    withClue(
      s"viewport=${buffer.viewport} cursor=$cursor visualLines=${snapshot.visualLines.map(line => (line.startColumn, line.endColumn))}"
    ) {
      snapshot.visualLines.exists(line =>
        line.bufferLine == cursor.line && cursor.column >= line.startColumn && cursor.column <= line.endColumn
      ) shouldBe true
    }
  }

  it should "refresh stored find-all results after text is inserted before matches" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("needle\nneedle"),
            cursors = List(CursorPosition(0, 0)),
            findState = Some(FindState("needle", List(FindResult(0, 0), FindResult(1, 0)), 0))
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(InsertChar('x'), paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.content.collect() shouldBe "xneedle\nneedle"
    buffer.findState shouldBe Some(FindState("needle", List(FindResult(0, 1), FindResult(1, 0)), 0))
  }

  it should "clear stored find-all results when an edit removes the last match" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("needle"),
            cursors = List(CursorPosition(0, "needle".length)),
            findState = Some(FindState("needle", List(FindResult(0, 0)), 0))
          )
      )
    )

    val afterFirstDelete = EditorEventReducer.reduce(DeleteBackward, paneId, initialState).state
    val buffer           = afterFirstDelete.buffers(bufferId)

    buffer.content.collect() shouldBe "needl"
    buffer.findState shouldBe None
  }

  it should "restore each cursor's preferred column after moving through a shorter line" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("abcdef\nxy\nabcdef"),
            cursors = List(CursorPosition(0, 1), CursorPosition(0, 4))
          )
      )
    )

    val afterFirstMove  = EditorEventReducer.reduce(MoveDown, paneId, initialState).state
    val afterSecondMove = EditorEventReducer.reduce(MoveDown, paneId, afterFirstMove).state
    val buffer          = afterSecondMove.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(2, 1), CursorPosition(2, 4))
  }

  it should "update viewport position for scroll events" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)
    val bufferId     = initialState.layout.editorPanes(paneId).bufferId.get
    val seededState = initialState.copy(
      buffers = initialState.buffers.updated(
        bufferId,
        initialState
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("a\nb\nc\nd\ne\nf\ng"),
            viewport = initialState.buffers(bufferId).viewport.copy(visibleLines = 2)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(ScrollDown(3), paneId, seededState).state

    updatedState.buffers(bufferId).viewport.topLine shouldBe 3
  }

  it should "move the cursor up by a visible page while clamping at the top" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("0\n1\n2\n3\n4\n5"),
            cursors = List(CursorPosition(4, 1)),
            viewport = AppState.initial.buffers(bufferId).viewport.copy(topLine = 3, visibleLines = 2)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(PageUp, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(2, 0))
    buffer.viewport.topLine shouldBe 1
  }

  it should "move the cursor to the start of the file" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha\nbeta\ngamma"),
            cursors = List(CursorPosition(2, 3)),
            viewport = AppState.initial.buffers(bufferId).viewport.copy(topLine = 2)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToStartOfFile, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(0, 0))
    buffer.viewport.topLine shouldBe 0
  }

  it should "move the cursor to the end of the file" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("alpha\nbeta\ngamma"),
            cursors = List(CursorPosition(0, 1)),
            viewport = AppState.initial.buffers(bufferId).viewport.copy(visibleLines = 2)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(MoveToEndOfFile, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(2, 5))
    buffer.viewport.topLine shouldBe 1
  }

  it should "move the cursor down by a visible page" in {
    val paneId   = PaneId(0)
    val bufferId = BufferId(0)
    val initialState = AppState.initial.copy(
      buffers = AppState.initial.buffers.updated(
        bufferId,
        AppState.initial
          .buffers(bufferId)
          .copy(
            content = com.serenity.rope.Rope("0\n1\n2\n3\n4\n5"),
            cursors = List(CursorPosition(1, 1)),
            viewport = AppState.initial.buffers(bufferId).viewport.copy(visibleLines = 2)
          )
      )
    )

    val updatedState = EditorEventReducer.reduce(PageDown, paneId, initialState).state
    val buffer       = updatedState.buffers(bufferId)

    buffer.cursors shouldBe List(CursorPosition(3, 0))
    buffer.viewport.topLine shouldBe 2
  }

  it should "open the goto line modal from editor events" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)

    val updatedState = EditorEventReducer.reduce(OpenGotoLine, paneId, initialState).state
    val modalSurface = updatedState.modalSurface

    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("")))
    updatedState.focus shouldBe Focus.Surface(modalSurface.get.id)
  }

  it should "provide a typed reducer instance for editor events" in {
    val initialState = AppState.initial
    val paneId       = PaneId(0)
    val reducer      = EditorEventReducer.reducer(paneId)

    val updatedState = reducer.reduce(OpenGotoLine, initialState).state
    val modalSurface = updatedState.modalSurface

    modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(Modal.GotoLine("")))
    updatedState.focus shouldBe Focus.Surface(modalSurface.get.id)
  }
