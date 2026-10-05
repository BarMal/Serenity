package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.EditingStateFixtures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated coverage for `EditorClipboardEventReducer` (#1442): Copy/Cut/Paste, the family that reads or writes the
  * clipboard alongside the buffer. Each of the three independently computes a clipboard string alongside its buffer
  * update; this suite exercises the with-selection and without-selection (whole-line) shape of each, plus multi-cursor
  * paste and the "nothing on the clipboard" no-op.
  */
class EditorClipboardEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateWith(
    text: String,
    cursors: List[CursorPosition],
    selection: Option[Selection] = None,
    clipboard: Option[String] = None,
    darlings: List[Darling] = Nil
  ): AppState =
    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(
        editing = EditingStateFixtures(cursors = cursors, selection = selection),
        annotations = Annotations(darlings = darlings)
      )
    val base = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    base.copy(runtime = base.runtime.copy(clipboard = clipboard))

  private def resultOf(event: TextEntryEvent, state: AppState): ReducerResult =
    EditorEventReducer.reduce(event, paneId, state)

  private def bufferAfter(event: TextEntryEvent, state: AppState): Buffer =
    resultOf(event, state).state.persisted.buffers(bufferId)

  private def clipboardAfter(event: TextEntryEvent, state: AppState): Option[String] =
    resultOf(event, state).state.runtime.clipboard

  private def movedTo(state: AppState, cursors: List[CursorPosition]): AppState =
    val buffer = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(bufferId, buffer.copy(editing = EditingStateFixtures(cursors = cursors)))
      )
    )

  "Copy with an active selection" should "put the selected text on the clipboard, leaving the buffer unchanged" in {
    val before = stateWith(
      "alpha beta",
      List(CursorPosition(0, 10)),
      selection = Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10)))
    )

    clipboardAfter(Copy, before) shouldBe Some("beta")
    bufferAfter(Copy, before).document.content.collect() shouldBe "alpha beta"
    bufferAfter(Copy, before).primarySelection shouldBe Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10)))
  }

  "Copy without a selection" should "put every cursor's whole line on the clipboard, one per line" in {
    val before = stateWith("alpha\nbeta\ngamma", List(CursorPosition(0, 2), CursorPosition(2, 1)))

    clipboardAfter(Copy, before) shouldBe Some("alpha\ngamma")
  }

  "Cut with an active selection" should "remove the selection and put it on the clipboard" in {
    val before = stateWith(
      "alpha beta",
      List(CursorPosition(0, 10)),
      selection = Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10)))
    )

    clipboardAfter(Cut, before) shouldBe Some("beta")
    bufferAfter(Cut, before).document.content.collect() shouldBe "alpha "
    bufferAfter(Cut, before).primarySelection shouldBe None
  }

  "Cut without a selection" should "delete every cursor's whole line and put them on the clipboard" in {
    val before = stateWith("alpha\nbeta\ngamma", List(CursorPosition(1, 0)))

    clipboardAfter(Cut, before) shouldBe Some("beta")
    bufferAfter(Cut, before).document.content.collect() shouldBe "alpha\ngamma"
  }

  "Paste with nothing on the clipboard" should "leave the buffer untouched" in {
    val before = stateWith("alpha", List(CursorPosition(0, 5)), clipboard = None)

    resultOf(Paste, before).state shouldBe before
  }

  "Paste with an empty clipboard string" should "leave the buffer untouched" in {
    val before = stateWith("alpha", List(CursorPosition(0, 5)), clipboard = Some(""))

    resultOf(Paste, before).state shouldBe before
  }

  "Paste with a single cursor" should "insert the clipboard text at the cursor" in {
    val before = stateWith("ab", List(CursorPosition(0, 1)), clipboard = Some("XY"))

    val after = bufferAfter(Paste, before)
    after.document.content.collect() shouldBe "aXYb"
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 3))
  }

  "Paste with an active selection" should "replace the selection with the clipboard text" in {
    val before = stateWith(
      "alpha beta",
      List(CursorPosition(0, 10)),
      selection = Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10))),
      clipboard = Some("XY")
    )

    val after = bufferAfter(Paste, before)
    after.document.content.collect() shouldBe "alpha XY"
    after.primarySelection shouldBe None
  }

  "Paste with multiple cursors" should "insert the clipboard text at every cursor independently" in {
    val before =
      stateWith(
        "a\nb\nc",
        List(CursorPosition(0, 1), CursorPosition(1, 1), CursorPosition(2, 1)),
        clipboard = Some("X")
      )

    val after = bufferAfter(Paste, before)
    after.document.content.collect() shouldBe "aX\nbX\ncX"
  }

  "Copy without a selection, then Paste on another line" should "insert the copied line above the caret line" in {
    val copied = resultOf(Copy, stateWith("alpha\nbeta\ngamma", List(CursorPosition(0, 2)))).state

    val after = bufferAfter(Paste, movedTo(copied, List(CursorPosition(2, 3))))
    after.document.content.collect() shouldBe "alpha\nbeta\nalpha\ngamma"
    after.editing.cursorPositions shouldBe List(CursorPosition(3, 3))
  }

  "Cut without a selection, then Paste on another line" should "move the cut line above the caret line" in {
    val cut = resultOf(Cut, stateWith("alpha\nbeta\ngamma", List(CursorPosition(0, 1)))).state

    val after = bufferAfter(Paste, movedTo(cut, List(CursorPosition(1, 2))))
    after.document.content.collect() shouldBe "beta\nalpha\ngamma"
    after.editing.cursorPositions shouldBe List(CursorPosition(2, 2))
  }

  "A whole-line paste with multiple cursors" should "insert the copied line above each caret line" in {
    val copied = resultOf(Copy, stateWith("alpha\nbeta\ngamma", List(CursorPosition(0, 0)))).state

    val after = bufferAfter(Paste, movedTo(copied, List(CursorPosition(1, 1), CursorPosition(2, 1))))
    after.document.content.collect() shouldBe "alpha\nalpha\nbeta\nalpha\ngamma"
    after.editing.cursorPositions shouldBe List(CursorPosition(2, 1), CursorPosition(4, 1))
  }

  "A whole-line paste over an active selection" should "replace the selection like any other paste" in {
    val copied = resultOf(Copy, stateWith("alpha\nbeta", List(CursorPosition(0, 0)))).state
    val selected = stateWith(
      "alpha\nbeta",
      List(CursorPosition(1, 4)),
      selection = Some(Selection(CursorPosition(1, 0), CursorPosition(1, 4)))
    ).copy(runtime = copied.runtime)

    bufferAfter(Paste, selected).document.content.collect() shouldBe "alpha\nalpha"
  }

  "A copied line replaced on the clipboard by other text" should "paste that text at the caret, not as a line" in {
    val copied   = resultOf(Copy, stateWith("alpha\nbeta", List(CursorPosition(0, 0)))).state
    val replaced = copied.copy(runtime = copied.runtime.copy(clipboard = Some("XY")))

    bufferAfter(Paste, movedTo(replaced, List(CursorPosition(1, 2)))).document.content.collect() shouldBe
      "alpha\nbeXYta"
  }

  "Paste of text with a bare CR" should "place the caret by the normalised text" in {
    val after = bufferAfter(Paste, stateWith("xy", List(CursorPosition(0, 1)), clipboard = Some("a\rb")))

    after.document.content.collect() shouldBe "xa\nby"
    after.editing.cursorPositions shouldBe List(CursorPosition(1, 1))
  }

  "Paste of CRLF text with multiple cursors" should "place every caret by the normalised text" in {
    val before = stateWith(
      "12\n34",
      List(CursorPosition(0, 1), CursorPosition(1, 1)),
      clipboard = Some("a\r\nb")
    )

    val after = bufferAfter(Paste, before)
    after.document.content.collect() shouldBe "1a\nb2\n3a\nb4"
    after.editing.cursorPositions shouldBe List(CursorPosition(1, 1), CursorPosition(3, 1))
  }

  "Paste of CRLF text" should "keep a bookmark after it on the same text" in {
    val before = stateWith("xy\nz", List(CursorPosition(0, 1)), clipboard = Some("a\r\nb"))
    val bookmarked = before.copy(persisted =
      before.persisted.copy(buffers =
        before.persisted.buffers.updated(
          bufferId,
          before.persisted
            .buffers(bufferId)
            .copy(annotations = Annotations(bookmarks = List(CursorPosition(1, 1))))
        )
      )
    )

    val after = bufferAfter(Paste, bookmarked)
    after.document.content.collect() shouldBe "xa\nby\nz"
    after.annotations.bookmarks shouldBe List(CursorPosition(2, 1))
  }

  "Copy and Cut" should "record each copied text in the clipboard history, newest first" in {
    val before = stateWith(
      "alpha beta",
      List(CursorPosition(0, 10)),
      selection = Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10)))
    )
    val copied = resultOf(Copy, before).state
    val cut    = resultOf(Cut, movedTo(copied, List(CursorPosition(0, 0)))).state

    cut.runtime.clipboardHistory.entries shouldBe
      List(ClipboardEntry("alpha beta", wholeLine = true), ClipboardEntry("beta", wholeLine = false))
  }

  "PasteFromHistory" should "paste the picked entry, leaving the clipboard as it was" in {
    val before = stateWith("ab", List(CursorPosition(0, 1)), clipboard = Some("current"))
    val result = resultOf(PasteFromHistory(ClipboardEntry("older", wholeLine = false)), before)

    result.state.persisted.buffers(bufferId).document.content.collect() shouldBe "aolderb"
    result.state.runtime.clipboard shouldBe Some("current")
  }

  it should "paste a whole-line entry above the caret line" in {
    val before = stateWith("ab\ncd", List(CursorPosition(1, 1)))

    bufferAfter(PasteFromHistory(ClipboardEntry("line", wholeLine = true)), before).document.content.collect() shouldBe
      "ab\nline\ncd"
  }

  "CutToDarlings with an active selection" should "remove the selection into darlings, leaving the clipboard alone" in {
    val before = stateWith(
      "alpha beta",
      List(CursorPosition(0, 10)),
      selection = Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10))),
      clipboard = Some("existing")
    )

    val after = bufferAfter(CutToDarlings, before)
    after.document.content.collect() shouldBe "alpha "
    after.annotations.darlings shouldBe List(Darling("beta", CursorPosition(0, 6)))
    clipboardAfter(CutToDarlings, before) shouldBe Some("existing")
  }

  "CutToDarlings without a selection" should "leave the buffer untouched" in {
    val before = stateWith("alpha beta", List(CursorPosition(0, 5)))

    resultOf(CutToDarlings, before).state shouldBe before
  }

  "CutToDarlings with an existing darling" should "prepend the new cut, most recent first" in {
    val existing = Darling("older", CursorPosition(2, 0))
    val before = stateWith(
      "alpha beta",
      List(CursorPosition(0, 10)),
      selection = Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10))),
      darlings = List(existing)
    )

    bufferAfter(CutToDarlings, before).annotations.darlings shouldBe
      List(Darling("beta", CursorPosition(0, 6)), existing)
  }

  "RestoreDarling with nothing cut" should "leave the buffer untouched" in {
    val before = stateWith("alpha", List(CursorPosition(0, 5)))

    resultOf(RestoreDarling, before).state shouldBe before
  }

  "RestoreDarling with a single cursor" should "insert the most recent darling at the cursor and pop it" in {
    val recent = Darling("XY", CursorPosition(4, 0))
    val older  = Darling("Z", CursorPosition(9, 0))
    val before = stateWith("ab", List(CursorPosition(0, 1)), darlings = List(recent, older))

    val after = bufferAfter(RestoreDarling, before)
    after.document.content.collect() shouldBe "aXYb"
    after.editing.cursorPositions shouldBe List(CursorPosition(0, 3))
    after.annotations.darlings shouldBe List(older)
  }

  "RestoreDarling with an active selection" should "replace the selection with the darling's text" in {
    val recent = Darling("XY", CursorPosition(4, 0))
    val before = stateWith(
      "alpha beta",
      List(CursorPosition(0, 10)),
      selection = Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10))),
      darlings = List(recent)
    )

    val after = bufferAfter(RestoreDarling, before)
    after.document.content.collect() shouldBe "alpha XY"
    after.primarySelection shouldBe None
    after.annotations.darlings shouldBe Nil
  }

  "An unrecognised event" should "leave the clipboard and buffer untouched" in {
    val before = stateWith("alpha", List(CursorPosition(0, 0)), clipboard = Some("existing"))
    val buffer = before.persisted.buffers(bufferId)
    val ctx =
      EditorCursorSupport.CursorEventContext(buffer, CursorPosition(0, 0), false, false, before, paneId, None)

    EditorClipboardEventReducer.reduce(NewLine, ctx).state shouldBe before
  }
