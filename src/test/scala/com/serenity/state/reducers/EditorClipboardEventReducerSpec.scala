package com.serenity.state.reducers

import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Dedicated coverage for `EditorClipboardEventReducer` (#1442): Copy/Cut/Paste, the family that reads or writes the
  * clipboard alongside the buffer. Each of the three independently computes a clipboard string alongside its buffer
  * update; this suite exercises the with-selection and without-selection (whole-line) shape of each, plus
  * multi-cursor paste and the "nothing on the clipboard" no-op.
  */
class EditorClipboardEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val paneId   = PaneId(0)

  private def stateWith(
    text: String,
    cursors: List[CursorPosition],
    selection: Option[Selection] = None,
    clipboard: Option[String] = None
  ): AppState =
    val buffer = Buffer
      .fromString(bufferId, text)
      .copy(editing = EditingState(cursors = cursors, selection = selection))
    val base = AppState.initial.copy(persisted = AppState.initial.persisted.copy(buffers = Map(bufferId -> buffer)))
    base.copy(runtime = base.runtime.copy(clipboard = clipboard))

  private def resultOf(event: TextEntryEvent, state: AppState): ReducerResult =
    EditorEventReducer.reduce(event, paneId, state)

  private def bufferAfter(event: TextEntryEvent, state: AppState): Buffer =
    resultOf(event, state).state.persisted.buffers(bufferId)

  private def clipboardAfter(event: TextEntryEvent, state: AppState): Option[String] =
    resultOf(event, state).state.runtime.clipboard

  "Copy with an active selection" should "put the selected text on the clipboard, leaving the buffer unchanged" in {
    val before = stateWith(
      "alpha beta",
      List(CursorPosition(0, 10)),
      selection = Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10)))
    )

    clipboardAfter(Copy, before) shouldBe Some("beta")
    bufferAfter(Copy, before).document.content.collect() shouldBe "alpha beta"
    bufferAfter(Copy, before).editing.selection shouldBe Some(Selection(CursorPosition(0, 6), CursorPosition(0, 10)))
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
    bufferAfter(Cut, before).editing.selection shouldBe None
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
    after.editing.cursors shouldBe List(CursorPosition(0, 3))
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
    after.editing.selection shouldBe None
  }

  "Paste with multiple cursors" should "insert the clipboard text at every cursor independently" in {
    val before =
      stateWith("a\nb\nc", List(CursorPosition(0, 1), CursorPosition(1, 1), CursorPosition(2, 1)), clipboard = Some("X"))

    val after = bufferAfter(Paste, before)
    after.document.content.collect() shouldBe "aX\nbX\ncX"
  }

  "An unrecognised event" should "leave the clipboard and buffer untouched" in {
    val before = stateWith("alpha", List(CursorPosition(0, 0)), clipboard = Some("existing"))
    val buffer = before.persisted.buffers(bufferId)
    val ctx =
      EditorCursorSupport.CursorEventContext(buffer, CursorPosition(0, 0), false, false, before, paneId)

    EditorClipboardEventReducer.reduce(NewLine, ctx).state shouldBe before
  }
