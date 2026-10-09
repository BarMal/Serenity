package com.serenity.state.manager

import java.nio.file.Paths

import com.serenity.command.LineEndingCommands
import com.serenity.config.StatusSegment
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.core.EditorState
import com.serenity.state.models.*
import com.serenity.state.reducers.ModalStateReducer
import com.serenity.text.{LineEnding, LineEndingCounts}
import com.serenity.ui.widget.Loadable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1964: choosing the line ending a buffer saves with, and being told when opening a file will lead to a rewrite. */
class LineEndingChoiceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val path  = Paths.get("/notes/draft.txt")
  private val mixed = LineEndingCounts.of("a\r\nb\r\nc\n")

  private def withBuffer(document: Document => Document): (AppState, BufferId) =
    val (opened, bufferId) = EditorState.createNewEmptyBuffer(AppState.initial)
    val buffer             = opened.persisted.buffers(bufferId)
    val updated = opened.copy(persisted =
      opened.persisted.copy(buffers =
        opened.persisted.buffers
          .updated(bufferId, buffer.copy(document = document(buffer.document.copy(filePath = Some(path)))))
      )
    )
    (EditorState.focusBuffer(EditorState.rebalancePanes(updated, Some(bufferId)), bufferId), bufferId)

  private def prompts(state: AppState): List[ConfirmPrompt] =
    state.runtime.modalStack.map(_.modal).collect { case Modal.Confirm(prompt) => prompt } ++
      state.runtime.uiSurfaces.map(_.content).collect { case SurfaceContent.ModalWorkflow(Modal.Confirm(p)) => p }

  private def pickers(state: AppState): List[ListPicker] =
    state.runtime.modalStack.map(_.modal).collect { case Modal.ListPicker(picker) => picker } ++
      state.runtime.uiSurfaces.map(_.content).collect { case SurfaceContent.ModalWorkflow(Modal.ListPicker(p)) => p }

  "Choosing a different line ending" should "save with it and mark the buffer dirty" in {
    val (state, bufferId) = withBuffer(identity)

    val changed = LineEndingChoice.withLineEnding(state, bufferId, LineEnding.Crlf).persisted.buffers(bufferId)

    changed.document.lineEnding shouldBe LineEnding.Crlf
    changed.document.isDirty shouldBe true
  }

  it should "leave a buffer that already saves with it clean" in {
    val (state, bufferId) = withBuffer(identity)

    val unchanged = LineEndingChoice.withLineEnding(state, bufferId, LineEnding.Lf).persisted.buffers(bufferId)

    unchanged.document.isDirty shouldBe false
  }

  it should "settle a mixed file, even on the ending it was already going to be saved with" in {
    val (state, bufferId) = withBuffer(_.copy(lineEnding = LineEnding.Crlf, mixedLineEndings = Some(mixed)))

    val chosen = LineEndingChoice.withLineEnding(state, bufferId, LineEnding.Crlf).persisted.buffers(bufferId)

    chosen.document.mixedLineEndings shouldBe None
    chosen.document.isDirty shouldBe true
  }

  it should "not touch the text" in {
    val (state, bufferId) = withBuffer(_.copy(content = Rope("a\nb")))

    val changed = LineEndingChoice.withLineEnding(state, bufferId, LineEnding.Cr).persisted.buffers(bufferId)

    changed.document.content.collect() shouldBe "a\nb"
    changed.document.contentVersion shouldBe state.persisted.buffers(bufferId).document.contentVersion
  }

  "The line ending picker" should "offer every line ending, marking the current one, each as a set command" in {
    val (state, bufferId) = withBuffer(_.copy(lineEnding = LineEnding.Crlf))

    val opened = LineEndingChoice.withPickerOpened(state).getOrElse(fail("no picker for a focused buffer"))

    val picker = pickers(opened).headOption.getOrElse(fail("no picker shown"))
    picker.title shouldBe "Change Line Ending"
    val choices = picker.items match
      case Loadable.Ready(list) => list.items.toList
      case _                    => Nil
    choices.map(_.label) shouldBe List("LF", "CRLF", "CR")
    choices.filter(_.detail.contains("current")).map(_.label) shouldBe List("CRLF")
    choices.map(_.action) shouldBe LineEnding.values.toList.map(LineEndingCommands.set(bufferId, _))
  }

  private def pending(state: AppState, bufferId: BufferId): AppState =
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updatedWith(bufferId)(
          _.map(buffer =>
            buffer.copy(document =
              buffer.document.copy(
                mixedLineEndings = Some(mixed),
                lineEnding = mixed.dominant,
                mixedNoticePending = true
              )
            )
          )
        )
      )
    )

  "Opening a mixed file" should "say what saving will do and offer the other endings" in {
    val (state, bufferId) = withBuffer(identity)

    val told  = LineEndingChoice.withPendingNotice(pending(state, bufferId))
    val shown = prompts(told)

    shown.map(_.title) shouldBe List("Mixed line endings")
    shown.flatMap(_.message) should contain("This file has 1 LF, 2 CRLF line endings.")
    shown.flatMap(_.message) should contain("Saving will write every line ending as CRLF.")
    shown.flatMap(_.choices.items.toList.map(_.label)) shouldBe List("OK", "Use LF instead", "Use CR instead")
    told.persisted.buffers(bufferId).document.mixedNoticePending shouldBe false
  }

  it should "say nothing for a file with one line ending" in {
    val (state, _) = withBuffer(identity)

    prompts(LineEndingChoice.withPendingNotice(state)) shouldBe Nil
  }

  it should "say it only once" in {
    val (state, bufferId) = withBuffer(identity)
    val told              = LineEndingChoice.withPendingNotice(pending(state, bufferId))

    prompts(LineEndingChoice.withPendingNotice(told.dismissTopModal)) shouldBe Nil
  }

  it should "wait behind a modal and say it once that closes" in {
    val (state, bufferId) = withBuffer(identity)
    val blocked =
      ModalStateReducer.show(Modal.Confirm(ConfirmPrompt.startupNotice("Welcome")), pending(state, bufferId)).state

    val held = LineEndingChoice.withPendingNotice(blocked)

    prompts(held).map(_.title) shouldBe List("Serenity")
    held.persisted.buffers(bufferId).document.mixedNoticePending shouldBe true

    val told = LineEndingChoice.withPendingNotice(held.dismissTopModal)

    prompts(told).map(_.title) shouldBe List("Mixed line endings")
  }

  "Saving a mixed file without choosing an ending" should "say which ending was written and what the file had" in {
    val (state, bufferId) = withBuffer(identity)

    val told = LineEndingChoice.withSavedMixedNotice(state, bufferId, LineEnding.Crlf, mixed)

    prompts(told).flatMap(_.message) should contain(
      "Saved with CRLF line endings; the file had 1 LF, 2 CRLF."
    )
  }

  "The status line" should "show the line ending, and that a mixed file was mixed" in {
    val (clean, _)      = withBuffer(_.copy(lineEnding = LineEnding.Crlf))
    val (mixedState, _) = withBuffer(_.copy(lineEnding = LineEnding.Lf, mixedLineEndings = Some(mixed)))

    StatusLineText.render(clean, List(StatusSegment.LineEnding)) shouldBe Some("CRLF")
    StatusLineText.render(mixedState, List(StatusSegment.LineEnding)) shouldBe Some("LF (file was mixed)")
  }
