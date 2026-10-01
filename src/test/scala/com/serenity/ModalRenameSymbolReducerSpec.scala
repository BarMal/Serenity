package com.serenity

import com.serenity.keystroke.events.*
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, LspQueueEffect, ModalEventReducer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coverage for the rename-symbol [[TextPrompt]] (#1467): typing the new name and, on submit, queuing the
  * `textDocument/rename` request against the invocation site the prompt was opened with.
  */
class ModalRenameSymbolReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val anchor = CursorPosition(0, 8)

  private val site = RenameSite("file:///tmp/example.scala", LanguageId.Scala, line = 0, character = 8, anchor)

  private def prompt(input: String): Modal = Modal.TextPrompt(TextPrompt.renameSymbol(site, input))

  private def stateWith(modal: Modal): AppState =
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(focus = Focus.Surface(SurfaceId("rename-symbol"))),
      runtime = AppState.initial.runtime.copy(
        uiSurfaces = List(
          UiSurface(
            SurfaceId("rename-symbol"),
            SurfaceContent.ModalWorkflow(modal),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  "ModalEventReducer" should "append characters typed into the rename-symbol prompt" in {
    val updated = ModalEventReducer.reduce(ModalType.TextPrompt, InsertChar('2'), stateWith(prompt("old"))).state

    updated.modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(prompt("old2")))
  }

  it should "delete the last character on backspace" in {
    val updated =
      ModalEventReducer.reduce(ModalType.TextPrompt, DeleteBackward, stateWith(prompt("old"))).state

    updated.modalSurface.map(_.content) shouldBe Some(SurfaceContent.ModalWorkflow(prompt("ol")))
  }

  it should "queue a rename request against the captured invocation site and dismiss on submit" in {
    val result = ModalEventReducer.reduce(ModalType.TextPrompt, Enter, stateWith(prompt("old")))

    result.state.modalSurface shouldBe None
    result.effects shouldBe List(
      AppEffect.LspQueue(
        LspQueueEffect.Enqueue(
          LspEffect.RenameRequested("file:///tmp/example.scala", LanguageId.Scala, 0, 8, anchor, "old")
        )
      )
    )
  }

  it should "dismiss without queuing a request when the new name is left empty" in {
    val result = ModalEventReducer.reduce(ModalType.TextPrompt, Enter, stateWith(prompt("")))

    result.state.modalSurface shouldBe None
    result.effects shouldBe Nil
  }
