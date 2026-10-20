package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.lsp.model.{LspPosition, LspRange}
import com.serenity.rope.Rope
import com.serenity.spellcheck.IncrementalSpellFixture
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1960: a replace is an edit like any other, so spell squiggles on the text around the match go with that text
  * (rich-text formatting is covered by `StateManagerReplaceWorkflowSpec`'s formatted-buffer specs).
  */
class ReplaceKeepsSquigglesSpec extends AnyFlatSpec with Matchers with OptionValues:

  import IncrementalSpellFixture.given

  private val surfaceId = SurfaceId("replace-workflow")
  private val bufferId  = BufferId(0)

  private def range(line: Int, from: Int, to: Int): LspRange = LspRange(LspPosition(line, from), LspPosition(line, to))

  private def modelWith(text: String, action: ReplaceWorkflowAction): Model =
    val base = AppState.initial
    val checked = com.serenity.spellcheck.SpellChecker.refreshDiagnostics(
      base.copy(persisted =
        base.persisted.copy(
          buffers = Map(bufferId -> Buffer(bufferId, Document(Rope(text)))),
          bufferOrder = List(bufferId),
          config = AppConfig.default.withSpellCheck(IncrementalSpellFixture.config)
        )
      ),
      IncrementalSpellFixture.snapshot
    )
    val workflow = ReplaceWorkflowState(findText = "cat", replacementText = "tiger", selectedAction = action)
    val surface = UiSurface(
      surfaceId,
      SurfaceContent.ModalWorkflow(Modal.ReplaceWorkflow(workflow)),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    Model(checked.copy(runtime = checked.runtime.copy(uiSurfaces = List(surface))), UndoState())

  private def squiggles(state: AppState): List[LspRange] =
    state.runtime.languageService.diagnosticsState.diagnostics.values.flatten.map(_.range).toList

  private def squigglesAfterReplace(action: ReplaceWorkflowAction): (List[LspRange], List[LspRange]) =
    val before = modelWith("cat teh dog", action)
    val after  = ReplaceWorkflowTransitions.submitted(before, surfaceId).value
    (squiggles(before.app), squiggles(com.serenity.state.models.DiagnosticsEditRemap.carried(before.app, after.app)))

  "Replace-next" should "keep a spell squiggle on the text after the match, moved by the length delta" in {
    val (flagged, carried) = squigglesAfterReplace(ReplaceWorkflowAction.ReplaceNext)

    flagged shouldBe List(range(0, 4, 7))
    carried shouldBe List(range(0, 6, 9))
  }

  "Replace-all" should "keep a spell squiggle on the text after the matches, moved by the length delta" in {
    val (flagged, carried) = squigglesAfterReplace(ReplaceWorkflowAction.ReplaceAll)

    flagged shouldBe List(range(0, 4, 7))
    carried shouldBe List(range(0, 6, 9))
  }
