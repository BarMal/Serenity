package com.serenity.ui.layout

import com.serenity.config.{AppConfig, WrapColumn}
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The wrap column is for prose: a pane showing code is not set in a column, whatever the workspace's mode. */
class ProseColumnPerPaneSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val proseBuffer = Buffer.fromString(BufferId(0), "Once upon a time, in a land far away.")
  private val codeBuffer =
    val plain = Buffer.fromString(BufferId(1), "val answer = 42")
    plain.copy(document = plain.document.copy(language = Some(LanguageId.Scala)))

  private val prosePane = PaneId(0)
  private val codePane  = PaneId(1)

  private def stateWithBothPanes: AppState =
    val base = AppState.initial(AppConfig.default.withWrapColumn(WrapColumn.Cells(66)))
    base.copy(persisted =
      base.persisted.copy(
        buffers = Map(proseBuffer.id -> proseBuffer, codeBuffer.id -> codeBuffer),
        bufferOrder = List(proseBuffer.id, codeBuffer.id),
        layout = base.persisted.layout.copy(
          editorPanes = Map(
            prosePane -> EditorPane.withBuffer(prosePane, proseBuffer.id),
            codePane  -> EditorPane.withBuffer(codePane, codeBuffer.id)
          ),
          activeEditorPaneId = Some(prosePane)
        )
      )
    )

  "A pane showing prose" should "keep the wrap column" in {
    ProseColumn.widthCellsFor(stateWithBothPanes, prosePane) shouldBe Some(66)
  }

  "A pane showing code" should "not be capped to the wrap column" in {
    ProseColumn.widthCellsFor(stateWithBothPanes, codePane) shouldBe None
  }

  "A pane with no buffer" should "follow the workspace" in {
    ProseColumn.widthCellsFor(stateWithBothPanes, PaneId(9)) shouldBe Some(66)
  }

  "The text area of a code pane" should "run the full width the column holds a prose pane to" in {
    val state  = stateWithBothPanes
    val insets = state.persisted.config.surfaceConfig.textAreaInsets

    val proseText = PaneTextArea.bands(state, prosePane, 0, 160, insets).text.width
    val codeText  = PaneTextArea.bands(state, codePane, 0, 160, insets).text.width

    proseText should be <= 66
    codeText should be > 66
  }
