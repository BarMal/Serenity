package com.serenity.state.manager

import com.serenity.config.AppConfig
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{Layout, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MouseTargetCacheSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def stateWith(buffer: Buffer, config: AppConfig = AppConfig.default): AppState =
    AppState.initial.copy(
      buffers = Map(buffer.id -> buffer),
      bufferOrder = List(buffer.id),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
        activeEditorPaneId = Some(paneId),
        paneOrder = List(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = config
    )

  "MouseTargetLayoutKey" should "ignore cursor and selection changes during mouse drags" in {
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(cursors = List(CursorPosition(0, 1)))
    val state = stateWith(buffer)
    val draggedState = state.copy(
      buffers = state.buffers.updated(
        bufferId,
        buffer.copy(
          cursors = List(CursorPosition(1, 3)),
          selection = Some(Selection(CursorPosition(0, 1), CursorPosition(1, 3)))
        )
      )
    )

    MouseTargetLayoutKey.from(state, ViewportSize(80, 24)) shouldBe
      MouseTargetLayoutKey.from(draggedState, ViewportSize(80, 24))
  }

  it should "change when layout-affecting content changes with line numbers enabled" in {
    val shortState = stateWith(Buffer.fromString(bufferId, "one"))
    val longState  = stateWith(Buffer.fromString(bufferId, (1 to 100).map(i => s"line $i").mkString("\n")))

    MouseTargetLayoutKey.from(shortState, ViewportSize(80, 24)) should not be
      MouseTargetLayoutKey.from(longState, ViewportSize(80, 24))
  }

  "MouseTargetSnapshotKey" should "ignore cursor and selection changes for the same buffer content" in {
    val buffer = Buffer
      .fromString(bufferId, "alpha beta")
      .copy(language = Some(LanguageId.Markdown), cursors = List(CursorPosition(0, 1)))
    val draggedBuffer = buffer.copy(
      cursors = List(CursorPosition(0, 5)),
      selection = Some(Selection(CursorPosition(0, 1), CursorPosition(0, 5)))
    )
    val fontConfig = FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)

    MouseTargetSnapshotKey.from(buffer, fontConfig, panelWidthPx = 640) shouldBe
      MouseTargetSnapshotKey.from(draggedBuffer, fontConfig, panelWidthPx = 640)
  }

  it should "change when buffer content or font settings change" in {
    val buffer     = Buffer.fromString(bufferId, "alpha beta")
    val changed    = Buffer.fromString(bufferId, "alpha beta gamma")
    val fontConfig = FontConfig(textFontFamily = "SansSerif", fontSize = 12.0f)

    MouseTargetSnapshotKey.from(buffer, fontConfig, panelWidthPx = 640) should not be
      MouseTargetSnapshotKey.from(changed, fontConfig, panelWidthPx = 640)

    MouseTargetSnapshotKey.from(buffer, fontConfig, panelWidthPx = 640) should not be
      MouseTargetSnapshotKey.from(buffer, fontConfig.copy(fontSize = 14.0f), panelWidthPx = 640)
  }
