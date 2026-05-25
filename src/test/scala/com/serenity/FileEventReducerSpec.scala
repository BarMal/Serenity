package com.serenity

import java.nio.file.Paths

import com.serenity.keystroke.events.{OpenFile, SaveFile}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, FileEventReducer}
import com.serenity.ui.layout.Layout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FileEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def stateWithPaneBuffer(
    buffer: Buffer,
    paneId: PaneId = PaneId(0),
    focus: Focus = Focus.EditorPane(PaneId(0))
  ): AppState =
    val pane = EditorPane(paneId, Some(buffer.id), Viewport.default, List(CursorPosition(0, 0)), 0)
    AppState.empty.copy(
      buffers = Map(buffer.id -> buffer),
      layout = Layout.empty.copy(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = focus
    )

  "FileEventReducer" should "emit a save-buffer effect for the focused file-backed buffer" in {
    val buffer = Buffer
      .fromString(BufferId(1), "val x = 42")
      .copy(filePath = Some(Paths.get("example.scala")), isDirty = true)
    val state = stateWithPaneBuffer(buffer)

    val result = FileEventReducer.reduce(SaveFile, state)

    result.state shouldBe state
    result.effects shouldBe List(AppEffect.SaveBuffer(buffer.id))
  }

  it should "emit a save-buffer effect for an explicitly targeted pane" in {
    val paneId = PaneId(4)
    val buffer = Buffer
      .fromString(BufferId(2), "val y = 99")
      .copy(filePath = Some(Paths.get("target.scala")), isDirty = true)
    val state = stateWithPaneBuffer(buffer, paneId = paneId, focus = Focus.CommandRunner)

    val result = FileEventReducer.reduceForPane(SaveFile, paneId, state)

    result.state shouldBe state
    result.effects shouldBe List(AppEffect.SaveBuffer(buffer.id))
  }

  it should "emit no save effect when the buffer has no file path" in {
    val buffer = Buffer.fromString(BufferId(3), "scratch").copy(isDirty = true)
    val state  = stateWithPaneBuffer(buffer)

    val result = FileEventReducer.reduce(SaveFile, state)

    result.state shouldBe state
    result.effects shouldBe Nil
  }

  it should "emit an open-file request effect" in {
    val result = FileEventReducer.reduce(OpenFile, AppState.empty)

    result.state shouldBe AppState.empty
    result.effects shouldBe List(AppEffect.RequestOpenFile)
  }

