package com.serenity

import java.nio.file.Paths

import com.serenity.command.CommandRunner
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{LoadFile, OpenFile, SaveAsFile, SaveFile}
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
    val state = stateWithPaneBuffer(buffer, paneId = paneId, focus = Focus.Surface(SurfaceId("command-runner"))).copy(
      uiSurfaces = List(
        UiSurface(
          SurfaceId("command-runner"),
          SurfaceContent.CommandPalette(CommandRunner.empty.activate(com.serenity.command.CommandRegistry.default, AppConfig.default)),
          SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
        )
      )
    )

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
    result.effects shouldBe List(AppEffect.RequestOpenFile())
  }

  it should "emit a request-save-as effect for SaveAsFile" in {
    val buffer = Buffer.fromString(BufferId(5), "content")
    val state  = stateWithPaneBuffer(buffer)

    val result = FileEventReducer.reduce(SaveAsFile, state)

    result.state shouldBe state
    result.effects shouldBe List(AppEffect.RequestSaveAs())
  }

  it should "emit a direct-load-file effect for LoadFile with a path" in {
    val path   = Paths.get("some/file.txt")
    val buffer = Buffer.fromString(BufferId(6), "existing content")
    val state  = stateWithPaneBuffer(buffer)

    val result = FileEventReducer.reduce(LoadFile(path), state)

    result.state shouldBe state
    result.effects shouldBe List(AppEffect.DirectLoadFile(path))
  }
