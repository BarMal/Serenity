package com.serenity

import com.serenity.command.{CommandRegistry, CommandRunner}
import com.serenity.config.{AppConfig, CursorInfoBarMode}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{SurfaceContentResolver, SurfaceRenderMode}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CursorInfoBarSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def editorState(
    cursor: CursorPosition = CursorPosition(0, 0),
    config: AppConfig = AppConfig.default.withCursorInfoBarMode(CursorInfoBarMode.Detailed)
  ): AppState =
    val buffer = Buffer
      .fromString(bufferId, "alpha\nbeta\ngamma")
      .copy(cursors = List(cursor))
    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.EditorPane(paneId),
      config = config
    )

  "AppState.cursorInfoBarSurface" should "derive a below-cursor surface from the active editor cursor" in {
    val state = editorState(CursorPosition(1, 2))

    val surface = state.cursorInfoBarSurface.getOrElse(fail("Expected cursor info bar surface"))

    surface.id shouldBe SurfaceId("cursor-info-bar")
    surface.content shouldBe SurfaceContent.CursorInfoBar("Line 2, Col 3 | Plain Text | Unsaved")
    surface.presentation shouldBe SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.BelowCursor)
  }

  it should "be disabled when the config mode is off" in {
    val state = editorState(config = AppConfig.default.withCursorInfoBarMode(CursorInfoBarMode.Off))

    state.cursorInfoBarSurface shouldBe None
  }

  "LayoutEngine.calculateLayout" should "place the cursor info bar below the active cursor" in {
    val layout = LayoutEngine.calculateLayout(editorState(), ViewportSize(80, 24))

    layout.belowCursorOverlayStack.map(_._1) should contain(SurfaceId("cursor-info-bar"))
  }

  it should "hide the cursor info bar behind command runner overlays" in {
    val runner = CommandRunner.empty.activate(CommandRegistry.default, AppConfig.default)
    val commandRunnerSurface = UiSurface(
      SurfaceId("command-runner"),
      SurfaceContent.CommandPalette(runner),
      SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
    )
    val state = editorState().copy(uiSurfaces = List(commandRunnerSurface))

    val layout = LayoutEngine.calculateLayout(state, ViewportSize(80, 24))

    layout.belowCursorOverlayStack.map(_._1) shouldBe List(SurfaceId("command-runner"))
  }

  "SurfaceContentResolver" should "render cursor info bar text as a single overlay row" in {
    val resolved = SurfaceContentResolver.resolve(
      SurfaceContent.CursorInfoBar("Line 1, Col 1"),
      LayoutRect(0, 0, 40, 3),
      SurfaceRenderMode.Floating
    )

    resolved.rows.map(_.plainText) shouldBe List("Line 1, Col 1")
  }
