package com.serenity

import java.nio.file.Paths

import com.googlecode.lanterna.screen.TerminalScreen
import com.googlecode.lanterna.terminal.virtual.DefaultVirtualTerminal
import com.googlecode.lanterna.{TerminalSize as LanternaSize}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.Renderer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PinnedPanelRenderingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def screen(width: Int, height: Int): TerminalScreen =
    val terminal = new DefaultVirtualTerminal(new LanternaSize(width, height))
    val screen   = new TerminalScreen(terminal)
    screen.startScreen()
    screen

  private def stateWithPinnedPanel: AppState =
    val buffer = Buffer.fromString(bufferId, "one\ntwo\nthree")
    val pane   = EditorPane.withBuffer(paneId, bufferId)
    val root   = Paths.get("/repo")
    val surface = UiSurface.fromPanelContent(
      SurfaceId("surface-left"),
      PanelContent.DirectoryTree(
        DirectoryTreeData(
          rootPath = root,
          entries = Map(
            root -> List(
              DirEntry(root.resolve("src"), "src", isDirectory = true),
              DirEntry(root.resolve("test"), "test", isDirectory = true)
            )
          )
        ),
        selectedPath = Some(root.resolve("src"))
      ),
      PanelPosition.Left,
      24
    )

    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      uiSurfaces = List(surface),
      focus = Focus.EditorPane(paneId)
    )

  "Renderer.render" should "paint pinned panel content inside the allocated panel rect" in {
    val testScreen = screen(120, 30)
    val state      = stateWithPinnedPanel
    val layout     = LayoutEngine.calculateLayout(state, TerminalSize(120, 30))
    val panelRect  = layout.pinnedPanelRects.getOrElse(PanelPosition.Left, fail("Expected left pinned panel rect"))

    Renderer.render(state, cursorVisible = false, testScreen)

    val titleLine =
      (panelRect.x + 1 until panelRect.right - 1)
        .map(x => testScreen.getBackCharacter(x, panelRect.y).getCharacter)
        .mkString
        .trim
    val contentLine =
      (panelRect.x + 1 until panelRect.right - 1)
        .map(x => testScreen.getBackCharacter(x, panelRect.y + 1).getCharacter)
        .mkString
        .trim

    titleLine should include("repo")
    contentLine should not be empty

    testScreen.stopScreen()
  }
end PinnedPanelRenderingSpec
