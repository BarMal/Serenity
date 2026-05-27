package com.serenity

import java.nio.file.Paths

import com.serenity.command.CommandRunner
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UiSurfaceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)

  private def baseState(cursor: CursorPosition = CursorPosition(4, 9)): AppState =
    val buffer = Buffer.fromString(bufferId, "alpha\nbeta\ngamma").copy(
      cursors = List(cursor)
    )
    val pane = EditorPane.withBuffer(paneId, bufferId)

    AppState.initial.copy(
      buffers = Map(bufferId -> buffer),
      bufferOrder = List(bufferId),
      layout = Layout(
        editorPanes = Map(paneId -> pane),
        activeEditorPaneId = Some(paneId)
      ),
      focus = Focus.EditorPane(paneId)
    )

  "AppState" should "store non-editor UI directly as surfaces" in {
    val runner = CommandRunner(
      isActive = true,
      searchTerm = "tog",
      selectedIndex = 0,
      filteredCommands = List.empty
    )
    val surfaces = List(
      UiSurface(
        SurfaceId("peek"),
        SurfaceContent.QuickInfo("map"),
        SurfacePresentation.Floating(Some(CursorPosition(2, 5)), SurfacePlacement.AboveCursor),
        dismissOnMove = true
      ),
      UiSurface(
        SurfaceId("command"),
        SurfaceContent.CommandPalette(runner),
        SurfacePresentation.Floating(Some(CursorPosition(4, 9)), SurfacePlacement.BelowCursor)
      ),
      UiSurface(
        SurfaceId("diagnostics"),
        SurfaceContent.Diagnostics(
          List(Diagnostic("Unused import", DiagnosticSeverity.Warning, Location(2, 1)))
        ),
        SurfacePresentation.Pinned(PanelPosition.Bottom, 8)
      )
    )

    val state = baseState().copy(
      focus = Focus.Surface(SurfaceId("command")),
      uiSurfaces = surfaces
    )

    state.uiSurfaces shouldBe surfaces
    state.focus shouldBe Focus.Surface(SurfaceId("command"))
  }

  it should "derive floating and pinned surface projections from stored surfaces" in {
    val root = Paths.get("/repo")
    val surfaces = List(
      UiSurface(
        SurfaceId("peek"),
        SurfaceContent.DirectoryListing(
          root,
          List(DirEntry(root.resolve("src"), "src", isDirectory = true)),
          Some(root.resolve("src"))
        ),
        SurfacePresentation.Floating(Some(CursorPosition(1, 2)), SurfacePlacement.AboveCursor),
        dismissOnMove = true
      ),
      UiSurface(
        SurfaceId("pinned"),
        SurfaceContent.Diagnostics(
          List(Diagnostic("Unused import", DiagnosticSeverity.Warning, Location(2, 1)))
        ),
        SurfacePresentation.Pinned(PanelPosition.Bottom, 8)
      )
    )

    val state = baseState().copy(uiSurfaces = surfaces)

    state.floatingSurfaces.map(_.id) shouldBe List(SurfaceId("peek"))
    state.pinnedSurfaces.map(_.id) shouldBe List(SurfaceId("pinned"))
  }
end UiSurfaceSpec
