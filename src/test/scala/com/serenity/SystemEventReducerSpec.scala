package com.serenity

import java.nio.file.Paths

import com.serenity.keystroke.events.{ExplorerEvent, ResizeEvent, UnhandledEvent}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, SurfaceContent, SurfacePresentation}
import com.serenity.state.reducers.{ReducerResult, SystemEventReducer}
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SystemEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "SystemEventReducer" should "recalculate buffer viewport dimensions on resize" in {
    val initialState = AppState.initial
    val newSize      = ViewportSize(120, 40)

    val ReducerResult(updatedState, effects) =
      SystemEventReducer.reduce(ResizeEvent(newSize), initialState)

    effects shouldBe Nil
    updatedState.viewportSize shouldBe Some(newSize)

    val expectedLayout = LayoutEngine.calculateLayout(updatedState, newSize)
    val bufferId       = updatedState.bufferOrder.head
    val buffer         = updatedState.buffers(bufferId)

    val contentRect = CursorLayout.contentRectForPane(expectedLayout.editorPanelRect)
    buffer.viewport.visibleColumns shouldBe contentRect.width
    buffer.viewport.visibleLines shouldBe contentRect.height
  }

  it should "leave unrelated system events as no-ops" in {
    val initialState = AppState.initial
    val unhandled    = UnhandledEvent(KeyStrokeInfo(InputKey.Unknown, None, Set.empty), new TextEntryTranslator())

    val result = SystemEventReducer.reduce(unhandled, initialState)

    result shouldBe ReducerResult.noEffects(initialState)
  }

  it should "materialize a pinned explorer surface from a root-directory completion event" in {
    val rootPath = Paths.get("/repo")
    val entries = List(
      DirEntry(rootPath.resolve("src"), "src", isDirectory = true),
      DirEntry(rootPath.resolve("build.sbt"), "build.sbt", isDirectory = false)
    )

    val result = SystemEventReducer.reduce(
      ExplorerEvent.RootDirectoryLoaded(
        position = PanelPosition.Left,
        rootPath = rootPath,
        size = 30,
        entries = entries,
        selectedPath = Some(rootPath.resolve("src"))
      ),
      AppState.initial
    )

    result.effects shouldBe Nil
    result.state.pinnedSurfaces should have size 1
    result.state.pinnedSurfaces.head.presentation shouldBe SurfacePresentation.Pinned(PanelPosition.Left, 30)
    result.state.pinnedSurfaces.head.content shouldBe
      SurfaceContent.DirectoryTree(
        com.serenity.ui.layout.DirectoryTreeData(rootPath, entries = Map(rootPath -> entries)),
        Some(rootPath.resolve("src"))
      )
  }

  it should "expand an existing pinned explorer tree from a nested-directory completion event" in {
    val rootPath     = Paths.get("/repo")
    val selectedPath = rootPath.resolve("src")
    val initialEntries = List(
      DirEntry(selectedPath, "src", isDirectory = true)
    )
    val nestedEntries = List(
      DirEntry(selectedPath.resolve("Main.scala"), "Main.scala", isDirectory = false)
    )
    val initialState = AppState.initial.copy(
      uiSurfaces = List(
        com.serenity.state.models.UiSurface.fromPanelContent(
          com.serenity.state.models.SurfaceId("left-panel"),
          com.serenity.ui.layout.PanelContent.DirectoryTree(
            com.serenity.ui.layout.DirectoryTreeData(rootPath, entries = Map(rootPath -> initialEntries)),
            Some(selectedPath)
          ),
          PanelPosition.Left,
          24
        )
      )
    )

    val result = SystemEventReducer.reduce(
      ExplorerEvent.DirectoryLoaded(PanelPosition.Left, selectedPath, nestedEntries),
      initialState
    )

    result.effects shouldBe Nil
    result.state.pinnedSurfaces.head.content shouldBe
      SurfaceContent.DirectoryTree(
        com.serenity.ui.layout.DirectoryTreeData(
          rootPath,
          expandedPaths = Set(selectedPath),
          entries = Map(
            rootPath     -> initialEntries,
            selectedPath -> nestedEntries
          )
        ),
        Some(selectedPath)
      )
  }
