package com.serenity

import java.nio.file.Paths

import com.serenity.keystroke.events.{Direction, PanelInputEvent}
import com.serenity.rope.Balance
import com.serenity.state.components.{ComponentResult, PinnedPanelComponent}
import com.serenity.state.models.*
import com.serenity.state.reducers.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PinnedPanelComponentSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId    = PaneId(0)
  private val surfaceId = SurfaceId("left-panel")

  private def baseState: AppState =
    val bufferId = BufferId(1)
    val buffer   = Buffer.fromString(bufferId, "hello")
    val pane     = EditorPane.withBuffer(paneId, bufferId)
    AppState.initial.copy(
      persisted = AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId)
        ),
        focus = Focus.EditorPane(paneId)
      )
    )

  private def dockedState(content: SurfaceContent): AppState =
    val docked = DockedPanelFixtures.dock(baseState, surfaceId, content, PanelPosition.Left, 24)
    docked.copy(persisted = docked.persisted.copy(focus = Focus.Surface(surfaceId)))

  "PinnedPanelComponent" should "treat pinned ui surfaces as the live source of truth" in {
    val state =
      dockedState(PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/repo")), None).asSurfaceContent)

    val component = PinnedPanelComponent(PanelPosition.Left)

    component
      .processEvent(PanelInputEvent.ReturnFocus, state)
      .shouldBe(ComponentResult.FocusTransfer(Focus.EditorPane(paneId)))
  }

  it should "keep navigation local to the pinned panel" in {
    val state =
      dockedState(PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/repo")), None).asSurfaceContent)

    val component = PinnedPanelComponent(PanelPosition.Left)

    component.processEvent(PanelInputEvent.Navigate(Direction.Down), state).shouldBe(ComponentResult.NoChange)
  }

  it should "move directory selection down within a pinned explorer panel" in {
    val root = Paths.get("/repo")
    val state = dockedState(
      SurfaceContent.DirectoryTree(
        DirectoryTreeData(
          root,
          entries = Map(
            root -> List(
              DirEntry(root.resolve("src"), "src", isDirectory = true),
              DirEntry(root.resolve("build.sbt"), "build.sbt", isDirectory = false)
            )
          )
        ),
        selectedPath = Some(root.resolve("src"))
      )
    )

    val component = PinnedPanelComponent(PanelPosition.Left)

    component.processEvent(PanelInputEvent.Navigate(Direction.Down), state) match
      case ComponentResult.StateChange(update) =>
        val updatedState = update(state)
        updatedState.surfaceById(surfaceId).map(_.content) shouldBe Some(
          SurfaceContent.DirectoryTree(
            DirectoryTreeData(
              root,
              entries = Map(
                root -> List(
                  DirEntry(root.resolve("src"), "src", isDirectory = true),
                  DirEntry(root.resolve("build.sbt"), "build.sbt", isDirectory = false)
                )
              )
            ),
            selectedPath = Some(root.resolve("build.sbt"))
          )
        )
      case other =>
        fail(s"Expected StateChange, got $other")
  }

  it should "emit a direct-load effect when activating a selected file in the explorer panel" in {
    val root         = Paths.get("/repo")
    val selectedFile = root.resolve("build.sbt")
    val state = dockedState(
      SurfaceContent.DirectoryTree(
        DirectoryTreeData(
          root,
          entries = Map(root -> List(DirEntry(selectedFile, "build.sbt", isDirectory = false)))
        ),
        selectedPath = Some(selectedFile)
      )
    )

    val component = PinnedPanelComponent(PanelPosition.Left)

    component.processEvent(PanelInputEvent.Activate, state) shouldBe
      ComponentResult.ReducerUpdate(
        ReducerResult.withEffect(state, AppEffect.File(FileEffect.DirectLoadFile(selectedFile)))
      )
  }

  it should "emit a load-directory effect when activating a selected directory in the explorer panel" in {
    val root        = Paths.get("/repo")
    val selectedDir = root.resolve("src")
    val state = dockedState(
      SurfaceContent.DirectoryTree(
        DirectoryTreeData(
          root,
          entries = Map(root -> List(DirEntry(selectedDir, "src", isDirectory = true)))
        ),
        selectedPath = Some(selectedDir)
      )
    )

    val component = PinnedPanelComponent(PanelPosition.Left)

    component.processEvent(PanelInputEvent.Activate, state) match
      case ComponentResult.ReducerUpdate(result) =>
        result shouldBe ReducerResult.withEffect(
          state,
          AppEffect.Explorer(ExplorerEffect.LoadDirectory(PanelPosition.Left, selectedDir))
        )
      case other =>
        fail(s"Expected ReducerUpdate, got $other")
  }

  it should "collapse an expanded directory when navigating left on that selection" in {
    val root        = Paths.get("/repo")
    val selectedDir = root.resolve("src")
    val state = dockedState(
      SurfaceContent.DirectoryTree(
        DirectoryTreeData(
          root,
          expandedPaths = Set(selectedDir),
          entries = Map(
            root        -> List(DirEntry(selectedDir, "src", isDirectory = true)),
            selectedDir -> List(DirEntry(selectedDir.resolve("Main.scala"), "Main.scala", isDirectory = false))
          )
        ),
        selectedPath = Some(selectedDir)
      )
    )

    val component = PinnedPanelComponent(PanelPosition.Left)

    component.processEvent(PanelInputEvent.Navigate(Direction.Left), state) match
      case ComponentResult.StateChange(update) =>
        val updatedState = update(state)
        updatedState.surfaceById(surfaceId).map(_.content) shouldBe Some(
          SurfaceContent.DirectoryTree(
            DirectoryTreeData(
              root,
              expandedPaths = Set.empty,
              entries = Map(
                root        -> List(DirEntry(selectedDir, "src", isDirectory = true)),
                selectedDir -> List(DirEntry(selectedDir.resolve("Main.scala"), "Main.scala", isDirectory = false))
              )
            ),
            selectedPath = Some(selectedDir)
          )
        )
      case other =>
        fail(s"Expected StateChange, got $other")
  }

  it should "ignore input when no pinned surface exists at the requested position" in {
    val component = PinnedPanelComponent(PanelPosition.Right)

    component.processEvent(PanelInputEvent.Activate, baseState).shouldBe(ComponentResult.NoChange)
  }

  it should "execute a resize command for the panel at this position on a resize keystroke (issue #1310)" in {
    val state =
      dockedState(PanelContent.DirectoryTree(DirectoryTreeData(Paths.get("/repo")), None).asSurfaceContent)
    val component = PinnedPanelComponent(PanelPosition.Left)

    component.processEvent(PanelInputEvent.Resize(1), state) match
      case ComponentResult.ExecuteCommand(command) =>
        command.intent shouldBe com.serenity.command.CommandIntent.View(
          com.serenity.command.ViewIntent.SetPanelSize(surfaceId, 1)
        )
      case other =>
        fail(s"Expected ExecuteCommand, got $other")
  }

  it should "ignore a resize keystroke when no pinned surface exists at the requested position" in {
    val component = PinnedPanelComponent(PanelPosition.Right)

    component.processEvent(PanelInputEvent.Resize(-1), baseState).shouldBe(ComponentResult.NoChange)
  }
end PinnedPanelComponentSpec
