package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent, CommandRegistry, ViewIntent}
import com.serenity.config.PanelEscapeTarget
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.{PanelContent, PanelPosition, PanelTarget}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class UIHotkeysAndPanelsSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "UI Hotkeys and Panels"

  private def viewCommand(intent: ViewIntent): Command =
    Command.typed("test-view-command", "A test view command.", CommandIntent.View(intent), CommandCategory.View)

  private def registeredCommand(name: String): Command =
    CommandRegistry.default.findCommand(name).getOrElse(fail(s"missing command $name"))

  extension (state: AppState)

    private def searchInOpenFilesSurface: Option[UiSurface] =
      state.runtime.uiSurfaces.find(
        _.content == SurfaceContent.ModalWorkflow(Modal.ListPicker(BufferTextSearch.picker))
      )

  // ── Command palette (Ctrl+P → ToggleCommandRunner) ────────────────────────

  it should "open command palette on ToggleCommandRunner" in new UIFixture:
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.commandRunnerSurface shouldBe defined
    state.persisted.focus match
      case Focus.Surface(id) => state.commandRunnerSurface.map(_.id) shouldBe Some(id)
      case _                 => fail("Expected focus on command runner surface")

  it should "dismiss Search in Open Files when opening command palette" in new UIFixture:
    stateManager.applyEvent(FileSearch).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().searchInOpenFilesSurface shouldBe defined

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.searchInOpenFilesSurface shouldBe None
    state.commandRunnerSurface shouldBe defined
    state.persisted.focus match
      case Focus.Surface(id) => state.commandRunnerSurface.map(_.id) shouldBe Some(id)
      case _                 => fail("Expected focus on command runner surface")

  it should "dismiss modal workflow when opening command palette" in new UIFixture:
    stateManager.applyEvent(OpenGotoLine).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().modalSurface shouldBe defined

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.modalSurface shouldBe None
    state.commandRunnerSurface shouldBe defined
    state.persisted.focus match
      case Focus.Surface(id) => state.commandRunnerSurface.map(_.id) shouldBe Some(id)
      case _                 => fail("Expected focus on command runner surface")

  it should "close command palette on a second ToggleCommandRunner" in new UIFixture:
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.commandRunnerSurface shouldBe None
    state.persisted.focus shouldBe a[Focus.EditorPane]

  // ── ESC dismissal ─────────────────────────────────────────────────────────

  it should "dismiss command palette with ESC" in new UIFixture:
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    stateManager.applyEvent(Escape).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.commandRunnerSurface shouldBe None
    state.persisted.focus shouldBe a[Focus.EditorPane]

  it should "dismiss a modal overlay with ESC and restore editor focus" in new UIFixture:
    stateManager.applyEvent(OpenGotoLine).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().modalSurface shouldBe defined

    stateManager.applyEvent(Escape).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.modalSurface shouldBe None
    state.persisted.focus shouldBe a[Focus.EditorPane]

  // ── Multiple pinned panels ────────────────────────────────────────────────

  it should "support two pinned panels at different positions simultaneously" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Bottom, 10).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val pinned = state.pinnedSurfaces
    pinned should have size 2

    val positions =
      pinned.flatMap(surface => state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id)))
    positions should contain(PanelPosition.Right)
    positions should contain(PanelPosition.Bottom)

  it should "append pinned panels when a new one occupies the same position" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Right, 30).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val pinned = state.pinnedSurfaces
    pinned should have size 2
    pinned.map(_.content).map {
      case SurfaceContent.Outline(_, _, _)     => "outline"
      case SurfaceContent.Diagnostics(_, _, _) => "diagnostics"
      case other                               => fail(s"Unexpected pinned content: $other")
    } shouldBe List("outline", "diagnostics")
    state.persisted.layout.workspaceTree.map(_.dockedSurfaceIds) shouldBe Some(pinned.map(_.id))

  it should "address focus, move, resize, and unpin operations by surface ID" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Right, 30).unsafeRunSync()
    val before      = stateManager.getCurrentState.unsafeRunSync()
    val outline     = before.pinnedSurfaces.head
    val diagnostics = before.pinnedSurfaces.last

    stateManager.switchToPinnedPanel(PanelTarget.ById(outline.id)).unsafeRunSync()
    stateManager.resizePinnedPanel(PanelTarget.ById(outline.id), 20).unsafeRunSync()
    stateManager.movePinnedPanel(outline.id, PanelPosition.Right).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().pinnedSurfaces.map(_.id) shouldBe List(diagnostics.id, outline.id)
    stateManager.movePinnedPanel(diagnostics.id, PanelPosition.Bottom).unsafeRunSync()
    stateManager.unpinPanel(PanelTarget.ById(outline.id)).unsafeRunSync()

    val updated = stateManager.getCurrentState.unsafeRunSync()
    updated.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
    updated.pinnedSurfaces.map(_.id) shouldBe List(diagnostics.id)
    updated.persisted.layout.workspaceTree.flatMap(_.positionForSurface(diagnostics.id)) shouldBe Some(
      PanelPosition.Bottom
    )
    // Outline and diagnostics shared the Right edge's column from line 119 through the move on line 126, and a
    // same-edge column's width is a single tree ratio shared by every panel docked there (issue #817) -- so
    // resizing outline to 20 on line 125 changed diagnostics' rendered width too, not just outline's, before
    // diagnostics ever moved to Bottom on its own.
    com.serenity.state.reducers.PanelStateReducer.currentSize(diagnostics.id, updated) shouldBe Some(20)
    updated.persisted.layout.workspaceTree.map(_.dockedSurfaceIds) shouldBe Some(List(diagnostics.id))

  it should "hide only the toggled panel, leaving another panel on the same edge" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Right, 30).unsafeRunSync()

    stateManager.executeCommand(viewCommand(ViewIntent.TogglePanelShown(PanelId.Diagnostics))).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.pinnedSurfaces should have size 1
    after.pinnedSurfaces.map(_.content).foreach {
      case SurfaceContent.Outline(_, _, _) => ()
      case other                           => fail(s"Unexpected pinned content: $other")
    }

  it should "do nothing when unpinning a surface ID that isn't a pinned panel" in new UIFixture:
    val before = stateManager.getCurrentState.unsafeRunSync()

    stateManager.unpinPanel(PanelTarget.ById(SurfaceId("no-such-surface"))).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync() shouldBe before

  it should "do nothing when unpinning a surface that exists but isn't pinned" in new UIFixture:
    (stateManager.applyEvent(FileSearch) >> stateManager.runtimeLifecycle.awaitEffects).unsafeRunSync()
    val floatingSurfaceId = stateManager.getCurrentState
      .unsafeRunSync()
      .searchInOpenFilesSurface
      .getOrElse(fail("Expected a floating Search in Open Files picker"))
      .id
    val before = stateManager.getCurrentState.unsafeRunSync()

    stateManager.unpinPanel(PanelTarget.ById(floatingSurfaceId)).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after shouldBe before
    after.searchInOpenFilesSurface.map(_.id) shouldBe Some(floatingSurfaceId)

  // ── Panel resize ─────────────────────────────────────────────────────────

  it should "resize a pinned panel to a new size" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.resizePinnedPanel(PanelTarget.ByPosition(PanelPosition.Right), 50).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val pinned = state.pinnedSurfaces
    pinned should have size 1
    com.serenity.state.reducers.PanelStateReducer.currentSize(pinned.head.id, state) shouldBe Some(50)

  it should "do nothing when resizing a position with no panel" in new UIFixture:
    stateManager.resizePinnedPanel(PanelTarget.ByPosition(PanelPosition.Left), 40).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().pinnedSurfaces shouldBe Nil

  it should "do nothing when resizing a surface ID that isn't a pinned panel" in new UIFixture:
    val before = stateManager.getCurrentState.unsafeRunSync()

    stateManager.resizePinnedPanel(PanelTarget.ById(SurfaceId("no-such-surface")), 40).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync() shouldBe before

  // ── switchToPinnedPanel ───────────────────────────────────────────────────

  it should "move focus to a pinned panel on switchToPinnedPanel" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.executeCommand(viewCommand(ViewIntent.FocusPanel(PanelId.Outline))).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.focus match
      case Focus.Surface(id) => state.pinnedSurfaces.map(_.id) should contain(id)
      case other             => fail(s"Expected focus on pinned surface, got $other")

  it should "return Escape to the pane a panel was focused from when set to previous" in new UIFixture:
    stateManager
      .updateState(state =>
        state.copy(persisted =
          state.persisted.copy(config =
            state.persisted.config.withPanelEscapeTarget(state.persisted.config.appMode, PanelEscapeTarget.Previous)
          )
        )
      )
      .unsafeRunSync()
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    val paneFocus = stateManager.getCurrentState.unsafeRunSync().persisted.focus
    stateManager.executeCommand(viewCommand(ViewIntent.FocusPanel(PanelId.Outline))).unsafeRunSync()

    stateManager.applyEvent(PanelInputEvent.Dismiss).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.focus shouldBe paneFocus
    state.persisted.layout.activeEditorPaneId.map(Focus.EditorPane(_)) shouldBe Some(paneFocus)

  it should "move focus to a docked panel and back with the directional focus keys" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Left, 28).unsafeRunSync()
    val editorFocus = stateManager.getCurrentState.unsafeRunSync().persisted.focus

    stateManager.applyEvent(FocusInDirection(com.serenity.keystroke.events.Direction.Left)).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().persisted.focus shouldBe Focus.Surface(PanelId.Outline.surfaceId)

    stateManager.applyEvent(FocusInDirection(com.serenity.keystroke.events.Direction.Right)).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().persisted.focus shouldBe editorFocus

  it should "do nothing on switchToPinnedPanel when no panel is at that position" in new UIFixture:
    val focusBefore = stateManager.getCurrentState.unsafeRunSync().persisted.focus
    stateManager.switchToPinnedPanel(PanelTarget.ByPosition(PanelPosition.Right)).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().persisted.focus shouldBe focusBefore

  it should "do nothing on switchToPinnedPanel when the surface ID isn't a pinned panel" in new UIFixture:
    val before = stateManager.getCurrentState.unsafeRunSync()

    stateManager.switchToPinnedPanel(PanelTarget.ById(SurfaceId("no-such-surface"))).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync() shouldBe before

  it should "do nothing on switchToPinnedPanel when addressing a surface that exists but isn't pinned" in new UIFixture:
    stateManager.applyEvent(FileSearch).unsafeRunSync()
    val floatingSurfaceId = stateManager.getCurrentState
      .unsafeRunSync()
      .searchInOpenFilesSurface
      .getOrElse(fail("Expected a floating Search in Open Files picker"))
      .id
    val before = stateManager.getCurrentState.unsafeRunSync()

    stateManager.switchToPinnedPanel(PanelTarget.ById(floatingSurfaceId)).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync() shouldBe before

  it should "expand and collapse a pinned panel through the panel facade" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.executeCommand(viewCommand(ViewIntent.FocusPanel(PanelId.Outline))).unsafeRunSync()
    stateManager.executeCommand(viewCommand(ViewIntent.ToggleMaximisePanel)).unsafeRunSync()

    val expanded = stateManager.getCurrentState.unsafeRunSync()
    expanded.expandedPanelSurface.map(_.presentation) shouldBe Some(SurfacePresentation.Docked)
    expanded.persisted.layout.workspaceTree.flatMap(
      _.positionForSurface(expanded.expandedPanelSurface.get.id)
    ) shouldBe Some(PanelPosition.Right)
    com.serenity.state.reducers.PanelStateReducer.currentSize(expanded.expandedPanelSurface.get.id, expanded) shouldBe
      Some(30)
    expanded.pinnedSurfaces should have size 1
    expanded.persisted.layout.maximizedWorkspaceNodeId shouldBe defined

    stateManager.executeCommand(viewCommand(ViewIntent.ToggleMaximisePanel)).unsafeRunSync()

    val collapsed = stateManager.getCurrentState.unsafeRunSync()
    collapsed.expandedPanelSurface shouldBe None
    collapsed.persisted.layout.maximizedWorkspaceNodeId shouldBe None
    collapsed.pinnedSurfaces.map(_.presentation) shouldBe List(SurfacePresentation.Docked)

  it should "do nothing when expanding a surface ID that isn't a pinned panel" in new UIFixture:
    val before = stateManager.getCurrentState.unsafeRunSync()

    stateManager.expandPinnedPanel(PanelTarget.ById(SurfaceId("no-such-surface"))).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync() shouldBe before

  it should "do nothing when expanding a surface that exists but isn't pinned" in new UIFixture:
    stateManager.applyEvent(FileSearch).unsafeRunSync()
    val floatingSurfaceId = stateManager.getCurrentState
      .unsafeRunSync()
      .searchInOpenFilesSurface
      .getOrElse(fail("Expected a floating Search in Open Files picker"))
      .id
    val before = stateManager.getCurrentState.unsafeRunSync()

    stateManager.expandPinnedPanel(PanelTarget.ById(floatingSurfaceId)).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync() shouldBe before

  it should "maximise and restore a focused panel through commands" in new UIFixture:
    stateManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Bottom, 10).unsafeRunSync()
    stateManager.executeCommand(registeredCommand("focus-diagnostics-panel")).unsafeRunSync()
    stateManager.executeCommand(registeredCommand("toggle-maximise-panel")).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().expandedPanelSurface.map(_.presentation) shouldBe Some(
      SurfacePresentation.Docked
    )

    stateManager.executeCommand(registeredCommand("toggle-maximise-panel")).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().pinnedSurfaces.map(_.presentation) shouldBe List(
      SurfacePresentation.Docked
    )

  // ── Backlog ───────────────────────────────────────────────────────────────

  it should "open Search in Open Files with Ctrl+Shift+F" in new UIFixture:
    stateManager.applyEvent(FileSearch).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.searchInOpenFilesSurface shouldBe defined
    state.persisted.focus match
      case Focus.Surface(id) => state.searchInOpenFilesSurface.map(_.id) shouldBe Some(id)
      case _                 => fail("Expected focus on the Search in Open Files picker")

  trait UIFixture:
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))

    val stateManager: StateManager = StateManager
      .apply(logger, dictionaryCache = SharedDictionary.default)(using
        com.serenity.rope.Balance.default,
        LoggerFactory[IO]
      )
      .unsafeRunSync()
