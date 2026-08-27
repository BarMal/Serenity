package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.{Command, CommandCategory, CommandIntent}
import com.serenity.config.{AppConfig, MotionPreset}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.{PanelContent, PanelPosition, PanelTarget}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class UIHotkeysAndPanelsSpec extends AnyFlatSpec with Matchers:

  given balance: Balance = Balance(weightBalance = 3, heightBalance = 1, leafChunkSize = 30)

  behavior of "UI Hotkeys and Panels"

  // ── Command palette (Ctrl+P → ToggleCommandRunner) ────────────────────────

  it should "open command palette on ToggleCommandRunner" in new UIFixture:
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.commandRunnerSurface shouldBe defined
    state.persisted.focus match
      case Focus.Surface(id) => state.commandRunnerSurface.map(_.id) shouldBe Some(id)
      case _                 => fail("Expected focus on command runner surface")

  it should "dismiss file search when opening command palette" in new UIFixture:
    stateManager.applyEvent(FileSearch).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().fileSearchSurface shouldBe defined

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.fileSearchSurface shouldBe None
    state.commandRunnerSurface shouldBe defined
    state.persisted.focus match
      case Focus.Surface(id) => state.commandRunnerSurface.map(_.id) shouldBe Some(id)
      case _                 => fail("Expected focus on command runner surface")

  it should "dismiss modal workflow when opening command palette" in new UIFixture:
    stateManager.showModal(Modal.GotoLine("12")).unsafeRunSync()
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
    stateManager.showModal(Modal.GotoLine("")).unsafeRunSync()
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

    val positions = pinned.collect { case UiSurface(_, _, SurfacePresentation.Pinned(pos, _), _) => pos }
    positions should contain(PanelPosition.Right)
    positions should contain(PanelPosition.Bottom)

  it should "append pinned panels when a new one occupies the same position" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Right, 30).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val pinned = state.pinnedSurfaces
    pinned should have size 2
    pinned.map(_.content).map {
      case SurfaceContent.Outline(_, _)     => "outline"
      case SurfaceContent.Diagnostics(_, _) => "diagnostics"
      case other                            => fail(s"Unexpected pinned content: $other")
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
    updated.pinnedSurfaces.head.presentation shouldBe SurfacePresentation.Pinned(PanelPosition.Bottom, 30)
    updated.persisted.layout.workspaceTree.map(_.dockedSurfaceIds) shouldBe Some(List(diagnostics.id))

  it should "start an element transition animation when pinning a panel" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Left, 28).unsafeRunSync()

    val state     = stateManager.getCurrentState.unsafeRunSync()
    val panel     = state.pinnedSurfaces.headOption.getOrElse(fail("Expected pinned panel"))
    val animation = state.runtime.surfaceAnimations.get(panel.id).getOrElse(fail("Expected panel animation"))

    animation.animationState.activeAnimationCount should be > 0
    animation.overlayHeight should be > 0

  it should "skip panel transition animation when reduced motion is enabled" in new UIFixture:
    stateManager
      .updateState(state =>
        state.copy(persisted = state.persisted.copy(config = AppConfig.default.withMotionPreset(MotionPreset.Reduced)))
      )
      .unsafeRunSync()

    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Left, 28).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().runtime.surfaceAnimations shouldBe empty

  it should "unpin one same-side panel at a time starting with the focused panel" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.switchToPinnedPanel(PanelTarget.ByPosition(PanelPosition.Right)).unsafeRunSync()

    val before = stateManager.getCurrentState.unsafeRunSync()
    val focusedSurfaceId = before.persisted.focus match
      case Focus.Surface(id) => id
      case other             => fail(s"Expected focus on pinned surface, got $other")

    stateManager.unpinPanel(PanelTarget.ByPosition(PanelPosition.Right)).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after.pinnedSurfaces should have size 1
    after.pinnedSurfaces.map(_.id) should not contain focusedSurfaceId
    after.pinnedSurfaces.map(_.content).foreach {
      case SurfaceContent.Outline(_, _) => ()
      case other                        => fail(s"Unexpected pinned content: $other")
    }

  it should "do nothing when unpinning a surface ID that isn't a pinned panel" in new UIFixture:
    val before = stateManager.getCurrentState.unsafeRunSync()

    stateManager.unpinPanel(PanelTarget.ById(SurfaceId("no-such-surface"))).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync() shouldBe before

  it should "do nothing when unpinning a surface that exists but isn't pinned" in new UIFixture:
    stateManager.applyEvent(FileSearch).unsafeRunSync()
    val floatingSurfaceId = stateManager.getCurrentState
      .unsafeRunSync()
      .fileSearchSurface
      .getOrElse(fail("Expected a floating file-search surface"))
      .id
    val before = stateManager.getCurrentState.unsafeRunSync()

    stateManager.unpinPanel(PanelTarget.ById(floatingSurfaceId)).unsafeRunSync()

    val after = stateManager.getCurrentState.unsafeRunSync()
    after shouldBe before
    after.fileSearchSurface.map(_.id) shouldBe Some(floatingSurfaceId)

  // ── Panel resize ─────────────────────────────────────────────────────────

  it should "create an exiting ghost overlay when unpinning a panel" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    advanceAnimations(80)

    stateManager.unpinPanel(PanelTarget.ByPosition(PanelPosition.Right)).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    val ghost = state.runtime.uiSurfaces.collectFirst {
      case surface @ UiSurface(_, SurfaceContent.GhostOverlay(SurfaceContent.Outline(_, _), _), _, _) => surface
    }
    ghost shouldBe defined
    ghost.flatMap(surface => state.runtime.surfaceAnimations.get(surface.id).map(_.phase)) shouldBe Some(
      SurfacePhase.Exiting
    )

  it should "remove a panel ghost overlay when its close animation completes" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    advanceAnimations(80)
    stateManager.unpinPanel(PanelTarget.ByPosition(PanelPosition.Right)).unsafeRunSync()

    advanceAnimations(120)

    stateManager.getCurrentState.unsafeRunSync().runtime.uiSurfaces.exists {
      _.content match
        case SurfaceContent.GhostOverlay(SurfaceContent.Outline(_, _), _) => true
        case _                                                            => false
    } shouldBe false

  it should "skip panel close ghosts when reduced motion is enabled" in new UIFixture:
    stateManager
      .updateState(state =>
        state.copy(persisted = state.persisted.copy(config = AppConfig.default.withMotionPreset(MotionPreset.Reduced)))
      )
      .unsafeRunSync()
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()

    stateManager.unpinPanel(PanelTarget.ByPosition(PanelPosition.Right)).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.runtime.surfaceAnimations shouldBe empty
    state.runtime.uiSurfaces.exists(_.content.isInstanceOf[SurfaceContent.GhostOverlay]) shouldBe false

  it should "resize a pinned panel to a new size" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.resizePinnedPanel(PanelTarget.ByPosition(PanelPosition.Right), 50).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val pinned = state.pinnedSurfaces
    pinned should have size 1
    pinned.head.presentation match
      case SurfacePresentation.Pinned(PanelPosition.Right, size) => size shouldBe 50
      case other                                                 => fail(s"Expected Pinned(Right, 50), got $other")

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
    stateManager.switchToPinnedPanel(PanelTarget.ByPosition(PanelPosition.Right)).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.persisted.focus match
      case Focus.Surface(id) => state.pinnedSurfaces.map(_.id) should contain(id)
      case other             => fail(s"Expected focus on pinned surface, got $other")

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
      .fileSearchSurface
      .getOrElse(fail("Expected a floating file-search surface"))
      .id
    val before = stateManager.getCurrentState.unsafeRunSync()

    stateManager.switchToPinnedPanel(PanelTarget.ById(floatingSurfaceId)).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync() shouldBe before

  it should "expand and collapse a pinned panel through the panel facade" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.expandPinnedPanel(PanelTarget.ByPosition(PanelPosition.Right)).unsafeRunSync()

    val expanded = stateManager.getCurrentState.unsafeRunSync()
    expanded.expandedPanelSurface.map(_.presentation) shouldBe Some(
      SurfacePresentation.Pinned(PanelPosition.Right, 30)
    )
    expanded.pinnedSurfaces should have size 1
    expanded.persisted.layout.maximizedWorkspaceNodeId shouldBe defined

    stateManager.collapseExpandedPanel().unsafeRunSync()

    val collapsed = stateManager.getCurrentState.unsafeRunSync()
    collapsed.expandedPanelSurface shouldBe None
    collapsed.persisted.layout.maximizedWorkspaceNodeId shouldBe None
    collapsed.pinnedSurfaces.map(_.presentation) shouldBe List(SurfacePresentation.Pinned(PanelPosition.Right, 30))

  it should "do nothing when expanding a surface ID that isn't a pinned panel" in new UIFixture:
    val before = stateManager.getCurrentState.unsafeRunSync()

    stateManager.expandPinnedPanel(PanelTarget.ById(SurfaceId("no-such-surface"))).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync() shouldBe before

  it should "do nothing when expanding a surface that exists but isn't pinned" in new UIFixture:
    stateManager.applyEvent(FileSearch).unsafeRunSync()
    val floatingSurfaceId = stateManager.getCurrentState
      .unsafeRunSync()
      .fileSearchSurface
      .getOrElse(fail("Expected a floating file-search surface"))
      .id
    val before = stateManager.getCurrentState.unsafeRunSync()

    stateManager.expandPinnedPanel(PanelTarget.ById(floatingSurfaceId)).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync() shouldBe before

  it should "expand and collapse a pinned panel through commands" in new UIFixture:
    stateManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Bottom, 10).unsafeRunSync()
    stateManager
      .executeCommand(
        Command.typed(
          "expand-bottom-panel",
          "Expand bottom panel",
          CommandIntent.ExpandPanel(PanelPosition.Bottom),
          CommandCategory.View
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().expandedPanelSurface.map(_.presentation) shouldBe Some(
      SurfacePresentation.Pinned(PanelPosition.Bottom, 10)
    )

    stateManager
      .executeCommand(
        Command.typed(
          "collapse-expanded-panel",
          "Collapse expanded panel",
          CommandIntent.CollapseExpandedPanel,
          CommandCategory.View
        )
      )
      .unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().pinnedSurfaces.map(_.presentation) shouldBe List(
      SurfacePresentation.Pinned(PanelPosition.Bottom, 10)
    )

  // ── Backlog ───────────────────────────────────────────────────────────────

  it should "open file search with Ctrl+Shift+F" in new UIFixture:
    stateManager.applyEvent(FileSearch).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.fileSearchSurface shouldBe defined
    state.persisted.focus match
      case Focus.Surface(id) => state.fileSearchSurface.map(_.id) shouldBe Some(id)
      case _                 => fail("Expected focus on file search surface")

  trait UIFixture:
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))

    val stateManager: StateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

    def advanceAnimations(ticks: Int): Unit =
      (1 to ticks).foreach(_ => stateManager.advanceAnimationsOnTick().unsafeRunSync())
