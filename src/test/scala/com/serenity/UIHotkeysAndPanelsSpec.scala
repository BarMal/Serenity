package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.{PanelContent, PanelPosition}
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
    state.focus match
      case Focus.Surface(id) => state.commandRunnerSurface.map(_.id) shouldBe Some(id)
      case _                 => fail("Expected focus on command runner surface")

  it should "dismiss file search when opening command palette" in new UIFixture:
    stateManager.applyEvent(FileSearch).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().fileSearchSurface shouldBe defined

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.fileSearchSurface shouldBe None
    state.commandRunnerSurface shouldBe defined
    state.focus match
      case Focus.Surface(id) => state.commandRunnerSurface.map(_.id) shouldBe Some(id)
      case _                 => fail("Expected focus on command runner surface")

  it should "dismiss modal workflow when opening command palette" in new UIFixture:
    stateManager.showModal(Modal.GotoLine("12")).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().modalSurface shouldBe defined

    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.modalSurface shouldBe None
    state.commandRunnerSurface shouldBe defined
    state.focus match
      case Focus.Surface(id) => state.commandRunnerSurface.map(_.id) shouldBe Some(id)
      case _                 => fail("Expected focus on command runner surface")

  it should "close command palette on a second ToggleCommandRunner" in new UIFixture:
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.commandRunnerSurface shouldBe None
    state.focus shouldBe a[Focus.EditorPane]

  // ── ESC dismissal ─────────────────────────────────────────────────────────

  it should "dismiss command palette with ESC" in new UIFixture:
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    stateManager.applyEvent(Escape).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.commandRunnerSurface shouldBe None
    state.focus shouldBe a[Focus.EditorPane]

  it should "dismiss a modal overlay with ESC and restore editor focus" in new UIFixture:
    stateManager.showModal(Modal.GotoLine("")).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().modalSurface shouldBe defined

    stateManager.applyEvent(Escape).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.modalSurface shouldBe None
    state.focus shouldBe a[Focus.EditorPane]

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

  it should "replace a pinned panel when a new one occupies the same position" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.pinPanel(PanelContent.Diagnostics(Nil), PanelPosition.Right, 30).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val pinned = state.pinnedSurfaces
    pinned should have size 1
    pinned.head.content shouldBe a[SurfaceContent.Diagnostics]

  // ── Panel resize ─────────────────────────────────────────────────────────

  it should "resize a pinned panel to a new size" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.resizePinnedPanel(PanelPosition.Right, 50).unsafeRunSync()

    val state  = stateManager.getCurrentState.unsafeRunSync()
    val pinned = state.pinnedSurfaces
    pinned should have size 1
    pinned.head.presentation match
      case SurfacePresentation.Pinned(PanelPosition.Right, size) => size shouldBe 50
      case other                                                 => fail(s"Expected Pinned(Right, 50), got $other")

  it should "do nothing when resizing a position with no panel" in new UIFixture:
    stateManager.resizePinnedPanel(PanelPosition.Left, 40).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().pinnedSurfaces shouldBe Nil

  // ── switchToPinnedPanel ───────────────────────────────────────────────────

  it should "move focus to a pinned panel on switchToPinnedPanel" in new UIFixture:
    stateManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 30).unsafeRunSync()
    stateManager.switchToPinnedPanel(PanelPosition.Right).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.focus match
      case Focus.Surface(id) => state.pinnedSurfaces.map(_.id) should contain(id)
      case other             => fail(s"Expected focus on pinned surface, got $other")

  it should "do nothing on switchToPinnedPanel when no panel is at that position" in new UIFixture:
    val focusBefore = stateManager.getCurrentState.unsafeRunSync().focus
    stateManager.switchToPinnedPanel(PanelPosition.Right).unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().focus shouldBe focusBefore

  // ── Backlog ───────────────────────────────────────────────────────────────

  it should "open file search with Ctrl+Shift+F" in new UIFixture:
    stateManager.applyEvent(FileSearch).unsafeRunSync()

    val state = stateManager.getCurrentState.unsafeRunSync()
    state.fileSearchSurface shouldBe defined
    state.focus match
      case Focus.Surface(id) => state.fileSearchSurface.map(_.id) shouldBe Some(id)
      case _                 => fail("Expected focus on file search surface")

  trait UIFixture:
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))

    val stateManager: StateManager = StateManager
      .apply(logger)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()
