package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.state.manager.StateManager
import com.serenity.state.reducers.PanelStateReducer
import com.serenity.ui.layout.{DirectoryTreeData, PanelContent, PanelPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** The command/keyboard resize entry point (issue #1310): `ViewIntent.SetPanelSize` feeds the same
  * `PanelStateReducer.resize` the existing mouse-drag path already uses, via a plain `Command` -- the same generic
  * execution path the command palette itself runs through.
  */
class PanelResizeCommandSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def createStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("PanelResizeCommandSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def resizeCommand(surfaceId: com.serenity.state.models.SurfaceId, delta: Int): Command =
    Command.typed(
      "resize-focused-panel",
      "Resize the focused panel.",
      CommandIntent.View(ViewIntent.SetPanelSize(surfaceId, delta)),
      CommandCategory.View
    )

  "ViewIntent.SetPanelSize" should "grow a pinned panel by the given delta" in {
    val stateManager = createStateManager()
    stateManager.panelManager
      .pinPanel(
        PanelContent.DirectoryTree(DirectoryTreeData(java.nio.file.Paths.get("/tmp")), None),
        PanelPosition.Left,
        24
      )
      .unsafeRunSync()
    val panelId = stateManager.getCurrentState.unsafeRunSync().pinnedSurfaces.head.id

    stateManager.commandExecutor.executeCommand(resizeCommand(panelId, 6)).unsafeRunSync()

    val resized = PanelStateReducer.currentSize(panelId, stateManager.getCurrentState.unsafeRunSync())
    resized shouldBe Some(30)
  }

  it should "shrink a pinned panel by a negative delta" in {
    val stateManager = createStateManager()
    stateManager.panelManager
      .pinPanel(
        PanelContent.DirectoryTree(DirectoryTreeData(java.nio.file.Paths.get("/tmp")), None),
        PanelPosition.Left,
        24
      )
      .unsafeRunSync()
    val panelId = stateManager.getCurrentState.unsafeRunSync().pinnedSurfaces.head.id

    stateManager.commandExecutor.executeCommand(resizeCommand(panelId, -6)).unsafeRunSync()

    val resized = PanelStateReducer.currentSize(panelId, stateManager.getCurrentState.unsafeRunSync())
    resized shouldBe Some(18)
  }

  it should "clamp shrinking at the minimum panel size rather than going to zero or negative" in {
    val stateManager = createStateManager()
    stateManager.panelManager
      .pinPanel(
        PanelContent.DirectoryTree(DirectoryTreeData(java.nio.file.Paths.get("/tmp")), None),
        PanelPosition.Left,
        5
      )
      .unsafeRunSync()
    val panelId = stateManager.getCurrentState.unsafeRunSync().pinnedSurfaces.head.id

    stateManager.commandExecutor.executeCommand(resizeCommand(panelId, -100)).unsafeRunSync()

    // `StateManagerPanelEffects.setPanelSize`'s own floor (`MinimumPanelSize = 4`) would allow 4, but the workspace
    // tree's ratio floor (`WorkspaceTree.MinimumSplitRatio = 0.05`, issue #817) is reached first against the assumed
    // 100-cell total used when no real viewport is known yet -- a stricter, independent floor, not a regression.
    val resized = PanelStateReducer.currentSize(panelId, stateManager.getCurrentState.unsafeRunSync())
    resized shouldBe Some(5)
  }

  it should "no-op when the target surface isn't pinned" in {
    val stateManager = createStateManager()
    val before       = stateManager.getCurrentState.unsafeRunSync()

    stateManager.commandExecutor
      .executeCommand(resizeCommand(com.serenity.state.models.SurfaceId("missing"), 6))
      .unsafeRunSync()

    // issue #1048: every executed command records MRU usage regardless of outcome, so the resize command's own
    // no-op still bumps `runtime.commandUsage` even though the panel state itself is unchanged.
    val expected = before.copy(runtime = before.runtime.copy(commandUsage = Map("resize-focused-panel" -> 1)))
    stateManager.getCurrentState.unsafeRunSync() shouldBe expected
  }
