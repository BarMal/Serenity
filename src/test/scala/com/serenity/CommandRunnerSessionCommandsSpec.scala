package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.io.FileDialog
import com.serenity.keystroke.events.*
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class CommandRunnerSessionCommandsSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def createStateManager(
    sessionRootOverride: Option[Path] = None,
    configPersistencePath: Option[Path] = None,
    fileDialog: Option[FileDialog] = None
  ): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("CommandRunnerSessionCommandsSpec"))
    StateManager
      .apply(
        logger,
        sessionRootOverride = sessionRootOverride,
        configPersistencePath = configPersistencePath,
        fileDialog = fileDialog
      )
      .unsafeRunSync()

  private def executeCommandThroughRunner(
    stateManager: StateManager,
    searchTerm: String,
    expectedCommandName: String
  ): Unit =
    val beforeOpen = stateManager.getCurrentState.unsafeRunSync()
    if beforeOpen.commandRunnerSurface
          .flatMap {
            _.content match
              case SurfaceContent.CommandPalette(runner) => Some(runner.isActive)
              case _                                     => None
          }
          .getOrElse(false) == false
    then stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()

    searchTerm.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())

    stateManager.getCurrentState.unsafeRunSync().commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => runner.selectedCommand.map(_.name)
        case _                                     => None
    } shouldBe Some(expectedCommandName)

    stateManager.applyEvent(Enter).unsafeRunSync()

  private def assertActiveBufferFitsViewport(state: AppState, viewportSize: ViewportSize): Unit =
    state.runtime.viewportSize shouldBe Some(viewportSize)
    val paneId = state.persisted.layout.activeEditorPaneId.getOrElse(fail("Expected active pane"))
    val bufferId = state.persisted.layout.editorPanes
      .get(paneId)
      .flatMap(_.bufferId)
      .getOrElse(fail("Expected active pane buffer"))
    val layout      = LayoutEngine.calculateLayout(state, viewportSize)
    val contentRect = LayoutEngine.calculateEditorPaneLayouts(state, layout)(paneId).contentRect
    val buffer      = state.persisted.buffers(bufferId)
    buffer.viewport.visibleColumns shouldBe contentRect.width
    buffer.viewport.visibleLines shouldBe contentRect.height

  "Command runner" should "save, restore, and clear the current session from command runner commands" in {
    val sessionRoot  = Files.createTempDirectory("serenity-command-session")
    val stateManager = createStateManager(Some(sessionRoot))
    val bufferId     = BufferId(0)
    val viewportSize = ViewportSize(120, 40)

    stateManager.bufferManager.updateBuffer(bufferId, "saved session").unsafeRunSync()
    stateManager.getCurrentState.unsafeRunSync().persisted.buffers(bufferId).document.isNewEmpty shouldBe false

    executeCommandThroughRunner(stateManager, "save-session", "save-session")
    stateManager.sessionStartupInfo.sessionExists.unsafeRunSync() shouldBe true

    stateManager.paneManager.handleViewportResize(viewportSize).unsafeRunSync()
    stateManager.bufferManager.updateBuffer(bufferId, "changed session").unsafeRunSync()

    executeCommandThroughRunner(stateManager, "restore-session", "restore-session")
    val restoredState = stateManager.getCurrentState.unsafeRunSync()
    restoredState.persisted.buffers(bufferId).document.content.collect() shouldBe "saved session"
    assertActiveBufferFitsViewport(restoredState, viewportSize)

    executeCommandThroughRunner(stateManager, "clear-session", "clear-session")
    stateManager.sessionStartupInfo.sessionExists.unsafeRunSync() shouldBe false
  }

  it should "restore a startup session into the current startup viewport" in {
    val sessionRoot     = Files.createTempDirectory("serenity-startup-session")
    val savedManager    = createStateManager(Some(sessionRoot))
    val restored        = createStateManager(Some(sessionRoot))
    val bufferId        = BufferId(0)
    val startupViewport = ViewportSize(120, 40)
    val savedViewport   = ViewportSize(80, 24)

    savedManager.bufferManager.updateBuffer(bufferId, "startup session").unsafeRunSync()
    executeCommandThroughRunner(savedManager, "save-session", "save-session")
    AppStartup
      .initializeState(restored, restored.sessionStartupInfo, Theme.default, startupViewport)
      .unsafeRunSync()
    restored.applyEvent(MoveDown).unsafeRunSync()
    restored.applyEvent(MoveDown).unsafeRunSync()
    restored.applyEvent(Enter).unsafeRunSync()

    val restoredState = restored.getCurrentState.unsafeRunSync()
    restoredState.persisted.buffers(bufferId).document.content.collect() shouldBe "startup session"
    restoredState.persisted.buffers(bufferId).viewport.visibleColumns should not be savedViewport.width
    restoredState.persisted.buffers(bufferId).viewport.visibleLines should not be savedViewport.height
    assertActiveBufferFitsViewport(restoredState, startupViewport)
  }
