package com.serenity

import java.nio.file.Files

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.app.AppStartup
import com.serenity.command.CommandRegistry
import com.serenity.keystroke.events.*
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

class StartupPageIntegrationSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  behavior of "Startup Page Integration"

  it should "allow navigation through startup options and execute selection" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupPageIntegrationSpec")
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      // Initialize startup state
      initialState <- AppStartup.initializeState(stateManager, theme, viewportSize)

      // Verify we start with startup page focused
      _         = initialState.persisted.focus shouldBe Focus.Surface(SurfaceId("surface-0"))
      _         = initialState.startPageSurface should be(defined)
      startPage = initialState.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _         = startPage.selectedIndex shouldBe 0

      // Navigate down one option
      _      <- stateManager.applyEvent(MoveDown)
      state1 <- stateManager.getCurrentState
      startPage1 = state1.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage1.selectedIndex shouldBe 1

      // Navigate down again
      _      <- stateManager.applyEvent(MoveDown)
      state2 <- stateManager.getCurrentState
      startPage2 = state2.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage2.selectedIndex shouldBe 2

      // Navigate through the workflow actions.
      _      <- stateManager.applyEvent(MoveDown)
      state3 <- stateManager.getCurrentState
      startPage3 = state3.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage3.selectedIndex shouldBe 3

      _      <- stateManager.applyEvent(MoveDown)
      state4 <- stateManager.getCurrentState
      startPage4 = state4.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage4.selectedIndex shouldBe 4

      // Navigate down once more (should wrap to the first option)
      _      <- stateManager.applyEvent(MoveDown)
      state5 <- stateManager.getCurrentState
      startPage5 = state5.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage5.selectedIndex shouldBe 0

      // Navigate up (should wrap to last option)
      _      <- stateManager.applyEvent(MoveUp)
      state6 <- stateManager.getCurrentState
      startPage6 = state6.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      _          = startPage6.selectedIndex shouldBe 4

      // Reset to first option and select it (new session)
      _          <- stateManager.applyEvent(MoveDown)
      _          <- stateManager.applyEvent(Enter)
      finalState <- stateManager.getCurrentState

      // After selecting "new session", startup page should be dismissed and we should have editor
      _ = finalState.startPageSurface shouldBe None
      _ = finalState.persisted.layout.editorPanes.size shouldBe 1
      _ = finalState.persisted.buffers.size shouldBe 1
    yield ()

    program.unsafeRunSync()
  }

  it should "restore into a usable buffer, never a blank screen, when the saved session has zero buffers" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      sessionRoot <- IO.blocking(Files.createTempDirectory("startup-page-empty-session-restore"))
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      // ---- First launch: fresh start, close everything without opening anything, save session. ----
      firstManager <- StateManager.apply(
        testLogger("StartupPageIntegrationSpec-empty-session-first"),
        sessionRootOverride = Some(sessionRoot)
      )
      firstInitial <- AppStartup.initializeState(firstManager, theme, viewportSize)
      _ = firstInitial.startPageSurface should be(defined)
      _ = firstInitial.persisted.buffers shouldBe empty

      // Save the session via the real "Save Session" command while zero buffers exist -- exactly what happens
      // when a user closes every tab before quitting.
      saveSessionCommand = CommandRegistry.default
        .findCommand("save-session")
        .getOrElse(fail("\"save-session\" command not registered in CommandRegistry.default"))
      _ <- firstManager.executeCommand(saveSessionCommand)

      // ---- Open again: a brand-new StateManager over the same session root, exactly like a fresh process launch. ----
      secondManager <- StateManager.apply(
        testLogger("StartupPageIntegrationSpec-empty-session-second"),
        sessionRootOverride = Some(sessionRoot)
      )
      secondInitial <- AppStartup.initializeState(secondManager, theme, viewportSize)
      _            = secondInitial.startPageSurface should be(defined)
      startPage    = secondInitial.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      restoreIndex = startPage.launchActions.indexWhere(_.id == "restore-session")
      _ = withClue("\"Restore previous session\" action should be offered once a session exists on disk") {
        restoreIndex should be >= 0
      }

      // Navigate to "Restore previous session" and press Enter, exactly as the user does.
      _          <- (0 until restoreIndex).toList.traverse_(_ => secondManager.applyEvent(MoveDown))
      _          <- secondManager.applyEvent(Enter)
      finalState <- secondManager.getCurrentState

      // A zero-buffer restore must never leave a blank, unusable screen: the startup page must be gone and at
      // least one buffer/pane must exist so the user has somewhere to type.
      _ = finalState.startPageSurface shouldBe None
      _ = finalState.persisted.buffers should not be empty
      _ = finalState.persisted.bufferOrder should not be empty
      _ = finalState.persisted.layout.editorPanes should not be empty
    yield ()

    program.unsafeRunSync()
  }

  it should "dismiss the startup page (not leave it masking a hidden editor) when a recent file is opened" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      sessionRoot <- IO.blocking(Files.createTempDirectory("startup-page-open-recent"))
      recentFile  <- IO.blocking(Files.createTempFile("startup-recent", ".md"))
      _           <- IO.blocking(Files.writeString(recentFile, "recent file body"))
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      // ---- First launch: open the file so it is tracked as a recent file, then save the session. ----
      firstManager <- StateManager.apply(
        testLogger("StartupPageIntegrationSpec-open-recent-first"),
        sessionRootOverride = Some(sessionRoot)
      )
      _ <- AppStartup.initializeState(firstManager, theme, viewportSize)
      openRecentSeed = com.serenity.command.Command.typed(
        "open-recent-seed",
        "Open the file so it is tracked as recent",
        com.serenity.command.CommandIntent.File(com.serenity.command.FileIntent.OpenRecentFile(recentFile))
      )
      _ <- firstManager.executeCommand(openRecentSeed)
      saveSessionCommand = CommandRegistry.default
        .findCommand("save-session")
        .getOrElse(fail("\"save-session\" command not registered in CommandRegistry.default"))
      _ <- firstManager.executeCommand(saveSessionCommand)

      // ---- Open again: a brand-new StateManager, exactly like a fresh process launch. The saved file is now
      // offered as a "recent" entry on the startup page (the same entry the user selects). ----
      secondManager <- StateManager.apply(
        testLogger("StartupPageIntegrationSpec-open-recent-second"),
        sessionRootOverride = Some(sessionRoot)
      )
      secondInitial <- AppStartup.initializeState(secondManager, theme, viewportSize)
      _         = secondInitial.startPageSurface should be(defined)
      startPage = secondInitial.startPageSurface.get.content.asInstanceOf[SurfaceContent.StartPage].page
      recentIndex = startPage.launchActions.indexWhere(_.id.startsWith("recent:"))
      _ = withClue("the just-opened file should be offered as a recent action on the startup page") {
        recentIndex should be >= 0
      }

      // Navigate to the recent-file entry and press Enter, exactly as the user does.
      _         <- (0 until recentIndex).toList.traverse_(_ => secondManager.applyEvent(MoveDown))
      _         <- secondManager.applyEvent(Enter)
      afterOpen <- secondManager.getCurrentState

      // The bug: opening a recent file left the StartPage surface in place, so Renderer's `startPageSurface`
      // short-circuit kept drawing the splash over the editor -- keystrokes reached the hidden buffer but nothing
      // repainted, so the whole app appeared frozen. The startup page must be gone after opening a recent file.
      _ = withClue("startup page must be dismissed after opening a recent file, not left masking the editor") {
        afterOpen.startPageSurface shouldBe None
      }
      _ = withClue("the recent file's content should be loaded into a buffer") {
        afterOpen.persisted.buffers.values.exists(_.document.content.toString.contains("recent file body")) shouldBe true
      }

      // And input must now reach the editor and change state -- the visible symptom of the fix.
      _         <- secondManager.applyEvent(InsertChar('Z'))
      afterType <- secondManager.getCurrentState
      _ = withClue("typing after opening a recent file must modify the editor buffer") {
        afterType.persisted.buffers.values.exists(_.document.content.toString.contains("Z")) shouldBe true
      }
    yield ()

    program.unsafeRunSync()
  }

  it should "dismiss startup page on Escape" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val program = for
      stateManager <- createStateManagerIO("StartupPageIntegrationSpec")
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      // Initialize startup state
      initialState <- AppStartup.initializeState(stateManager, theme, viewportSize)
      _ = initialState.startPageSurface should be(defined)

      // Press escape to dismiss
      _          <- stateManager.applyEvent(Escape)
      finalState <- stateManager.getCurrentState

      // Startup page should be dismissed
      _ = finalState.startPageSurface shouldBe None
    yield ()

    program.unsafeRunSync()
  }
