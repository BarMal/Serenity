package com.serenity

import java.nio.file.Files

import scala.concurrent.duration.*

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

/** End-to-end reproduction of "fresh start, new file, type content, save, close, reopen, resume the previous session"
  * entirely through the real production pipeline -- the startup-page component, `StateManager.executeCommand` (the same
  * entry point `ComponentResult.executeCommand` uses), and a real on-disk `SessionManager` -- rather than calling
  * `SessionManager` directly the way `SessionManagerSpec` does. Two separate `StateManager` instances sharing one temp
  * session root simulate two separate process launches, since that's exactly the boundary a real quit/reopen crosses.
  */
class SessionResumeIntegrationSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  behavior of "Session resume, end to end through the real startup-page pipeline"

  it should "restore a saved session's content when the user selects Restore on a fresh launch" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val loremIpsum =
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore " +
        "et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut " +
        "aliquip ex ea commodo consequat."

    val program = for
      sessionRoot <- IO.blocking(Files.createTempDirectory("session-resume-integration"))
      theme        = Theme.default
      viewportSize = ViewportSize(80, 24)

      // ---- First launch: fresh start, new document, type content, save session ----
      firstManager <- StateManager.apply(
        testLogger("SessionResumeIntegrationSpec-first"),
        sessionRootOverride = Some(sessionRoot)
      )
      firstInitial <- AppStartup.initializeState(firstManager, theme, viewportSize)
      _ = firstInitial.startPageSurface should be(defined)

      // Select "New document" (index 0, the default selection) from the startup page.
      _        <- firstManager.applyEvent(Enter)
      afterNew <- firstManager.getCurrentState
      _ = afterNew.startPageSurface shouldBe None
      _ = afterNew.persisted.buffers.size shouldBe 1

      // Type the lorem ipsum content, one InsertChar event per character -- the same event the real input
      // pipeline emits per keystroke.
      _           <- loremIpsum.toList.traverse_(c => firstManager.applyEvent(InsertChar(c)))
      afterTyping <- firstManager.getCurrentState
      _ = afterTyping.persisted.buffers.values.head.document.content.toString shouldBe loremIpsum

      // Save the session via the real "Save Session" command -- the same one the command runner/hotkey path
      // dispatches through `StateManager.executeCommand`, not a direct `SessionManager.saveSession` call.
      saveSessionCommand = CommandRegistry.default
        .findCommand("save-session")
        .getOrElse(fail("\"save-session\" command not registered in CommandRegistry.default"))
      _ <- firstManager.executeCommand(saveSessionCommand)

      // ---- Close Serenity: nothing further happens on firstManager, simulating quit. ----

      // ---- Open again: a brand-new StateManager over the same session root, exactly like a fresh process launch. ----
      secondManager <- StateManager.apply(
        testLogger("SessionResumeIntegrationSpec-second"),
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

      // The editor should now be showing the restored content -- the startup page must be gone, not still sitting
      // there unresponsive.
      _ = finalState.startPageSurface shouldBe None
      _ = finalState.persisted.buffers.size shouldBe 1
      _ = finalState.persisted.buffers.values.head.document.content.toString shouldBe loremIpsum
    yield ()

    // Bounded rather than unbounded: a hang in the restore pipeline must fail this test loudly, never wedge the
    // test run itself.
    program.timeout(30.seconds).unsafeRunSync()
  }
