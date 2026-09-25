package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.command.{Command, CommandIntent, EditIntent}
import com.serenity.keystroke.events.{Enter, InsertChar, TabKey, ToggleCommandRunner}
import com.serenity.state.manager.StateManager
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #1719: the command runner's recents belong to the session they were used in -- they are saved with it, and
  * restoring a session replaces the running recents with that session's own rather than merging the two.
  */
class SessionCommandRecentsSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def undoCommand(name: String): Command =
    Command.typed(name, s"$name for session recents", CommandIntent.Edit(EditIntent.Undo), label = name)

  private val usedInSavedSession = undoCommand("test-recents-saved-session-command")
  private val usedAfterSaving    = undoCommand("test-recents-later-command")

  private def recents(stateManager: StateManager): Map[String, Int] =
    stateManager.getCurrentState.unsafeRunSync().persisted.commandUsage

  private def run(stateManager: StateManager, command: Command): Unit =
    (stateManager.executeCommand(command) >> stateManager.runtimeLifecycle.awaitEffects).unsafeRunSync()

  private def runFromPalette(stateManager: StateManager, commandName: String): Unit =
    stateManager.applyEvent(ToggleCommandRunner).unsafeRunSync()
    commandName.foreach(char => stateManager.applyEvent(InsertChar(char)).unsafeRunSync())
    pressEnter(stateManager)

  private def pressEnter(stateManager: StateManager): Unit =
    (stateManager.applyEvent(Enter) >> stateManager.runtimeLifecycle.awaitEffects).unsafeRunSync()

  private def stateManagerAt(root: Path): StateManager =
    StateManager(testLogger("SessionCommandRecentsSpec"), sessionRootOverride = Some(root)).unsafeRunSync()

  private def currentSessionFile(root: Path): Path =
    root.resolve("sessions").resolve("session.json")

  "Restore Session" should "replace the recents with the saved session's own, dropping commands used since" in {
    val stateManager = createStateManager("SessionCommandRecentsRestore")
    run(stateManager, usedInSavedSession)
    runFromPalette(stateManager, "save-session")
    val savedRecents = recents(stateManager)

    run(stateManager, usedAfterSaving)
    runFromPalette(stateManager, "restore-session")

    savedRecents.keySet should contain(usedInSavedSession.name)
    recents(stateManager) shouldBe savedRecents
  }

  it should "leave no recents when the restored session file predates saving them" in {
    val root         = Files.createTempDirectory("session-command-recents-legacy")
    val stateManager = stateManagerAt(root)
    runFromPalette(stateManager, "save-session")
    val legacyJson = _root_.io.circe.parser
      .parse(Files.readString(currentSessionFile(root)))
      .fold(error => fail(error.getMessage), _.mapObject(_.remove("commandUsage")))
    Files.writeString(currentSessionFile(root), legacyJson.spaces2)

    run(stateManager, usedAfterSaving)
    runFromPalette(stateManager, "restore-session")

    recents(stateManager) shouldBe empty
  }

  "Open Session" should "take the opened session's recents only, not the ones used in the session it replaces" in {
    val root         = Files.createTempDirectory("session-command-recents-named")
    val sessionB     = stateManagerAt(root)
    run(sessionB, usedInSavedSession)
    runFromPalette(sessionB, "save-session-as")
    "B".foreach(char => sessionB.applyEvent(InsertChar(char)).unsafeRunSync())
    pressEnter(sessionB)
    val sessionBRecents = recents(sessionB)

    val sessionA = stateManagerAt(root)
    run(sessionA, usedAfterSaving)
    runFromPalette(sessionA, "open-session")
    pressEnter(sessionA)

    sessionBRecents.keySet should contain(usedInSavedSession.name)
    recents(sessionA) shouldBe sessionBRecents
  }

  "Startup session restore" should "bring back the recents saved by the previous run" in {
    val root     = Files.createTempDirectory("session-command-recents-startup")
    val previous = stateManagerAt(root)
    run(previous, usedInSavedSession)
    runFromPalette(previous, "save-session")
    val savedRecents = recents(previous)

    val next = stateManagerAt(root)
    AppStartup.initializeState(next, next.sessionStartupInfo, Theme.default, ViewportSize(120, 40)).unsafeRunSync()
    next.applyEvent(TabKey).unsafeRunSync()
    next.runtimeLifecycle.awaitEffects.unsafeRunSync()

    savedRecents.keySet should contain(usedInSavedSession.name)
    recents(next) shouldBe savedRecents
  }

  "A new process" should "start with no recents" in {
    recents(createStateManager("SessionCommandRecentsFresh")) shouldBe empty
  }
end SessionCommandRecentsSpec
