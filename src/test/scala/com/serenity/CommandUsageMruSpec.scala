package com.serenity

import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.state.models.SurfaceContent
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** issue #1048: MRU tracking survives the palette closing and reopening within the same session, since
  * `CommandRunner` itself is reconstructed fresh on every open (`AppEventReducer.openCommandRunner`) -- the
  * generation counter has to live on `AppState.runtime` instead, and get seeded back into the freshly-activated
  * runner.
  */
class CommandUsageMruSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def runnerFrom(sm: com.serenity.state.manager.StateManager): CommandRunner =
    sm.getCurrentState
      .unsafeRunSync()
      .commandRunnerSurface
      .flatMap(_.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None
      )
      .getOrElse(fail("expected the command palette to be open"))

  "executing a command" should "record its use on AppState.runtime, surviving the palette closing" in {
    val sm = createStateManager("CommandUsageMru")

    val command = Command.typed(
      "test-mru-command",
      "A test command for MRU tracking",
      CommandIntent.Edit(EditIntent.Undo),
      label = "Test MRU Command"
    )
    sm.executeCommand(command).unsafeRunSync()

    sm.getCurrentState.unsafeRunSync().runtime.commandUsage should contain key command.name
  }

  it should "seed the freshly-activated palette's own commandUsage on the next open" in {
    val sm = createStateManager("CommandUsageMruReopen")
    val command = Command.typed(
      "test-mru-reopen-command",
      "A test command for MRU tracking across reopen",
      CommandIntent.Edit(EditIntent.Undo),
      label = "Test MRU Reopen Command"
    )
    sm.executeCommand(command).unsafeRunSync()

    // Open, then close, then reopen the palette -- CommandRunner.empty is reconstructed fresh each time.
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()

    runnerFrom(sm).commandUsage should contain key command.name
  }
