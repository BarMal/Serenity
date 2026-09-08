package com.serenity

import cats.effect.IO
import com.serenity.command.Command
import com.serenity.state.manager.StateManager

// #1017: CommandExecutor is a capability record (`stateManager.commandExecutor.executeCommand`), not a mixed-in
// trait. Specs with many command-execution call sites (e.g. StateManagerUiPresetSpec, CommandRunnerCoreCommandsSpec)
// share this extension rather than spelling out `.commandExecutor.` at every site.
extension (stateManager: StateManager)
  def executeCommand(command: Command): IO[Unit] = stateManager.commandExecutor.executeCommand(command)
