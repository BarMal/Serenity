package com.serenity.state.reducers

import java.nio.file.Path

import com.serenity.command.{CommandIntent, CommandRegistry, FileIntent}
import com.serenity.keystroke.events.OpenRecentFolder
import com.serenity.rope.Balance
import com.serenity.state.models.AppState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OpenRecentFolderEventSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Choosing a folder in Open Recent Folder" should "run the command that opens it as the project root" in {
    val folder = Path.of("/work/book")

    val result = AppEventReducer.reduce(OpenRecentFolder(folder), AppState.initial, CommandRegistry.withToggleUI)

    result.state shouldBe AppState.initial
    result.effects.collect { case AppEffect.ExecuteCommand(command) => command.intent } shouldBe
      List(CommandIntent.File(FileIntent.OpenRecentFolder(folder)))
  }
