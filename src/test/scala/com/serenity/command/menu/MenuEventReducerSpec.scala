package com.serenity.command.menu

import java.nio.file.Paths

import com.serenity.command.{CommandIntent, CommandRegistry, FileIntent}
import com.serenity.keystroke.events.{ActivateBuffer, NewTab, OpenRecentPath}
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId}
import com.serenity.state.reducers.{AppEffect, AppEventReducer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MenuEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.withToggleUI

  private val twoBuffers = AppEventReducer.reduce(NewTab, AppState.initial, registry).state

  "ActivateBuffer" should "focus the chosen buffer without other effects" in {
    twoBuffers.focusedBufferId shouldBe Some(BufferId(1))

    val result = AppEventReducer.reduce(ActivateBuffer(BufferId(0)), twoBuffers, registry)

    result.state.focusedBufferId shouldBe Some(BufferId(0))
    result.effects shouldBe Nil
  }

  it should "leave the state alone when the buffer was closed after the menu was built" in {
    val result = AppEventReducer.reduce(ActivateBuffer(BufferId(42)), twoBuffers, registry)

    result.state shouldBe twoBuffers
    result.effects shouldBe Nil
  }

  "OpenRecentPath" should "execute the open-recent-file command for that path" in {
    val path = Paths.get("/tmp/novel/chapter-one.md")

    val result = AppEventReducer.reduce(OpenRecentPath(path), AppState.initial, registry)

    result.state shouldBe AppState.initial
    result.effects.map {
      case AppEffect.ExecuteCommand(command) => Some(command.intent)
      case _                                 => None
    } shouldBe List(Some(CommandIntent.File(FileIntent.OpenRecentFile(path))))
  }
