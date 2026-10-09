package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.unsafe.implicits.global
import com.serenity.command.*
import com.serenity.config.AppMode
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerRecentFilesEffectsSpec extends AnyFlatSpec with Matchers with StateManagerEffectHandlersHarness:

  "StateManagerEffectHandlers" should "forget every recent file for ClearRecentFiles, in each mode's own list too" in {
    val recent = List(Path.of("/notes/a.md"), Path.of("/notes/b.md"))
    val seeded = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        recentFiles = recent,
        recentFilesByMode = Map(AppMode.Prose -> recent)
      )
    )
    val fixture = harness(seeded)

    fixture.handlers
      .interpretCommand(command(CommandIntent.File(FileIntent.ClearRecentFiles)), seeded)
      .unsafeRunSync()

    fixture.currentState.persisted.recentFiles shouldBe Nil
    fixture.currentState.persisted.recentFilesByMode shouldBe Map.empty
    fixture.currentState.persisted.buffers shouldBe seeded.persisted.buffers
  }

  it should "offer ClearRecentFiles as the clear-recent-files command" in {
    CommandRegistry.withToggleUI.findCommand("clear-recent-files").map(_.intent) shouldBe
      Some(CommandIntent.File(FileIntent.ClearRecentFiles))
  }
