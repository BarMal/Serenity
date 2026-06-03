package com.serenity

import com.serenity.command.CommandRegistry
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, Focus, PaneId, SurfaceContent}
import com.serenity.state.reducers.{AppEffect, AppEventReducer}
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AppEventReducerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val registry = CommandRegistry.withToggleUI

  "AppEventReducer" should "emit a quit effect without mutating state" in {
    val initialState = AppState.initial

    val result = AppEventReducer.reduce(Quit, initialState, registry)

    result.state shouldBe initialState
    result.effects shouldBe List(AppEffect.CompleteQuit)
  }

  it should "toggle the command runner while preserving previous focus" in {
    val initialState = AppState.initial.copy(focus = Focus.EditorPane(PaneId(0)))

    val opened = AppEventReducer.reduce(ToggleCommandRunner, initialState, registry)
    val openedRunner = opened.state.commandRunnerSurface.flatMap {
      _.content match
        case SurfaceContent.CommandPalette(runner) => Some(runner)
        case _                                     => None
    }

    openedRunner shouldBe defined
    opened.state.focus shouldBe Focus.Surface(opened.state.commandRunnerSurface.get.id)
    openedRunner.get.isActive shouldBe true
    openedRunner.get.previousFocus shouldBe Some(Focus.EditorPane(PaneId(0)))

    val closed = AppEventReducer.reduce(ToggleCommandRunner, opened.state, registry)

    closed.state.focus shouldBe Focus.EditorPane(PaneId(0))
    closed.state.commandRunnerSurface shouldBe None
    closed.effects shouldBe Nil
  }

  it should "create a new buffer and focus it on new tab" in {
    val initialState = AppState.initial

    val result = AppEventReducer.reduce(NewTab, initialState, registry)
    val state  = result.state

    state.buffers should have size 2
    state.bufferOrder shouldBe List(com.serenity.state.models.BufferId(0), com.serenity.state.models.BufferId(1))
    state.focusedBufferId shouldBe Some(com.serenity.state.models.BufferId(1))
    state.buffers(com.serenity.state.models.BufferId(1)).isNewEmpty shouldBe true
    result.effects shouldBe Nil
  }

  it should "navigate to the next and previous buffer according to buffer order" in {
    val stateWithBuffers = AppEventReducer
      .reduce(NewTab, AppState.initial, registry)
      .state
      .copy(viewportSize = Some(ViewportSize(200, 24)))

    val movedBack = AppEventReducer.reduce(PreviousTab, stateWithBuffers, registry).state
    movedBack.focusedBufferId shouldBe Some(com.serenity.state.models.BufferId(0))

    val movedForward = AppEventReducer.reduce(NextTab, movedBack, registry).state
    movedForward.focusedBufferId shouldBe Some(com.serenity.state.models.BufferId(1))
  }
