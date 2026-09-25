package com.serenity.state.manager

import scala.compiletime.testing.typeChecks

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Deliberately does not import `StateManagerTestFacade`: these operations must stay reachable only through it. */
class StateManagerProductionFacadeSpec extends AnyFlatSpec with Matchers:

  "StateManager" should "keep operations only specs drive out of the production façade (#1692)" in {
    // A control: the snippets below must fail only for the missing member, not for how they are written.
    typeChecks("(sm: StateManager) => sm.fileService.checkExternalChangesOnFocus") shouldBe true

    typeChecks("(sm: StateManager) => sm.updateState(identity)") shouldBe false
    typeChecks("(sm: StateManager) => sm.getBufferAnimations") shouldBe false
    typeChecks("(sm: StateManager) => sm.updateBufferAnimations(identity)") shouldBe false
    typeChecks("(sm: StateManager) => sm.sessionService.saveSession") shouldBe false
    typeChecks("(sm: StateManager) => sm.sessionService.clearSession") shouldBe false
    typeChecks("(sm: StateManager) => sm.fileService.setBufferFilePath") shouldBe false
    typeChecks("(sm: StateManager) => sm.fileService.markBufferSaved") shouldBe false
    typeChecks("(sm: StateManager) => sm.fileService.checkUnsavedChanges") shouldBe false
    typeChecks("(sm: StateManager) => sm.fileService.getRecentFiles") shouldBe false
    // #1724: `commandExecutor` is gone outright, not merely reachable-only-via-the-facade -- running an arbitrary
    // `Command` needs the composition's `interpretCommand` wiring, so its replacement (`StateManagerTestFacade`'s
    // `executeCommand`) reaches a `private[manager]` trait member instead of reimplementing the effect here.
    typeChecks("(sm: StateManager) => sm.commandExecutor") shouldBe false
  }
