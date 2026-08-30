package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, MotionPreset}
import com.serenity.keystroke.Modifier
import com.serenity.keystroke.events.*
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Covers the cursor-peek prototype's settle-in/settle-out animation: reusing the existing command-runner open/close
  * fade choreography (`AnimationChoreography.animatedCommandSurfaces`) for `SurfaceContent.CommandRunnerPeek`
  * surfaces exactly as it already applies to `CommandPalette` ones, rather than inventing a second animation
  * mechanism -- so it automatically respects `commandRunnerAnimation`/`MotionPreset.Reduced` with no new opt-out.
  */
class CursorPeekAnimationSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def createStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("CursorPeekAnimationSpec"))
    StateManager.apply(logger).unsafeRunSync()

  private def enablePeek(sm: StateManager): Unit =
    sm.updateState(state =>
      state.copy(persisted =
        state.persisted.copy(config = state.persisted.config.withCommandRunnerCursorPeekEnabled(true))
      )
    ).unsafeRunSync()

  "a begun peek" should "start a settle-in fade animation on the peek surface" in {
    val sm = createStateManager()
    enablePeek(sm)

    sm.applyEvent(CursorPeekModifierPressed(Modifier.Meta, 0L)).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    state.runtime.uiSurfaces.exists(_.id == SurfaceId.CursorPeek) shouldBe true
    val animState = state.runtime.surfaceAnimations.get(SurfaceId.CursorPeek)
    animState shouldBe defined
    animState.get.phase shouldBe SurfacePhase.Visible
    animState.get.animationState.hasActiveAnimations shouldBe true
  }

  it should "skip the settle-in fade when the command runner animation is disabled" in {
    val sm = createStateManager()
    enablePeek(sm)
    sm.updateState(state =>
      state.copy(persisted = state.persisted.copy(config = state.persisted.config.withCommandRunnerAnimation(None)))
    ).unsafeRunSync()

    sm.applyEvent(CursorPeekModifierPressed(Modifier.Meta, 0L)).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    state.runtime.surfaceAnimations.get(SurfaceId.CursorPeek) shouldBe None
  }

  it should "skip the settle-in fade under a reduced motion preset with zero speed scale" in {
    val sm = createStateManager()
    enablePeek(sm)
    sm.updateState { state =>
      state.copy(persisted =
        state.persisted.copy(config =
          state.persisted.config
            .withMotionPreset(MotionPreset.Smooth)
            .withElementTransitionSpeedScale(0.0)
        )
      )
    }.unsafeRunSync()

    sm.applyEvent(CursorPeekModifierPressed(Modifier.Meta, 0L)).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    state.runtime.surfaceAnimations.get(SurfaceId.CursorPeek) shouldBe None
  }

  "ending a peek" should "start a settle-out fade (a ghost overlay), removing the live peek surface animation" in {
    val sm = createStateManager()
    enablePeek(sm)

    sm.applyEvent(CursorPeekModifierPressed(Modifier.Meta, 0L)).unsafeRunSync()
    sm.applyEvent(CursorPeekModifierReleased(Modifier.Meta, 5L)).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    state.runtime.uiSurfaces.exists(_.id == SurfaceId.CursorPeek) shouldBe false
    state.runtime.surfaceAnimations.get(SurfaceId.CursorPeek) shouldBe None
    val exitingGhost = state.runtime.uiSurfaces.collectFirst {
      case surface @ UiSurface(id, SurfaceContent.GhostOverlay(_: SurfaceContent.CommandRunnerPeek, _), _, _)
          if state.runtime.surfaceAnimations.get(id).exists(_.phase == SurfacePhase.Exiting) =>
        surface
    }
    exitingGhost shouldBe defined
  }

  it should "leave no animation state at all when the settle-in fade was itself disabled" in {
    val sm = createStateManager()
    enablePeek(sm)
    sm.updateState(state =>
      state.copy(persisted = state.persisted.copy(config = state.persisted.config.withCommandRunnerAnimation(None)))
    ).unsafeRunSync()

    sm.applyEvent(CursorPeekModifierPressed(Modifier.Meta, 0L)).unsafeRunSync()
    sm.applyEvent(CursorPeekModifierReleased(Modifier.Meta, 5L)).unsafeRunSync()
    val state = sm.getCurrentState.unsafeRunSync()

    state.runtime.uiSurfaces.exists {
      case UiSurface(_, _: SurfaceContent.GhostOverlay, _, _) => true
      case _                                                  => false
    } shouldBe false
  }
