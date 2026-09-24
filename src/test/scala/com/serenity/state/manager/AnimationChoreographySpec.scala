package com.serenity.state.manager

import com.serenity.animation.{AnimationOwner, AnimationState, SweepDirection}
import com.serenity.command.CommandRegistry
import com.serenity.config.AppConfigMotionOps.*
import com.serenity.config.{AppConfig, MotionAccessibility}
import com.serenity.keystroke.events.ToggleCommandRunner
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEventReducer, PanelStateReducer}
import com.serenity.ui.layout.{PanelContent, PanelPosition, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Pure surface/pane animation choreography (#1697 wave 2): the open/close hooks and the tab-cycling pane sweep as
  * plain functions of the states around an event, each committed result checked against `AppStateValidation` because
  * these used to be unvalidated `stateRef.update` writes (#1183).
  */
class AnimationChoreographySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)

  private def baseState(config: AppConfig): AppState =
    val initial = AppState.initial(config)
    initial.copy(
      persisted = initial.persisted
        .copy(buffers = initial.persisted.buffers.updated(bufferId, Buffer.fromString(bufferId, "hello\nworld"))),
      runtime = initial.runtime.copy(viewportSize = Some(ViewportSize(120, 40)))
    )

  private val animated = baseState(AppConfig.withTestAnimations)
  private val still    = baseState(AppConfig.withTestAnimations.withCommandRunnerAnimation(None))

  private def toggleCommandRunner(state: AppState): AppState =
    AppEventReducer.reduce(ToggleCommandRunner, state, CommandRegistry.default).state

  private def pinOutline(state: AppState): AppState =
    PanelStateReducer.pin(PanelContent.Outline(Nil), PanelPosition.Left, 24, state).state

  private def isValid(state: AppState): Boolean = AppStateValidation.validated(state).isRight

  private def ghostIds(state: AppState): List[SurfaceId] =
    state.runtime.uiSurfaces.collect { case UiSurface(id, SurfaceContent.GhostOverlay(_, _), _, _) => id }

  "animateSurfaceTransitions" should "leave the state alone when no surface opened or closed" in {
    AnimationChoreography.animateSurfaceTransitions(animated, animated) shouldBe None
  }

  it should "seed a Visible fade for an opened command runner and still validate" in {
    val opened = toggleCommandRunner(animated)
    val next   = AnimationChoreography.animateSurfaceTransitions(animated, opened)

    val runnerId = opened.commandRunnerSurface.map(_.id)
    runnerId shouldBe defined
    next.flatMap(state => runnerId.flatMap(state.runtime.surfaceAnimations.get)).map(_.phase) shouldBe
      Some(SurfacePhase.Visible)
    next.forall(isValid) shouldBe true
  }

  it should "drop any fade for an opened command runner when its animation is off and still validate" in {
    val opened   = toggleCommandRunner(still)
    val runnerId = opened.commandRunnerSurface.map(_.id)
    val stale = runnerId.fold(opened)(id =>
      opened.copy(runtime =
        opened.runtime.copy(surfaceAnimations =
          opened.runtime.surfaceAnimations + (id -> SurfaceAnimationState(
            SurfacePhase.Visible,
            AnimationState.empty,
            overlayHeight = 1,
            bufferFadeLength = 0,
            phaseTick = 0
          ))
        )
      )
    )

    val next = AnimationChoreography.animateSurfaceTransitions(still, stale)

    next shouldBe defined
    next.map(_.runtime.surfaceAnimations.keySet.intersect(runnerId.toSet)) shouldBe Some(Set.empty)
    next.forall(isValid) shouldBe true
  }

  it should "leave an Exiting ghost behind a closed command runner and still validate" in {
    val open   = AnimationChoreography.animateSurfaceTransitions(animated, toggleCommandRunner(animated))
    val before = open.getOrElse(fail("expected the open animation to apply"))
    val closed = toggleCommandRunner(before)

    val next = AnimationChoreography.animateSurfaceTransitions(before, closed)

    val ghosts = next.map(ghostIds).getOrElse(Nil)
    ghosts should have size 1
    next.map(state => ghosts.flatMap(state.runtime.surfaceAnimations.get).map(_.phase)) shouldBe
      Some(List(SurfacePhase.Exiting))
    next.forall(isValid) shouldBe true
  }

  it should "close a command runner without a ghost when its animation is off and still validate" in {
    val opened = toggleCommandRunner(still)
    val closed = toggleCommandRunner(opened)

    val next = AnimationChoreography.animateSurfaceTransitions(opened, closed)

    next shouldBe defined
    next.map(ghostIds) shouldBe Some(Nil)
    next.forall(isValid) shouldBe true
  }

  it should "seed an opening pinned panel's fade or geometry and still validate" in {
    val pinned  = pinOutline(animated)
    val panelId = pinned.pinnedSurfaces.map(_.id).filterNot(animated.pinnedSurfaces.map(_.id).contains)

    val next = AnimationChoreography.animateSurfaceTransitions(animated, pinned)

    panelId should have size 1
    next.exists(state =>
      panelId.forall(id => state.runtime.surfaceAnimations.contains(id) || state.runtime.panelGeometry.contains(id))
    ) shouldBe true
    next.forall(isValid) shouldBe true
  }

  it should "leave a ghost behind an unpinned panel and still validate" in {
    val pinned   = pinOutline(animated)
    val unpinned = PanelStateReducer.unpin(PanelPosition.Left, pinned).state

    val next = AnimationChoreography.animateSurfaceTransitions(pinned, unpinned)

    next.map(ghostIds).getOrElse(Nil) should have size 1
    next.forall(isValid) shouldBe true
  }

  "withPaneFlowAnimation" should "sweep the active buffer with UiTransitions-owned animations" in {
    val swept = AnimationChoreography.withPaneFlowAnimation(animated, SweepDirection.Forward)(Map.empty)

    swept.get(bufferId).exists(_.hasActiveAnimations) shouldBe true
    swept.get(bufferId).map(_.animations.values.map(_.owner).toSet) shouldBe Some(Set(AnimationOwner.UiTransitions))
  }

  it should "leave the buffer animations untouched when motion is off" in {
    val off      = baseState(AppConfig.withTestAnimations.withMotionAccessibility(MotionAccessibility.Off))
    val existing = Map(bufferId -> AnimationState.empty)

    AnimationChoreography.withPaneFlowAnimation(off, SweepDirection.Forward)(existing) shouldBe existing
  }
