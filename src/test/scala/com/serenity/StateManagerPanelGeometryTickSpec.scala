package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.animation.{EasingCurve, Tween}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.ui.layout.{LayoutRect, PanelContent, PanelPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Panel scale-in/out (issue #1085 phase 1): `animationTicker.advanceAnimationsOnTick` advances `Runtime.panelGeometry`
  * once per tick, mirroring `Runtime.columnTransitions`' tick-driven advance (`StateManagerColumnTransitionTickSpec`),
  * and additionally reclaims a closing ghost overlay once its geometry-only scale-out completes with no colour fade
  * left to remove it (`AnimationChoreography.advancePanelGeometry`).
  */
class StateManagerPanelGeometryTickSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private def makeStateManager(): StateManager =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    StateManager.apply(logger).unsafeRunSync()

  private def seedGeometry(sm: StateManager, surfaceId: SurfaceId, steps: Int): Unit =
    sm.updateState { state =>
      state.copy(runtime =
        state.runtime.copy(panelGeometry =
          Map(
            surfaceId -> PanelGeometryState(
              Tween(
                start = LayoutRect(0, 0, 0, 10),
                end = LayoutRect(0, 0, 20, 10),
                curve = EasingCurve.Linear,
                steps = steps
              )
            )
          )
        )
      )
    }.unsafeRunSync()

  "advanceAnimationsOnTick" should "return true while a panel-geometry animation is still in flight" in {
    val sm        = makeStateManager()
    val surfaceId = SurfaceId("panel-1")
    seedGeometry(sm, surfaceId, steps = 4)

    val result = sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    result shouldBe true
    sm.getCurrentState.unsafeRunSync().runtime.panelGeometry(surfaceId).currentRect shouldBe LayoutRect(0, 0, 5, 10)
  }

  it should "drop the panel-geometry animation once it completes, returning false" in {
    val sm        = makeStateManager()
    val surfaceId = SurfaceId("panel-1")
    seedGeometry(sm, surfaceId, steps = 1)

    val result = sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    result shouldBe false
    sm.getCurrentState.unsafeRunSync().runtime.panelGeometry shouldBe empty
  }

  it should "leave other runtime animation state untouched while advancing a panel-geometry animation" in {
    val sm        = makeStateManager()
    val surfaceId = SurfaceId("panel-1")
    seedGeometry(sm, surfaceId, steps = 4)
    val before = sm.getCurrentState.unsafeRunSync()

    sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    val after = sm.getCurrentState.unsafeRunSync()
    after.runtime.themeTransition shouldBe before.runtime.themeTransition
    after.runtime.surfaceAnimations shouldBe before.runtime.surfaceAnimations
    after.runtime.columnTransitions shouldBe before.runtime.columnTransitions
  }

  it should "remove a closing ghost overlay once its geometry-only scale-out completes" in {
    val sm      = makeStateManager()
    val ghostId = SurfaceId("closing-ghost")
    sm.updateState { state =>
      val ghost = UiSurface(
        id = ghostId,
        content = SurfaceContent.GhostOverlay(SurfaceContent.Outline(Nil), LayoutRect(0, 0, 20, 10)),
        presentation = SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
      )
      state.copy(runtime =
        state.runtime.copy(
          uiSurfaces = state.runtime.uiSurfaces :+ ghost,
          panelGeometry = Map(
            ghostId -> PanelGeometryState(
              Tween(
                start = LayoutRect(0, 0, 20, 10),
                end = LayoutRect(0, 0, 0, 10),
                curve = EasingCurve.Linear,
                steps = 1
              )
            )
          )
        )
      )
    }.unsafeRunSync()

    sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    state.runtime.panelGeometry shouldBe empty
    state.runtime.uiSurfaces.exists(_.id == ghostId) shouldBe false
  }

  it should "not remove a real docked panel's own surface once its opening geometry completes" in {
    val sm = makeStateManager()
    // Pinned through the panel manager so the docked surface sits in the workspace tree, as a real panel does.
    sm.panelManager.pinPanel(PanelContent.Outline(Nil), PanelPosition.Right, 20).unsafeRunSync()
    val surfaceId = sm.getCurrentState
      .unsafeRunSync()
      .runtime
      .uiSurfaces
      .collectFirst { case UiSurface(id, SurfaceContent.Outline(_, _), SurfacePresentation.Docked, _) => id }
      .getOrElse(fail("Expected the pinned outline panel's docked surface"))
    sm.updateState { state =>
      state.copy(runtime =
        state.runtime.copy(
          panelGeometry = Map(
            surfaceId -> PanelGeometryState(
              Tween(
                start = LayoutRect(0, 0, 0, 10),
                end = LayoutRect(0, 0, 20, 10),
                curve = EasingCurve.Linear,
                steps = 1
              )
            )
          )
        )
      )
    }.unsafeRunSync()

    sm.animationTicker.advanceAnimationsOnTick.unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    state.runtime.panelGeometry shouldBe empty
    state.runtime.uiSurfaces.exists(_.id == surfaceId) shouldBe true
  }
