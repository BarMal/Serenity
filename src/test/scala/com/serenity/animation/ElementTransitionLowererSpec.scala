package com.serenity.animation

import java.awt.Color

import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ElementTransitionLowererSpec extends AnyFlatSpec with Matchers:

  private val transparent = new Color(0, 0, 0, 0)
  private val ink         = new Color(20, 20, 20)

  private def cell(char: Char): CellAnimation =
    CellAnimation(char, transparent, ink)

  "ElementTransitionLowerer" should "produce no active animation for disabled transitions" in {
    val plan = ElementTransitionPlan(
      scope = TransitionScope.PanelOpen,
      kind = TransitionKind.Disabled,
      direction = TransitionDirection.LeftToRight,
      timing = TransitionTiming.immediate
    )

    val state = ElementTransitionLowerer.lower(
      plan,
      ElementTransitionCells(content = Map(CharacterKey(0, 0) -> cell('a')))
    )

    state.shouldBe(AnimationState.empty)
  }

  it should "lower left-to-right sweeps into column-forward staggered cells" in {
    val plan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.Row, Some(PanelPosition.Left)),
      ElementTransitionSettings.subtle
    )

    val state = ElementTransitionLowerer.lower(
      plan,
      ElementTransitionCells(
        content = Map(
          CharacterKey(0, 0) -> cell('a'),
          CharacterKey(1, 0) -> cell('b'),
          CharacterKey(2, 0) -> cell('c')
        )
      ),
      tickRateMs = 16
    )

    state.animations(CharacterKey(0, 0)).foregroundSteps.length.shouldBe(10)
    state.animations(CharacterKey(1, 0)).foregroundSteps.length.shouldBe(11)
    state.animations(CharacterKey(2, 0)).foregroundSteps.length.shouldBe(12)
  }

  it should "lower right-to-left sweeps into column-backward staggered cells" in {
    val plan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.Row, Some(PanelPosition.Right)),
      ElementTransitionSettings.subtle
    )

    val state = ElementTransitionLowerer.lower(
      plan,
      ElementTransitionCells(
        content = Map(
          CharacterKey(0, 0) -> cell('a'),
          CharacterKey(1, 0) -> cell('b'),
          CharacterKey(2, 0) -> cell('c')
        )
      ),
      tickRateMs = 16
    )

    state.animations(CharacterKey(0, 0)).foregroundSteps.length.shouldBe(12)
    state.animations(CharacterKey(1, 0)).foregroundSteps.length.shouldBe(11)
    state.animations(CharacterKey(2, 0)).foregroundSteps.length.shouldBe(10)
  }

  it should "delay outline-then-content cells until after frame cells begin" in {
    val plan = ElementTransitionPlanner.plan(
      ElementTransitionRequest(TransitionScope.PanelOpen, Some(PanelPosition.Left)),
      ElementTransitionSettings.subtle
    )

    val state = ElementTransitionLowerer.lower(
      plan,
      ElementTransitionCells(
        frame = Map(CharacterKey(0, 0) -> cell('|')),
        content = Map(CharacterKey(1, 0) -> cell('a'))
      ),
      tickRateMs = 16
    )

    state.animations(CharacterKey(0, 0)).foregroundSteps.length.shouldBe(10)
    state.animations(CharacterKey(1, 0)).foregroundSteps.length.should(be > 10)
  }
