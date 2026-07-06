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

    state
      .animations(CharacterKey(0, 0))
      .foregroundAnimation
      .map(animation => animation.steps -> animation.delayFrames)
      .shouldBe(Some(10 -> 0))
    state
      .animations(CharacterKey(1, 0))
      .foregroundAnimation
      .map(animation => animation.steps -> animation.delayFrames)
      .shouldBe(Some(10 -> 1))
    state
      .animations(CharacterKey(2, 0))
      .foregroundAnimation
      .map(animation => animation.steps -> animation.delayFrames)
      .shouldBe(Some(10 -> 2))
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

    state
      .animations(CharacterKey(0, 0))
      .foregroundAnimation
      .map(animation => animation.steps -> animation.delayFrames)
      .shouldBe(Some(10 -> 2))
    state
      .animations(CharacterKey(1, 0))
      .foregroundAnimation
      .map(animation => animation.steps -> animation.delayFrames)
      .shouldBe(Some(10 -> 1))
    state
      .animations(CharacterKey(2, 0))
      .foregroundAnimation
      .map(animation => animation.steps -> animation.delayFrames)
      .shouldBe(Some(10 -> 0))
  }

  it should "apply planned delay to fade cells" in {
    val plan = ElementTransitionPlan(
      scope = TransitionScope.Glyph,
      kind = TransitionKind.Fade,
      direction = TransitionDirection.AnchorIn,
      timing = TransitionTiming(durationMs = 160, staggerMs = 16, delayMs = 32, speedScale = 1.0)
    )

    val state = ElementTransitionLowerer.lower(
      plan,
      ElementTransitionCells(content = Map(CharacterKey(0, 0) -> cell('a'))),
      tickRateMs = 16
    )

    state
      .animations(CharacterKey(0, 0))
      .foregroundAnimation
      .map(animation => animation.steps -> animation.delayFrames)
      .shouldBe(Some(10 -> 2))
  }

  it should "add planned delay before staggered transition offsets" in {
    val delayedKinds = List(
      TransitionKind.TypedText,
      TransitionKind.DirectionalSweep,
      TransitionKind.LineAndCharacterTandem
    )

    delayedKinds.foreach { kind =>
      val plan = ElementTransitionPlan(
        scope = TransitionScope.Row,
        kind = kind,
        direction = TransitionDirection.LeftToRight,
        timing = TransitionTiming(durationMs = 160, staggerMs = 16, delayMs = 32, speedScale = 1.0)
      )

      val state = ElementTransitionLowerer.lower(
        plan,
        ElementTransitionCells(
          content = Map(
            CharacterKey(0, 0) -> cell('a'),
            CharacterKey(1, 0) -> cell('b')
          )
        ),
        tickRateMs = 16
      )

      withClue(s"$kind should preserve planned delay before positional stagger") {
        state.animations(CharacterKey(0, 0)).foregroundAnimation.map(_.delayFrames).shouldBe(Some(2))
        state.animations(CharacterKey(1, 0)).foregroundAnimation.map(_.delayFrames).shouldBe(Some(3))
      }
    }
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

    state.animations(CharacterKey(0, 0)).foregroundAnimation.map(_.steps).shouldBe(Some(10))
    state.animations(CharacterKey(1, 0)).foregroundAnimation.map(_.delayFrames).exists(_ > 0).shouldBe(true)
  }

  it should "apply planned delay before outline-then-content staging" in {
    val plan = ElementTransitionPlan(
      scope = TransitionScope.PanelOpen,
      kind = TransitionKind.OutlineThenContent,
      direction = TransitionDirection.LeftToRight,
      timing = TransitionTiming(durationMs = 160, staggerMs = 16, delayMs = 32, speedScale = 1.0)
    )

    val state = ElementTransitionLowerer.lower(
      plan,
      ElementTransitionCells(
        frame = Map(CharacterKey(0, 0) -> cell('|')),
        content = Map(CharacterKey(1, 0) -> cell('a'))
      ),
      tickRateMs = 16
    )

    state.animations(CharacterKey(0, 0)).foregroundAnimation.map(_.delayFrames).shouldBe(Some(2))
    state.animations(CharacterKey(1, 0)).foregroundAnimation.map(_.delayFrames).shouldBe(Some(12))
  }
