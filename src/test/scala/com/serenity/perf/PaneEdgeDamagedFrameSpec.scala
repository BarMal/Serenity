package com.serenity.perf

import java.awt.image.BufferedImage

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.{PageDown, ScrollDown}
import com.serenity.rope.Balance
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.{AppState, Damage}
import com.serenity.testkit.MacOsSeamQuarantine
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Beside another pane, a frame that redraws only the damaged rows must come out the same as a frame drawn from scratch
  * for the same state, pixel for pixel. Two panes scrolled one line left a 5-column band at the shared edge that
  * differed (#2481); the rest of the frame matched.
  */
class PaneEdgeDamagedFrameSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private case class Scenario(
      name: String,
      panes: Int,
      step: SplitPaneFixture => (AppState, AppState),
      document: Int => String = SplitPaneFixture.prose
  )

  private val scenarios = List(
    Scenario("two panes, a one-line scroll", 2, _.transition(ScrollDown(1))),
    Scenario("two panes, a page down", 2, _.transition(PageDown)),
    Scenario("four panes, a one-line scroll", 4, _.transition(ScrollDown(1))),
    Scenario("two panes, a keystroke", 2, _.typing()),
    Scenario("four panes, a page down", 4, _.transition(PageDown)),
    Scenario("four panes, a keystroke", 4, _.typing()),
    Scenario(
      "four panes whose lines overhang the left edge, a one-line scroll",
      4,
      _.transition(ScrollDown(1)),
      SplitPaneFixture.overhangingLines
    ),
    Scenario(
      "two panes whose lines overhang the left edge, a one-line scroll",
      2,
      _.transition(ScrollDown(1)),
      SplitPaneFixture.overhangingLines
    )
  )

  private def differingPixels(a: BufferedImage, b: BufferedImage): IndexedSeq[(Int, Int)] =
    val width   = a.getWidth
    val pixelsA = a.getRGB(0, 0, width, a.getHeight, null, 0, width)
    val pixelsB = b.getRGB(0, 0, width, b.getHeight, null, 0, width)
    pixelsA.indices.filter(index => pixelsA(index) != pixelsB(index)).map(index => (index % width, index / width))

  scenarios.foreach { scenario =>
    s"A damaged frame of ${scenario.name}" should "equal a frame drawn from scratch, pixel for pixel" in {
      assume(!MacOsSeamQuarantine.appliesHere, MacOsSeamQuarantine.Reason)
      val (before, after) = scenario.step(SplitPaneFixture.open(scenario.panes, scenario.document))
      val damage          = DamageProducer.forTransition(before, after)
      damage should not be Damage.Everything

      val frames = LaptopFrameBenchmarks.PersistingFrames()
      val _      = frames.render(before, Damage.Everything)
      val drawn  = frames.render(after, damage)
      val cold = LaptopFrameBenchmarks.renderedFrame(
        after,
        RenderCaches.create(),
        LaptopFrameBenchmarks.frameImage(),
        cursorVisible = false
      )

      val differing = differingPixels(drawn, cold)
      withClue(s"${differing.size} pixels differ, in device columns ${differing.map(_._1).distinct.sorted}: ")(
        differing shouldBe empty
      )
    }
  }
