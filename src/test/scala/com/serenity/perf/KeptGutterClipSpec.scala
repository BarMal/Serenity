package com.serenity.perf

import java.util.concurrent.atomic.AtomicReference

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.ScrollDown
import com.serenity.rope.Balance
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.Damage
import com.serenity.ui.layout.PixelRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A redrawn row's antialiased ink reaches a pixel past its band, and a pixel of it that lands on a kept row's copied
  * pixels composites twice (#2481). macOS showed it in the line-number counter, not only beside the text: the clip that
  * keeps painting off kept pixels has to cover the counter's kept bands as it covers the text's.
  */
class KeptGutterClipSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "A one-line scroll of four panes" should "keep a repainted line number off the top of the kept counter row below it" in {
    val (before, after) = SplitPaneFixture.open(4).transition(ScrollDown(1))
    val kept            = AtomicReference(List.empty[PixelRect])
    val frames          = LaptopFrameBenchmarks.PersistingFrames(onKeptOff = rects => kept.set(rects))
    val _               = frames.render(before, Damage.Everything)
    kept.set(Nil)
    val _ = frames.render(after, DamageProducer.forTransition(before, after))

    val rects    = kept.get
    val textLeft = rects.filter(_.widthPx > 200).map(_.xPx).min
    val guards   = rects.filter(rect => rect.heightPx <= 2 && rect.xPx < textLeft)

    guards should not be empty
  }
