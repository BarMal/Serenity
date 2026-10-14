package com.serenity.perf

import java.awt.Font

import cats.effect.unsafe.implicits.global
import com.serenity.keystroke.events.InsertChar
import com.serenity.state.manager.RenderCaches
import com.serenity.ui.layout.{CellMetrics, ViewportSize, WrappedLineCache}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A keystroke in a prose document sizes the scrollbar's thumb without recounting the document: the scene it builds
  * wraps the lines the edit touched and no others.
  */
class ScrollbarTypingSceneSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val codeFont = LaptopFrameBenchmarks.codeFont
  private val textFont = Font(Font.SERIF, Font.PLAIN, 14)
  private val metrics  = CellMetrics.fromFont(codeFont)
  private val viewport = ViewportSize(1500 / metrics.charWidth, 1000 / metrics.lineHeight)

  "The scene built after a keystroke" should "wrap only the lines the keystroke reached" in {
    val (stateManager, _) = LaptopFrameBenchmarks.proseStateManager()
    val caches            = RenderCaches.create()
    val wraps = caches.wrappedLines match
      case bounded: WrappedLineCache.Bounded => bounded
      case other                             => fail(s"expected a bounded cache, got $other")

    val first = stateManager.getCurrentState.unsafeRunSync()
    val scene = caches.authoritativeScene.forState(first, viewport, codeFont, textFont)
    scene.paneScrolls should not be empty
    val coldWraps = wraps.wrapStats.coldWraps + wraps.wrapStats.incrementalWraps

    stateManager.applyEvent(InsertChar('x')).unsafeRunSync()
    val typed = caches.authoritativeScene.forState(
      stateManager.getCurrentState.unsafeRunSync(),
      viewport,
      codeFont,
      textFont
    )
    val wrappedByKeystroke = wraps.wrapStats.coldWraps + wraps.wrapStats.incrementalWraps - coldWraps

    typed.paneScrolls.keySet shouldBe scene.paneScrolls.keySet
    wrappedByKeystroke should be <= 3L
  }
