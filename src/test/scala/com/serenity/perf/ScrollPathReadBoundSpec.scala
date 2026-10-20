package com.serenity.perf

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.ScrollDown
import com.serenity.rope.{Balance, Leaf}
import com.serenity.state.manager.StateManagerTestFacade.updateState
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A wheel scroll through a live `StateManager` -- the reducer, the damage it reports and the frame drawn -- reads the
  * lines it shows, however long the document is.
  */
class ScrollPathReadBoundSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  final private class CountingRope(text: String, val lineReads: AtomicInteger) extends Leaf(text):

    override def getLine(lineIndex: Int): Option[String] =
      lineReads.incrementAndGet()
      super.getLine(lineIndex)

    override def linesIteratorFrom(lineIndex: Int): Iterator[(Int, String)] =
      super.linesIteratorFrom(lineIndex).map { line =>
        lineReads.incrementAndGet()
        line
      }

  private val lineTotal = 50000
  private val margin    = 100
  private val text      = (1 to lineTotal).map(line => s"line number $line of the long document").mkString("\n")

  private def readsPerStep(config: AppConfig): List[(String, Int)] =
    val (stateManager, bufferId) =
      LaptopFrameBenchmarks.stateManagerHolding(text, config, cursorLine = lineTotal / 2, cursorColumn = 4)
    val reads = AtomicInteger(0)
    stateManager
      .updateState(state =>
        state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.map {
          case (`bufferId`, buffer) =>
            bufferId -> buffer.copy(document = buffer.document.copy(content = CountingRope(text, reads)))
          case other => other
        }))
      )
      .unsafeRunSync()
    val caches            = RenderCaches.create()
    val image             = LaptopFrameBenchmarks.frameImage()
    def current: AppState = stateManager.getCurrentState.unsafeRunSync()
    def scrollOne(): Unit = stateManager.applyEvent(ScrollDown(1)).unsafeRunSync()
    def measured(step: () => Unit): Int =
      reads.set(0)
      step()
      reads.get()
    val _ = LaptopFrameBenchmarks.renderedFrame(current, caches, image)
    scrollOne()
    val _       = LaptopFrameBenchmarks.renderedFrame(current, caches, image)
    val before  = current
    val reduced = measured(() => scrollOne())
    val after   = current
    val damaged = measured(() =>
      val _ = DamageProducer.forTransition(before, after)
    )
    val drawn = measured(() =>
      val _ = LaptopFrameBenchmarks.renderedFrame(after, caches, image)
    )
    List("reduce" -> reduced, "damage" -> damaged, "render" -> drawn)

  "A one-line wheel scroll through a 50k-line document" should "read only the visible lines when wrapping" in
    readsPerStep(AppConfig.conventional).foreach { (step, count) =>
      withClue(s"$step read $count lines: ")(count should be <= margin)
    }

  it should "read only the visible lines without wrapping" in
    readsPerStep(AppConfig.conventional.withWordWrap(false)).foreach { (step, count) =>
      withClue(s"$step read $count lines: ")(count should be <= margin)
    }
