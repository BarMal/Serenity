package com.serenity.perf

import java.util.concurrent.Executor

import scala.annotation.tailrec
import scala.collection.mutable

import cats.effect.unsafe.implicits.global
import com.serenity.state.manager.{RenderCaches, ScrollCounting}
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The window shows only the rects each frame names (see [[ColdFrameHarness]]). A scrollbar that appears once its
  * pane's rows are counted (#2445) must arrive as a repaint of its own strip and leave the screen as a cold frame with
  * the bar would have it.
  */
class LateScrollbarFrameSpec extends AnyFlatSpec with Matchers with ColdFrameHarness:

  final private class ByHand extends Executor:
    private val queued                         = mutable.Queue.empty[Runnable]
    override def execute(task: Runnable): Unit = queued.enqueue(task)

    @tailrec def runAll(): Unit =
      if queued.nonEmpty then
        queued.dequeue().run()
        runAll()

  private def longDocument: AppState =
    val (manager, _) =
      LaptopFrameBenchmarks.stateManagerHolding(
        BenchmarkFixtures.loremIpsumProse(40),
        baseConfig.withScrollbar(true),
        grid = viewport
      )
    manager.getCurrentState.unsafeRunSync()

  private def withoutBar(state: AppState): AppState =
    state.copy(persisted = state.persisted.copy(config = state.persisted.config.withScrollbar(false)))

  "The first frame of a long document" should "show no bar, as a frame with the scrollbar off does" in {
    val state    = longDocument
    val executor = new ByHand
    val frames =
      Pooled(1.0, persists = true, RenderCaches.create(scrollCounting = ScrollCounting.deferred(executor, _ => ())))

    val first = frames.render(state, Damage.Everything)

    differing(first, withoutBar(state), 1.0) shouldBe empty
    differing(first, state, 1.0) should not be empty
  }

  "The repaint for the bar once the rows are counted" should "leave the screen as a cold frame with the bar draws it" in {
    val state    = longDocument
    val paneId   = state.persisted.layout.editorPanes.keys.head
    val executor = new ByHand
    val frames =
      Pooled(1.0, persists = true, RenderCaches.create(scrollCounting = ScrollCounting.deferred(executor, _ => ())))
    frames.render(state, Damage.Everything)

    executor.runAll()
    val counted = frames.render(state, Damage.Scrollbar(paneId))

    differing(counted, state, 1.0) shouldBe empty
  }
