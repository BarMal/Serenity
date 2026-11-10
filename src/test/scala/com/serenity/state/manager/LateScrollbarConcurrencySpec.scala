package com.serenity.state.manager

import java.util.concurrent.{Executors, Semaphore, TimeUnit}

import scala.annotation.tailrec
import scala.concurrent.duration.DurationInt

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The count runs on another thread beside the render thread, over the one row index (#2445). Edits that each leave
  * every row unmeasured keep the two working on the same index while it changes under them; whatever the order they
  * interleave in, the bar that lands is the one a single thread would have counted for the last text.
  */
class LateScrollbarConcurrencySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewportSize = ViewportSize(100, 30)
  private val paneId       = PaneId(0)
  private val bufferId     = BufferId(1)

  private def stateOf(text: String): AppState =
    val buffer  = Buffer.fromString(bufferId, text)
    val initial = AppState.initial(AppConfig.conventional)
    val state = initial.copy(
      runtime = initial.runtime.copy(viewportSize = Some(viewportSize)),
      persisted = initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), paneId)))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )
    LayoutEngine.syncViewportDimensions(state, viewportSize)

  private def replaced(state: AppState, text: String): AppState =
    val buffer = state.persisted.buffers(bufferId)
    val edited = buffer.copy(document = buffer.document.withContent(Buffer.fromString(bufferId, text).document.content))
    state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, edited)))

  private def text(round: Int): String =
    Vector
      .tabulate(400)(line => s"Round $round line $line " + "lorem ipsum dolor sit amet " * (4 + line % 5 * 3))
      .mkString("\n")

  "A count beside a run of whole-text edits" should "end with the bar a single thread counts for the last text" in {
    val executor = Executors.newSingleThreadExecutor { runnable =>
      val thread = new Thread(runnable, "late-scrollbar-spec")
      thread.setDaemon(true)
      thread
    }
    val counts = new Semaphore(0)
    val scene  = AuthoritativeUiScene(scrollCounting = ScrollCounting.deferred(executor, _ => counts.release()))
    val first  = stateOf(text(0))
    val states = (1 to 25).scanLeft(first)((before, round) => replaced(before, text(round)))

    states.foreach(state => scene.forState(state, viewportSize))

    val last = states.last
    barLands(scene, last, counts, deadline = System.nanoTime + 120.seconds.toNanos) shouldBe true
    executor.shutdown()
    scene.forState(last, viewportSize).paneScrolls(paneId).extent shouldBe
      AuthoritativeUiScene().forState(last, viewportSize).paneScrolls(paneId).extent
  }

  it should "wait for the last text's bar however many earlier texts were announced first" in {
    val inner = Executors.newSingleThreadExecutor { runnable =>
      val thread = new Thread(runnable, "late-scrollbar-spec")
      thread.setDaemon(true)
      thread
    }
    val executor = new HoldableExecutor(inner)
    val counts   = new Semaphore(0)
    val scene    = AuthoritativeUiScene(scrollCounting = ScrollCounting.deferred(executor, _ => counts.release()))
    val states   = (1 to 25).scanLeft(stateOf(text(0)))((before, round) => replaced(before, text(round)))

    states.init.zipWithIndex.foreach { (state, announced) =>
      scene.forState(state, viewportSize)
      counts.tryAcquire(announced + 1, 10, TimeUnit.SECONDS) shouldBe true
      counts.release(announced + 1)
    }
    val last = states.last
    executor.hold()
    scene.forState(last, viewportSize)
    executor.stopHolding()
    executor.releaseAfter(1.second)

    barLands(scene, last, counts, deadline = System.nanoTime + 120.seconds.toNanos) shouldBe true
    inner.shutdown()
    scene.forState(last, viewportSize).paneScrolls(paneId).extent shouldBe
      AuthoritativeUiScene().forState(last, viewportSize).paneScrolls(paneId).extent
  }

  /** A notice says some text's count ended, not that the last text's did, so a notice is only a reason to look again:
    * the wait ends when the bar lands, when ten seconds pass with no notice, or at `deadline`.
    */
  @tailrec private def barLands(
    scene: AuthoritativeUiScene,
    last: AppState,
    counts: Semaphore,
    deadline: Long
  ): Boolean =
    scene.forState(last, viewportSize).paneScrolls.contains(paneId) ||
      (System.nanoTime < deadline && counts.tryAcquire(10, TimeUnit.SECONDS) && barLands(scene, last, counts, deadline))
