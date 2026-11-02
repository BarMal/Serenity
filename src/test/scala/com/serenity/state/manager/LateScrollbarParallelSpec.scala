package com.serenity.state.manager

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentLinkedQueue, Executor, Semaphore, TimeUnit}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Random

import com.serenity.StateManagerTestFixtures
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Several panes' counts run at once on a pool of threads (#2483), over the row indexes they share with each other and
  * with the render thread. Whatever order they interleave in, each pane ends with the bar one thread would have counted
  * for its last text, told once, and panes that share an index share the work of measuring it.
  */
class LateScrollbarParallelSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewport = ViewportSize(100, 30)

  final private class ByHand extends Executor:
    private val queued = mutable.Queue.empty[Runnable]
    private val total  = new AtomicInteger(0)
    def started: Int   = total.get

    override def execute(task: Runnable): Unit =
      val _ = total.incrementAndGet()
      queued.enqueue(task)

    def firstQueued: Option[Runnable] = queued.headOption

    @tailrec final def runAll(): Unit =
      if queued.nonEmpty then
        queued.dequeue().run()
        runAll()

  private def prose(prefix: String, lines: Int = 400): String =
    Vector
      .tabulate(lines)(line => s"$prefix line $line " + "lorem ipsum dolor sit amet " * (4 + line % 5 * 3))
      .mkString("\n")

  /** Two columns of two panes, the first showing buffer 1; each other pane shows its own buffer, or buffer 1 again. */
  private def fourPanes(texts: Vector[String]): AppState =
    val buffers = texts.zipWithIndex.map((text, index) => Buffer.fromString(BufferId(index + 1), text))
    val first   = PaneId(0)
    val initial = AppState.initial(AppConfig.conventional)
    val base = initial.copy(
      runtime = initial.runtime.copy(viewportSize = Some(viewport), nextBufferId = BufferId(buffers.size + 1)),
      persisted = initial.persisted.copy(
        buffers = buffers.map(buffer => buffer.id -> buffer).toMap,
        bufferOrder = buffers.map(_.id).toList,
        layout = initial.persisted.layout.copy(
          editorPanes = Map(first -> EditorPane.withBuffer(first, BufferId(1))),
          activeEditorPaneId = Some(first),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), first)))
        ),
        focus = Focus.EditorPane(first)
      )
    )
    def shown(pane: Int) = Some(BufferId(if texts.size == 1 then 1 else pane))
    def insert(from: AppState, after: PaneId, buffer: Int, axis: SplitAxis): (AppState, PaneId) =
      StateManagerTestFixtures.insertPane(from, Some(after), shown(buffer), axis)
    val (withRight, right) = insert(base, first, 2, SplitAxis.Horizontal)
    val (withBelow, _)     = insert(withRight, first, 3, SplitAxis.Vertical)
    val (all, _)           = insert(withBelow, right, 4, SplitAxis.Vertical)
    LayoutEngine.syncViewportDimensions(all, viewport)

  private def panes(state: AppState): List[PaneId] = state.persisted.layout.orderedPaneIds

  private def edited(state: AppState, bufferId: BufferId, text: String): AppState =
    val buffer = state.persisted.buffers(bufferId)
    val after  = buffer.copy(document = buffer.document.withContent(Buffer.fromString(bufferId, text).document.content))
    state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, after)))

  private def extents(state: AppState, size: ViewportSize = viewport): Map[PaneId, ScrollExtent] =
    AuthoritativeUiScene().forState(state, size).paneScrolls.view.mapValues(_.extent).toMap

  final private class Notices:
    val heard   = new ConcurrentLinkedQueue[PaneId]()
    val arrived = new Semaphore(0)

    def tell(paneId: PaneId): Unit =
      heard.add(paneId): Unit
      arrived.release()

  /** Draws `state` until every one of its panes shows a bar, waiting on the notices between draws. */
  @tailrec private def barsLand(
    scene: AuthoritativeUiScene,
    notices: Notices,
    state: AppState,
    size: ViewportSize,
    waits: Int
  ): Boolean =
    scene.forState(state, size).paneScrolls.keySet == panes(state).toSet ||
      (waits > 0 && notices.arrived
        .tryAcquire(10, TimeUnit.SECONDS) && barsLand(scene, notices, state, size, waits - 1))

  /** Waits until every pane has been told its bar is counted, then checks none is told twice. A bar can show in a
    * scene as soon as its rows are measured, before the run that measured them has told every pane, so the notices are
    * what is waited on.
    */
  private def toldOnce(notices: Notices, state: AppState): Unit =
    val all = panes(state)
    notices.arrived.tryAcquire(all.size, 60, TimeUnit.SECONDS) shouldBe true
    notices.arrived.tryAcquire(1, 300, TimeUnit.MILLISECONDS) shouldBe false
    notices.heard.asScala.toList.sortBy(_.value) shouldBe all.sortBy(_.value)

  private def withPool[A](threads: Int)(body: Executor => A): A =
    val pool = ScrollCountExecutor.pool(threads)
    body(pool)

  "Four panes of different buffers counted on four threads" should "each end with the bar one thread counts, told once" in
    withPool(4) { pool =>
      val notices = new Notices
      val scene   = AuthoritativeUiScene(scrollCounting = ScrollCounting.deferred(pool, notices.tell))
      val state   = fourPanes(Vector("a", "b", "c", "d").map(prose(_)))

      scene.forState(state, viewport).paneScrolls shouldBe empty
      toldOnce(notices, state)

      scene.forState(state, viewport).paneScrolls.view.mapValues(_.extent).toMap shouldBe extents(state)
    }

  "Four panes of one buffer counted on four threads" should "each end with the bar one thread counts, told once" in
    withPool(4) { pool =>
      val notices = new Notices
      val scene   = AuthoritativeUiScene(scrollCounting = ScrollCounting.deferred(pool, notices.tell))
      val state   = fourPanes(Vector(prose("shared")))

      scene.forState(state, viewport)
      toldOnce(notices, state)

      scene.forState(state, viewport).paneScrolls.view.mapValues(_.extent).toMap shouldBe extents(state)
    }

  "Edits and resizes across four panes on four threads" should "end with each pane's bar for its last text and width" in
    withPool(4) { pool =>
      val notices = new Notices
      val scene   = AuthoritativeUiScene(scrollCounting = ScrollCounting.deferred(pool, notices.tell))
      val random  = new Random(2483)
      val first   = fourPanes(Vector("a", "b", "c", "d").map(prose(_)))
      val sizes   = Vector(ViewportSize(100, 30), ViewportSize(96, 30), ViewportSize(88, 30))

      val (last, lastSize) = (1 to 40).foldLeft((first, sizes.head)) {
        case ((before, _), round) =>
          val bufferId = BufferId(1 + random.nextInt(4))
          val size     = sizes(random.nextInt(sizes.size))
          val after =
            edited(before, bufferId, prose(s"round $round buffer ${bufferId.value}", 300 + random.nextInt(200)))
          val sized = LayoutEngine.syncViewportDimensions(after, size)
          scene.forState(sized, size)
          (sized, size)
      }

      barsLand(scene, notices, last, lastSize, waits = 40) shouldBe true
      scene.forState(last, lastSize).paneScrolls.view.mapValues(_.extent).toMap shouldBe extents(last, lastSize)
    }

  "Panes whose rows share an index" should "start one count between them and tell every pane" in {
    val executor = new ByHand
    val notices  = new Notices
    val scene    = AuthoritativeUiScene(scrollCounting = ScrollCounting.deferred(executor, notices.tell))
    val state    = fourPanes(Vector(prose("shared")))

    scene.forState(state, viewport).paneScrolls shouldBe empty
    executor.started shouldBe 1
    executor.runAll()

    notices.heard.asScala.toList.sortBy(_.value) shouldBe panes(state).sortBy(_.value)
    scene.forState(state, viewport).paneScrolls.view.mapValues(_.extent).toMap shouldBe extents(state)
  }

  "Panes whose rows are their own" should "each start a count" in {
    val executor = new ByHand
    val scene    = AuthoritativeUiScene(scrollCounting = ScrollCounting.deferred(executor, _ => ()))
    val state    = fourPanes(Vector("a", "b", "c", "d").map(prose(_)))

    scene.forState(state, viewport)

    executor.started shouldBe 4
  }

  "A shared count whose first pane is replaced by an edit" should "still count for the panes that stand" in {
    val executor = new ByHand
    val notices  = new Notices
    val scene    = AuthoritativeUiScene(scrollCounting = ScrollCounting.deferred(executor, notices.tell))
    val state    = fourPanes(Vector(prose("shared")))
    val later    = edited(state, BufferId(1), prose("changed"))

    scene.forState(state, viewport)
    scene.forState(later, viewport)
    executor.runAll()

    scene.forState(later, viewport).paneScrolls.view.mapValues(_.extent).toMap shouldBe extents(later)
    notices.heard.asScala.toList.sortBy(_.value) shouldBe panes(later).sortBy(_.value)
  }

  "The panes' counts" should "start with the focused pane's, then the others in pane order" in {
    val executor = new ByHand
    val notices  = new Notices
    val scene    = AuthoritativeUiScene(scrollCounting = ScrollCounting.deferred(executor, notices.tell))
    val state    = fourPanes(Vector("a", "b", "c", "d").map(prose(_)))
    val ids      = panes(state)
    val focused  = ids.last
    val refocused =
      state.copy(persisted =
        state.persisted.copy(
          layout = state.persisted.layout.copy(activeEditorPaneId = Some(focused)),
          focus = Focus.EditorPane(focused)
        )
      )

    scene.forState(refocused, viewport)
    executor.runAll()

    notices.heard.asScala.toList shouldBe focused :: ids.filterNot(_ == focused)
  }
