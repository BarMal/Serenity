package com.serenity.state.manager

import java.util.concurrent.Executor
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.*

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A window being resized (#2445): only a size that has settled is counted, and the bar the pane last had exactly stays
  * on show until the count for the settled size lands. The quiet time is a clock the spec moves by hand.
  */
class LateScrollbarResizeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val rows     = 30

  final private class ByHand extends Executor:
    private val queued = mutable.Queue.empty[Runnable]
    private val total  = new AtomicInteger(0)
    def started: Int   = total.get
    override def execute(task: Runnable): Unit =
      val _ = total.incrementAndGet()
      queued.enqueue(task)
    @tailrec def runAll(): Unit =
      if queued.nonEmpty then
        queued.dequeue().run()
        runAll()

  final private class ManualQuiet extends QuietTimer:
    final private case class Waiting(due: FiniteDuration, task: Runnable)
    private val now     = new AtomicLong(0L)
    private val waiting = mutable.ArrayBuffer.empty[Waiting]

    override def after(delay: FiniteDuration)(task: Runnable): () => Unit =
      val entry = Waiting(now.get.nanos + delay, task)
      waiting += entry
      () => waiting -= entry: Unit

    def advance(by: FiniteDuration): Unit =
      val until = now.addAndGet(by.toNanos).nanos
      val due   = waiting.filter(_.due <= until).toList
      waiting --= due
      due.foreach(_.task.run())

  final private class Window:
    val executor = new ByHand
    val quiet    = new ManualQuiet
    val notified = mutable.ListBuffer.empty[PaneId]
    val scene: AuthoritativeUiScene =
      AuthoritativeUiScene(scrollCounting = ScrollCounting.deferred(executor, notified += _, quiet))

    def at(state: AppState, columns: Int): UiSceneSnapshot = scene.forState(state, ViewportSize(columns, rows))

    def counted(state: AppState, columns: Int): ScrollExtent =
      at(state, columns)
      executor.runAll()
      at(state, columns).paneScrolls(paneId).extent

    def settle(): Unit =
      quiet.advance(ScrollCounting.ResizeQuietPeriod)
      executor.runAll()

  private def stateOf(text: String, columns: Int): AppState =
    val size    = ViewportSize(columns, rows)
    val buffer  = Buffer.fromString(bufferId, text)
    val initial = AppState.initial(AppConfig.conventional)
    val state = initial.copy(
      runtime = initial.runtime.copy(viewportSize = Some(size)),
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
    LayoutEngine.syncViewportDimensions(state, size)

  private def prose(prefix: String): String =
    Vector.tabulate(300)(line => s"$prefix $line " + "lorem ipsum dolor sit amet " * 12).mkString("\n")

  /** The same document with its text replaced by an edit that cannot say what changed. */
  private def replaced(state: AppState, text: String): AppState =
    val buffer = state.persisted.buffers(bufferId)
    val edited = buffer.copy(document = buffer.document.withContent(Buffer.fromString(bufferId, text).document.content))
    state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, edited)))

  private def exactExtent(state: AppState, columns: Int): ScrollExtent =
    AuthoritativeUiScene().forState(state, ViewportSize(columns, rows)).paneScrolls(paneId).extent

  "A pane being resized" should "keep the bar it last had exactly until the settled size is counted" in {
    val window = new Window
    val wide   = stateOf(prose("Paragraph"), 100)
    val narrow = stateOf(prose("Paragraph"), 90)
    val atWide = window.counted(wide, 100)

    window.at(narrow, 90).paneScrolls(paneId).extent shouldBe atWide

    window.settle()
    window.at(narrow, 90).paneScrolls(paneId).extent shouldBe exactExtent(narrow, 90)
  }

  it should "show no bar when none was ever counted" in {
    val window = new Window
    val wide   = stateOf(prose("Paragraph"), 100)
    val narrow = stateOf(prose("Paragraph"), 90)
    window.at(wide, 100)

    window.at(narrow, 90).paneScrolls shouldBe empty

    window.settle()
    window.at(narrow, 90).paneScrolls(paneId).extent shouldBe exactExtent(narrow, 90)
  }

  it should "stop counting the size it left and count only the size it settles at" in {
    val window = new Window
    val wide   = stateOf(prose("Paragraph"), 100)
    val narrow = stateOf(prose("Paragraph"), 90)
    window.at(wide, 100)
    window.executor.started shouldBe 1

    window.at(narrow, 90)
    window.executor.started shouldBe 1

    window.settle()
    window.executor.started shouldBe 2
    window.notified.toList shouldBe List(paneId)
  }

  "A burst of resize steps" should "start one count, for the last size, once they are quiet" in {
    val window = new Window
    window.counted(stateOf(prose("Paragraph"), 100), 100)
    val before = window.executor.started
    val steps  = List(99, 98, 97, 96, 95).map(columns => columns -> stateOf(prose("Paragraph"), columns))

    steps.foreach { (columns, state) =>
      window.at(state, columns)
      window.quiet.advance(ScrollCounting.ResizeQuietPeriod / 3)
    }
    window.executor.started shouldBe before

    window.settle()
    window.executor.started shouldBe before + 1
    val (columns, last) = steps.last
    window.at(last, columns).paneScrolls(paneId).extent shouldBe exactExtent(last, columns)
  }

  "A resize followed by an edit before the count lands" should "never show a total from the text before the edit" in {
    val window = new Window
    window.counted(stateOf(prose("Paragraph"), 100), 100)
    val narrow = stateOf(prose("Paragraph"), 90)
    val edited = replaced(narrow, prose("Replaced"))
    window.at(narrow, 90)

    window.at(edited, 90).paneScrolls shouldBe empty

    window.settle()
    window.at(edited, 90).paneScrolls(paneId).extent shouldBe exactExtent(edited, 90)
  }

  "A resize, an edit and a further resize" should "end with the bar for the last text at the last width" in {
    val window = new Window
    window.counted(stateOf(prose("Paragraph"), 100), 100)
    val narrow = stateOf(prose("Paragraph"), 90)
    val edited = replaced(narrow, prose("Replaced"))
    val last   = replaced(stateOf(prose("Paragraph"), 80), prose("Replaced"))
    window.at(narrow, 90)
    window.at(edited, 90)
    window.at(last, 80)

    window.settle()

    window.at(last, 80).paneScrolls(paneId).extent shouldBe exactExtent(last, 80)
    window.notified.toList shouldBe List(paneId, paneId)
  }

  "Returning to a width whose rows are still measured" should "show its bar at once and start no count" in {
    val window = new Window
    val wide   = stateOf(prose("Paragraph"), 100)
    val narrow = stateOf(prose("Paragraph"), 90)
    val atWide = window.counted(wide, 100)
    val before = window.executor.started
    window.at(narrow, 90)

    window.at(wide, 100).paneScrolls(paneId).extent shouldBe atWide

    window.quiet.advance(ScrollCounting.ResizeQuietPeriod)
    window.executor.started shouldBe before
  }
