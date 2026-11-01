package com.serenity.state.manager

import java.util.concurrent.Executor

import scala.annotation.tailrec
import scala.collection.mutable

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The scrollbar of a pane whose rows are not all measured on the first frame (#2445): the first scene has no bar, the
  * rest of the rows are counted off the render path, and the scene that follows carries the same bar the whole-document
  * count gave before. The count is handed to an executor the spec runs by hand, so nothing here depends on time.
  */
class LateScrollbarSceneSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewportSize = ViewportSize(100, 30)
  private val paneId       = PaneId(0)
  private val bufferId     = BufferId(1)

  final private class ByHand extends Executor:
    private val queued                         = mutable.Queue.empty[Runnable]
    def pending: Int                           = queued.size
    override def execute(task: Runnable): Unit = queued.enqueue(task)

    @tailrec def runAll(): Unit =
      if queued.nonEmpty then
        queued.dequeue().run()
        runAll()

  final private class Counted:
    val executor = new ByHand
    val notified = mutable.ListBuffer.empty[PaneId]
    val scene: AuthoritativeUiScene =
      AuthoritativeUiScene(scrollCounting = ScrollCounting.deferred(executor, notified += _))

  private def stateOf(text: String, topLine: Int = 0): AppState =
    val buffer   = Buffer.fromString(bufferId, text)
    val scrolled = buffer.copy(viewport = buffer.viewport.scrolledTo(topLine, 0, 0))
    val initial  = AppState.initial(AppConfig.conventional)
    val state = initial.copy(
      runtime = initial.runtime.copy(viewportSize = Some(viewportSize)),
      persisted = initial.persisted.copy(
        buffers = Map(buffer.id -> scrolled),
        bufferOrder = List(buffer.id),
        layout = initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppConfig.conventional
      )
    )
    LayoutEngine.syncViewportDimensions(state, viewportSize)

  private def lines(count: Int, text: Int => String): String = Vector.tabulate(count)(text).mkString("\n")

  private val prose = (line: Int) => s"Paragraph $line " + "lorem ipsum dolor sit amet " * 12

  private def wholeDocument(count: Int): AppState = stateOf(lines(count, prose))

  private def typed(state: AppState, offset: Int, text: String): AppState =
    val buffer = state.persisted.buffers(bufferId)
    val edited = buffer.copy(
      document =
        buffer.document.withContent(buffer.document.content.insert(offset, text).getOrElse(buffer.document.content))
    )
    state.copy(persisted = state.persisted.copy(buffers = state.persisted.buffers.updated(bufferId, edited)))

  /** What the pane's scrollbar was before it was counted late: the whole document measured inside the frame. */
  private def exactExtent(state: AppState): ScrollExtent =
    AuthoritativeUiScene().forState(state, viewportSize).paneScrolls(paneId).extent

  private def wraps(scene: AuthoritativeUiScene): Long = scene.wrappedLines match
    case bounded: WrappedLineCache.Bounded => bounded.wrapStats.coldWraps + bounded.wrapStats.incrementalWraps
    case other                             => fail(s"expected a bounded cache, got $other")

  "A cold scene of a document longer than the screen" should "have no scrollbar and wrap only the lines on screen" in {
    val counted = new Counted
    val scene   = counted.scene.forState(wholeDocument(300), viewportSize)

    scene.paneScrolls shouldBe empty
    wraps(counted.scene) should be <= viewportSize.height.toLong
  }

  it should "get the bar it had before, once the rest of the rows are counted" in {
    val state   = wholeDocument(300)
    val counted = new Counted
    counted.scene.forState(state, viewportSize)

    counted.executor.runAll()

    counted.scene.forState(state, viewportSize).paneScrolls(paneId).extent shouldBe exactExtent(state)
  }

  it should "say once that the pane's rows are counted" in {
    val state   = wholeDocument(300)
    val counted = new Counted
    counted.scene.forState(state, viewportSize)
    counted.notified shouldBe empty

    counted.executor.runAll()

    counted.notified.toList shouldBe List(paneId)
  }

  it should "start one count however many times the same scene is asked for" in {
    val state   = wholeDocument(300)
    val counted = new Counted

    (1 to 5).foreach(_ => counted.scene.forState(state, viewportSize))

    counted.executor.pending shouldBe 1
  }

  "A document the screen covers" should "have its scrollbar at once, with nothing left to count" in {
    val state   = wholeDocument(10)
    val counted = new Counted

    val scene = counted.scene.forState(state, viewportSize)

    scene.paneScrolls(paneId).extent shouldBe exactExtent(state)
    counted.executor.pending shouldBe 0
  }

  "A document of more than 2048 lines" should "keep its estimated scrollbar at once, with nothing to count" in {
    val state   = stateOf(lines(2500, line => s"line $line"))
    val counted = new Counted

    val scene = counted.scene.forState(state, viewportSize)

    scene.paneScrolls should contain key paneId
    counted.executor.pending shouldBe 0
  }

  "Typing in a document whose rows are counted" should "keep the bar on every scene and start no count" in {
    val first   = wholeDocument(300)
    val counted = new Counted
    counted.scene.forState(first, viewportSize)
    counted.executor.runAll()

    val afterTyping = typed(first, offset = 5, "x")
    val scene       = counted.scene.forState(afterTyping, viewportSize)

    scene.paneScrolls(paneId).extent shouldBe exactExtent(afterTyping)
    counted.executor.pending shouldBe 0
  }

  "An edit that leaves rows off screen unmeasured" should "hide the bar until they are counted again" in {
    val counted = new Counted
    counted.scene.forState(wholeDocument(300), viewportSize)
    counted.executor.runAll()

    val replaced = stateOf(lines(300, line => s"Replaced $line " + "ipsum dolor sit amet " * 15))
    val before   = counted.scene.forState(replaced, viewportSize)
    before.paneScrolls shouldBe empty

    counted.executor.runAll()
    counted.scene.forState(replaced, viewportSize).paneScrolls(paneId).extent shouldBe exactExtent(replaced)
  }

  "An edit while the rows are being counted" should "never give the bar a total from the text before it" in {
    val original = wholeDocument(300)
    val counted  = new Counted
    counted.scene.forState(original, viewportSize)

    val replaced = stateOf(lines(300, line => s"Replaced $line " + "ipsum dolor sit amet " * 15))
    counted.scene.forState(replaced, viewportSize)
    counted.executor.runAll()

    counted.scene.forState(replaced, viewportSize).paneScrolls(paneId).extent shouldBe exactExtent(replaced)
    counted.notified.toList shouldBe List(paneId)
  }

  "A pane that loses its bar while its rows are being counted" should "stop counting and tell nobody" in {
    val state   = wholeDocument(300)
    val counted = new Counted
    counted.scene.forState(state, viewportSize)

    val barless = state.copy(persisted = state.persisted.copy(config = state.persisted.config.withScrollbar(false)))
    counted.scene.forState(barless, viewportSize).paneScrolls shouldBe empty
    counted.executor.runAll()

    counted.notified shouldBe empty
  }
