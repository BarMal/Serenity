package com.serenity.state.manager

import java.util.concurrent.Executor

import scala.annotation.tailrec
import scala.collection.mutable

import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.frontend.FrontendCapabilities
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

/** How the app hands a pane's row count to a background worker and learns that it is done (#2445): the notice that
  * reaches the render loop, the frontends that count late, and the state manager whose caches do.
  */
class ScrollCountWiringSpec extends AnyFlatSpec with Matchers:

  given Balance                                              = Balance.default
  given org.typelevel.log4cats.LoggerFactory[cats.effect.IO] = NoOpFactory[cats.effect.IO]

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

  private def longDocument: AppState =
    val buffer =
      Buffer.fromString(bufferId, Vector.tabulate(300)(line => s"line $line " + "lorem ipsum " * 20).mkString("\n"))
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

  "The notice to the render loop" should "drop a pane counted before anyone listens" in {
    val notice = new ScrollCountNotice
    val heard  = mutable.ListBuffer.empty[PaneId]

    notice.deliver(paneId)
    notice.listen(heard += _)

    heard shouldBe empty
  }

  it should "reach whoever listens once the pane is counted" in {
    val notice = new ScrollCountNotice
    val heard  = mutable.ListBuffer.empty[PaneId]
    notice.listen(heard += _)

    notice.deliver(paneId)

    heard.toList shouldBe List(paneId)
  }

  it should "reach only the latest listener" in {
    val notice = new ScrollCountNotice
    val first  = mutable.ListBuffer.empty[PaneId]
    val second = mutable.ListBuffer.empty[PaneId]
    notice.listen(first += _)
    notice.listen(second += _)

    notice.deliver(paneId)

    (first.toList, second.toList) shouldBe (Nil, List(paneId))
  }

  "A graphical frontend" should "count a long document's rows late and tell the notice" in {
    val executor = new ByHand
    val notice   = new ScrollCountNotice
    val heard    = mutable.ListBuffer.empty[PaneId]
    notice.listen(heard += _)
    val scene =
      AuthoritativeUiScene(scrollCounting = ScrollCounting.forFrontend(FrontendCapabilities.gui, executor, notice))

    scene.forState(longDocument, viewportSize).paneScrolls shouldBe empty
    executor.runAll()

    scene.forState(longDocument, viewportSize).paneScrolls should contain key paneId
    heard.toList shouldBe List(paneId)
  }

  "A terminal frontend" should "keep counting inside the frame, with nothing handed to a worker" in {
    val executor = new ByHand
    val scene = AuthoritativeUiScene(scrollCounting =
      ScrollCounting.forFrontend(FrontendCapabilities.tui(), executor, new ScrollCountNotice)
    )

    scene.forState(longDocument, viewportSize).paneScrolls should contain key paneId
    executor.pending shouldBe 0
  }

  "A state manager made with late counting" should "build scenes whose bar waits for its rows" in {
    val executor = new ByHand
    val config   = AppConfig.conventional
    val manager = StateManager
      .apply(
        NoOpLogger[cats.effect.IO],
        initialConfig = config,
        dictionaryCache = SharedDictionary.cacheFor(config),
        scrollCounting = ScrollCounting.deferred(executor, _ => ())
      )
      .unsafeRunSync()

    manager.renderCaches.authoritativeScene.forState(longDocument, viewportSize).paneScrolls shouldBe empty
    executor.runAll()

    manager.renderCaches.authoritativeScene.forState(longDocument, viewportSize).paneScrolls should contain key paneId
  }
