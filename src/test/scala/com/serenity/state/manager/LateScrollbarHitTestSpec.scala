package com.serenity.state.manager

import java.util.concurrent.Executor

import scala.annotation.tailrec
import scala.collection.mutable

import com.serenity.config.AppConfig
import com.serenity.keystroke.events.MousePress
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A press on the strip where a pane's bar will appear (#2445): until the rows are counted there is no bar to take hold
  * of, so the press reaches the text beside it; once they are, the bar takes it.
  */
class LateScrollbarHitTestSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val viewportSize = ViewportSize(100, 30)
  private val paneId       = PaneId(0)
  private val bufferId     = BufferId(1)

  final private class ByHand extends Executor:
    private val queued                         = mutable.Queue.empty[Runnable]
    override def execute(task: Runnable): Unit = queued.enqueue(task)

    @tailrec def runAll(): Unit =
      if queued.nonEmpty then
        queued.dequeue().run()
        runAll()

  private def stateOf(lines: Int): AppState =
    val buffer  = Buffer.fromString(bufferId, (0 until lines).map(line => s"line $line").mkString("\n"))
    val initial = AppState.initial(AppConfig.conventional)
    val state = initial.copy(
      runtime = initial.runtime.copy(viewportSize = Some(viewportSize)),
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), paneId)))
        ),
        focus = Focus.EditorPane(paneId)
      )
    )
    LayoutEngine.syncViewportDimensions(state, viewportSize)

  private def onStrip(state: AppState, scene: UiSceneSnapshot): MousePress =
    val metrics = MouseHitTestGeometry.floatingCellMetrics(state)
    val strip   = ScrollbarLayout.strip(scene.paneLayouts(paneId).contentRect, metrics)
    val pixelX  = strip.xPx + strip.widthPx / 2
    val pixelY  = strip.yPx + strip.heightPx / 2
    MousePress(metrics.toCol(pixelX), metrics.toRow(pixelY), Some(pixelX), Some(pixelY))

  "A press on the strip of a pane whose rows are not counted" should "reach the text, and take hold of the bar once they are" in {
    val state    = stateOf(300)
    val executor = new ByHand
    val scene    = AuthoritativeUiScene(scrollCounting = ScrollCounting.deferred(executor, _ => ()))
    val waiting  = scene.forState(state, viewportSize)
    val press    = onStrip(state, waiting)

    ScrollbarHitTesting.barAt(press, state, waiting) shouldBe None
    MouseTransition.run(state)(ScrollbarHitTesting.press(press, state, waiting))._2 shouldBe false

    executor.runAll()
    val counted = scene.forState(state, viewportSize)

    ScrollbarHitTesting.barAt(press, state, counted).map(_.paneId) shouldBe Some(paneId)
    MouseTransition.run(state)(ScrollbarHitTesting.press(press, state, counted))._2 shouldBe true
  }
