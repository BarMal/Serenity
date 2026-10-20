package com.serenity

import java.awt.Font
import java.util.concurrent.atomic.AtomicInteger

import com.serenity.config.AppConfig
import com.serenity.rope.{Balance, Leaf}
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, Layout, ViewportSize}
import com.serenity.ui.renderer.{FontSpec, RendererEntryPoints}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Scrolling a long document costs what is visible, not what the document holds. */
class ScrollReadBoundSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val lineTotal    = 50000
  private val visibleLines = 24
  private val margin       = 2 * visibleLines

  final private class CountingRope(text: String, val lineReads: AtomicInteger) extends Leaf(text):

    override def getLine(lineIndex: Int): Option[String] =
      lineReads.incrementAndGet()
      super.getLine(lineIndex)

    override def linesIteratorFrom(lineIndex: Int): Iterator[(Int, String)] =
      super.linesIteratorFrom(lineIndex).map { line =>
        lineReads.incrementAndGet()
        line
      }

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val cellMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(80, visibleLines + 2)
  private val document     = (1 to lineTotal).map(line => s"line number $line of the long document").mkString("\n")

  private def scrollOf(wordWrap: Boolean): Int =
    val paneId    = PaneId(0)
    val bufferId  = BufferId(1)
    val lineReads = AtomicInteger(0)
    val content   = CountingRope(document, lineReads)
    val middle    = lineTotal / 2
    val buffer = Buffer(bufferId, Document(content)).copy(
      editing = EditingState(List(CursorPosition(middle, 0))),
      viewport = Viewport(topLine = middle, leftColumn = 0, visibleColumns = 80, visibleLines = visibleLines)
    )
    val initial = AppState.initial
    val state = initial.copy(persisted =
      initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        config = AppConfig.default.withLineNumbers(true).withoutStatusLine.withWordWrap(wordWrap)
      )
    )
    val scrolled = state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers + (bufferId -> buffer.copy(viewport = buffer.viewport.copy(topLine = middle + 1)))
      )
    )
    val caches  = RenderCaches.create()
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)
    def render(frame: AppState): Unit =
      RendererEntryPoints.render(
        frame,
        cursorVisible = true,
        surface,
        viewportSize,
        FontSpec.fromAwt(monoFont),
        FontSpec.fromAwt(monoFont),
        cellMetrics,
        None,
        caches
      )
    render(state)
    lineReads.set(0)
    render(scrolled)
    lineReads.get()

  "Scrolling a 50k-line document" should "read only about the visible lines per frame without wrapping" in {
    scrollOf(wordWrap = false) should be <= visibleLines + margin
  }

  it should "read only about the visible lines per frame with word wrap" in {
    scrollOf(wordWrap = true) should be <= visibleLines + margin
  }
