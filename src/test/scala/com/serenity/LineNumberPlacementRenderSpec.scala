package com.serenity

import java.awt.Font

import com.serenity.config.{LineNumberLayout, LineNumberSide}
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.RendererEntryPoints
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Rendering for configurable line-number placement: the gutter/body divider must sit on the content-facing edge of
  * each counter (the last column of a left counter, the first column of a right counter), and every digit of a line
  * number must land between the pane edge and that divider (#1979).
  */
class LineNumberPlacementRenderSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def stateWith(side: LineNumberSide): AppState =
    stateWithLines(side, (1 to 20).map(i => s"line $i").mkString("\n"), topLine = 0)

  private def stateWithLines(side: LineNumberSide, lines: String, topLine: Int): AppState =
    val buffer = Buffer
      .fromString(BufferId(1), lines)
      .copy(viewport = Viewport(topLine = topLine, leftColumn = 0, visibleLines = 10, visibleColumns = 20))
    val base = AppState.initial
    base.copy(persisted =
      base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = Layout(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0)),
          workspaceTree = Some(TestWorkspaceTrees.linear(PaneId(0)))
        ),
        theme = Theme.light,
        config = base.persisted.config.withLineNumberLayout(LineNumberLayout(side = side))
      )
    )

  private val viewport = ViewportSize(80, 24)

  "A left counter" should "paint its divider on its last column" in {
    val state   = stateWith(LineNumberSide.Left)
    val surface = new MockRenderSurface(80, 24)
    val layout  = LayoutEngine.calculateLayout(state, viewport)
    val rect    = layout.lineNumberRect.getOrElse(fail("expected a left counter"))

    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      com.serenity.state.manager.RenderCaches.create()
    )

    val dividerColumn = rect.x + rect.width - 1
    (rect.y until rect.bottom).foreach(row =>
      surface.getBg(dividerColumn, row) shouldBe state.persisted.theme.panelBorder
    )
    surface.getRow(rect.y).slice(rect.x, rect.right).trim shouldBe "1"
  }

  "A right counter" should "paint its divider on its first (content-facing) column and right-align its digits" in {
    val state   = stateWith(LineNumberSide.Right)
    val surface = new MockRenderSurface(80, 24)
    val layout  = LayoutEngine.calculateLayout(state, viewport)
    val rect    = layout.rightLineNumberRect.getOrElse(fail("expected a right counter"))

    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      com.serenity.state.manager.RenderCaches.create()
    )

    val dividerColumn = rect.x
    (rect.y until rect.bottom).foreach(row =>
      surface.getBg(dividerColumn, row) shouldBe state.persisted.theme.panelBorder
    )
    surface.getRow(rect.y).slice(rect.x, rect.right).trim shouldBe "1"
  }

  "Both placement" should "paint a counter on each side with mirrored dividers" in {
    val state   = stateWith(LineNumberSide.Both)
    val surface = new MockRenderSurface(80, 24)
    val layout  = LayoutEngine.calculateLayout(state, viewport)
    val left    = layout.lineNumberRect.getOrElse(fail("expected a left counter"))
    val right   = layout.rightLineNumberRect.getOrElse(fail("expected a right counter"))

    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      com.serenity.state.manager.RenderCaches.create()
    )

    surface.getBg(left.x + left.width - 1, left.y) shouldBe state.persisted.theme.panelBorder
    surface.getBg(right.x, right.y) shouldBe state.persisted.theme.panelBorder
    surface.getRow(left.y).slice(left.x, left.right).trim shouldBe "1"
    surface.getRow(right.y).slice(right.x, right.right).trim shouldBe "1"
  }

  private val codeFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val proseFont   = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val cellWidthPx = CellMetrics.fromFont(codeFont).charWidth
  private val subPixelPx  = 0.5f

  /** A digit-free prose buffer, so every all-digit measured run is a line number drawn in the prose font. */
  private def proseStateWith(side: LineNumberSide, lineCount: Int, topLine: Int): AppState =
    stateWithLines(side, List.fill(lineCount)("prose").mkString("\n"), topLine)

  private def renderProse(state: AppState): MockRenderSurface =
    val surface = new MockRenderSurface(80, 24)
    RendererEntryPoints.render(
      state,
      cursorVisible = false,
      surface,
      viewport,
      codeFont,
      proseFont,
      CellMetrics.fromFont(codeFont),
      None,
      com.serenity.state.manager.RenderCaches.create()
    )
    surface

  private def lineNumberRuns(surface: MockRenderSurface, rect: LayoutRect): List[MockRenderSurface#DrawRunPxCall] =
    surface.drawRunPxCalls.filter { call =>
      val digits    = call.s.trim
      val screenXPx = call.xPx + call.translationXPx.toFloat
      val nearRect  = screenXPx >= (rect.x - 1) * cellWidthPx && screenXPx < rect.right * cellWidthPx
      digits.nonEmpty && digits.forall(_.isDigit) && nearRect
    }

  /** The on-screen pixel span the run's digits cover, leading padding excluded. */
  private def digitSpanPx(call: MockRenderSurface#DrawRunPxCall): (Float, Float) =
    val font      = call.font.getOrElse(fail("expected the line number to be drawn in a measured font"))
    val screenXPx = call.xPx + call.translationXPx.toFloat
    val startPx   = screenXPx + TextAlignment.measureTextWidth(call.s.takeWhile(_ == ' '), font)
    (startPx, startPx + TextAlignment.measureTextWidth(call.s.trim, font))

  "A left counter on a buffer of 100+ lines" should "draw every three-digit number whole, clear of its divider" in {
    val state   = proseStateWith(LineNumberSide.Left, lineCount = 150, topLine = 99)
    val rect    = LayoutEngine.calculateLayout(state, viewport).lineNumberRect.getOrElse(fail("expected a counter"))
    val surface = renderProse(state)
    val runs    = lineNumberRuns(surface, rect)

    rect.width shouldBe 4
    runs.map(_.s.trim) should contain("100")
    runs.foreach { call =>
      val (startPx, endPx) = digitSpanPx(call)
      withClue(s"line ${call.s.trim}: ") {
        startPx should be >= rect.x * cellWidthPx - subPixelPx
        endPx should be <= (rect.right - 1) * cellWidthPx + subPixelPx
      }
    }
  }

  "A right counter on a buffer of 100+ lines" should "draw every three-digit number whole, clear of its divider" in {
    val state   = proseStateWith(LineNumberSide.Right, lineCount = 150, topLine = 99)
    val rect    = LayoutEngine.calculateLayout(state, viewport).rightLineNumberRect.getOrElse(fail("no counter"))
    val surface = renderProse(state)
    val runs    = lineNumberRuns(surface, rect)

    rect.width shouldBe 4
    runs.map(_.s.trim) should contain("100")
    runs.foreach { call =>
      val (startPx, endPx) = digitSpanPx(call)
      withClue(s"line ${call.s.trim}: ") {
        startPx should be >= (rect.x + 1) * cellWidthPx - subPixelPx
        endPx should be <= rect.right * cellWidthPx + subPixelPx
      }
    }
  }

  "The counter" should "widen by one cell per digit of the last document line and draw four-digit numbers whole" in {
    List(99 -> 3, 100 -> 4, 999 -> 4, 1000 -> 5).foreach {
      case (lineCount, expectedWidth) =>
        val state = proseStateWith(LineNumberSide.Left, lineCount, topLine = 0)
        LayoutEngine.calculateLayout(state, viewport).lineNumberRect.map(_.width) shouldBe Some(expectedWidth)
    }

    val state   = proseStateWith(LineNumberSide.Left, lineCount = 1050, topLine = 999)
    val rect    = LayoutEngine.calculateLayout(state, viewport).lineNumberRect.getOrElse(fail("expected a counter"))
    val surface = renderProse(state)
    val runs    = lineNumberRuns(surface, rect)

    runs.map(_.s.trim) should contain("1000")
    runs.foreach { call =>
      val (startPx, endPx) = digitSpanPx(call)
      withClue(s"line ${call.s.trim}: ") {
        startPx should be >= rect.x * cellWidthPx - subPixelPx
        endPx should be <= (rect.right - 1) * cellWidthPx + subPixelPx
      }
    }
  }
