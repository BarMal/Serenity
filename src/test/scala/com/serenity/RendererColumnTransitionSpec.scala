package com.serenity

import java.awt.Font

import com.serenity.animation.{EasingCurve, Tween, TransitionDirection}
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{RenderContext, RendererColumnTransition, RendererPaneSetup}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Column-based document layout (issue #1338, Phase 1 animation): `RendererColumnTransition.render` paints the receding
  * column's plain text into the sliver of the pane the sweep hasn't yet covered with the new column's content, and
  * nothing once the sweep is complete.
  */
class RendererColumnTransitionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val cellMetrics = CellMetrics.fromFont(monoFont)
  private val contentRect = LayoutRect(0, 0, 20, 8)

  private def renderContext(surface: MockRenderSurface): RenderContext =
    RenderContext(
      surface = surface,
      layout = CalculatedLayout(
        editorPanelRect = LayoutRect(0, 0, 20, 8),
        leftSpacerRect = LayoutRect(0, 0, 0, 0),
        rightSpacerRect = LayoutRect(0, 0, 0, 0)
      ),
      codeFont = monoFont,
      textFont = monoFont,
      uiFont = monoFont,
      cellMetrics = cellMetrics,
      uiMetrics = cellMetrics
    )

  private def stateWith(buffer: Buffer): AppState =
    val base = AppState.initial
    base.copy(persisted =
      base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        theme = Theme.light,
        config = AppConfig.default.withoutStatusLine.withLineNumbers(false).withColumnMode(true)
      )
    )

  // 30 distinguishable lines, one column ("old", starting at bufferOrder 0) and the next ("new", line 8+). Each line
  // is long enough to span the whole pane width, so a sweep boundary partway across the pane actually falls inside
  // the text rather than past its (short) end.
  private def buffer: Buffer =
    val content = (0 until 30).map(i => f"L$i%02d" + ("x" * 40)).mkString("\n")
    Buffer.fromString(BufferId(1), content)

  private def activeSnapshot(
    state: AppState,
    context: RenderContext
  ): (Buffer, com.serenity.ui.layout.TextLayoutSnapshot) =
    val buf = buffer.copy(viewport = Viewport(topLine = 8, leftColumn = 0, visibleColumns = 20, visibleLines = 8))
    buf -> RendererPaneSetup.snapshotForBuffer(
      buf,
      contentRect,
      state.copy(persisted = state.persisted.copy(buffers = Map(buf.id -> buf))),
      context
    )

  "RendererColumnTransition.render" should "paint the receding column's text when the sweep just started" in {
    val state             = stateWith(buffer)
    val surface           = new MockRenderSurface(200, 40)
    val context           = renderContext(surface)
    val (buf, activeSnap) = activeSnapshot(state, context)
    val transition = ColumnTransitionState(
      tween = Tween(start = 0.0, end = 1.0, curve = EasingCurve.Linear, steps = 4),
      direction = TransitionDirection.RightToLeft,
      previousTopLine = 0,
      previousTopVisualLine = 0
    )

    RendererColumnTransition.render(buf, contentRect, state, context, activeSnap, transition)

    surface.drawRunPxCalls.map(_.s).exists(_.contains("L00")) shouldBe true
    surface.drawRunPxCalls.map(_.s).exists(_.contains("L08")) shouldBe false
  }

  it should "paint nothing once the sweep has fully completed" in {
    val state             = stateWith(buffer)
    val surface           = new MockRenderSurface(200, 40)
    val context           = renderContext(surface)
    val (buf, activeSnap) = activeSnapshot(state, context)
    val transition = ColumnTransitionState(
      tween = Tween(start = 0.0, end = 1.0, curve = EasingCurve.Linear, steps = 1, currentFrame = 1),
      direction = TransitionDirection.RightToLeft,
      previousTopLine = 0,
      previousTopVisualLine = 0
    )
    transition.isComplete shouldBe true

    RendererColumnTransition.render(buf, contentRect, state, context, activeSnap, transition)

    surface.drawRunPxCalls shouldBe empty
  }

  it should "shrink the visible sliver of receding text as progress advances" in {
    val state             = stateWith(buffer)
    val context           = renderContext(new MockRenderSurface(200, 40))
    val (buf, activeSnap) = activeSnapshot(state, context)

    def paintedWidth(progress: Double): Float =
      val surface = new MockRenderSurface(200, 40)
      val ctx     = renderContext(surface)
      val steps   = 4
      val transition = ColumnTransitionState(
        tween = Tween(
          start = 0.0,
          end = 1.0,
          curve = EasingCurve.Linear,
          steps = steps,
          currentFrame = math.round(progress * steps).toInt
        ),
        direction = TransitionDirection.RightToLeft,
        previousTopLine = 0,
        previousTopVisualLine = 0
      )
      RendererColumnTransition.render(buf, contentRect, state, ctx, activeSnap, transition)
      surface.drawRunPxCalls.map(_.bgWidthPx).sum

    paintedWidth(0.75) should be < paintedWidth(0.25)
  }
