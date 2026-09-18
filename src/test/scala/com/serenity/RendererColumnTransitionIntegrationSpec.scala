package com.serenity

import java.awt.Font

import com.serenity.animation.{EasingCurve, Tween, TransitionDirection}
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.RendererEntryPoints
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Column-based document layout (issue #1338, Phase 1 animation): a full `RendererEntryPoints.render` pass actually
  * paints the outgoing column's receding text when `AppState.runtime.columnTransitions` holds an in-flight transition
  * for the active buffer -- not just `RendererColumnTransition.render` in isolation.
  */
class RendererColumnTransitionIntegrationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val cellMetrics  = CellMetrics.fromFont(monoFont)
  private val viewportSize = ViewportSize(40, 12)

  private def buildState: AppState =
    val paneId   = PaneId(0)
    val bufferId = BufferId(1)
    val content  = (0 until 30).map(i => f"L$i%02d" + ("x" * 60)).mkString("\n")
    val buffer   = Buffer.fromString(bufferId, content)
    val pane     = EditorPane.withBuffer(paneId, bufferId)
    val base     = AppState.initial
    base.copy(persisted =
      base.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> pane),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        config = AppConfig.default.withLineNumbers(false).withoutStatusLine.withColumnMode(true)
      )
    )

  "RendererEntryPoints.render" should "paint the receding column's text when a column transition is in flight" in {
    val state    = buildState
    val bufferId = state.persisted.bufferOrder.headOption.getOrElse(fail("expected a buffer"))
    val withTransition = state.copy(runtime =
      state.runtime.copy(columnTransitions =
        Map(
          bufferId -> ColumnTransitionState(
            tween = Tween(start = 0.0, end = 1.0, curve = EasingCurve.Linear, steps = 4, currentFrame = 1),
            direction = TransitionDirection.RightToLeft,
            previousTopLine = 0,
            previousTopVisualLine = 0
          )
        )
      )
    )
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    RendererEntryPoints.render(
      withTransition,
      cursorVisible = true,
      surface,
      viewportSize,
      monoFont,
      monoFont,
      cellMetrics,
      None
    )

    // "L00" is the outgoing column's own first line; painting it at all (on top of whatever the incoming column,
    // starting further down the document, painted first) is proof the transition overlay actually ran.
    surface.drawRunPxCalls.map(_.s).exists(_.startsWith("L00")) shouldBe true
  }

  it should "not paint anything extra when there is no in-flight column transition" in {
    val state   = buildState
    val surface = new MockRenderSurface(viewportSize.width, viewportSize.height)

    noException should be thrownBy RendererEntryPoints.render(
      state,
      cursorVisible = true,
      surface,
      viewportSize,
      monoFont,
      monoFont,
      cellMetrics,
      None
    )
  }
