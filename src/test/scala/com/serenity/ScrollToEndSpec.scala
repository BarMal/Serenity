package com.serenity

import com.serenity.keystroke.events.{ScrollDown, ScrollUp}
import com.serenity.state.components.{ComponentResult, EditorPaneComponent}
import com.serenity.state.models.*
import com.serenity.ui.layout.TextRowMetrics
import com.serenity.ui.renderer.EditorPaneRenderPlan
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1948 phase 1: the wheel stops where the document's last row is whole inside the pane, however many rows the pane's
  * own font fits and however the last paragraph wraps. The limit used to count code-grid rows against buffer lines, so
  * prose, whose rows are taller than a code cell, could never be scrolled to its last lines.
  *
  * Observed on the rendered frame: the row the last line ends on has to lie entirely inside the content rect.
  */
class ScrollToEndSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private val manyShortLines = Vector.tabulate(120)(line => s"line $line")

  /** The fixture draws with its own fonts; the limit measures the configured ones, as the app does, so they agree. */
  private def withFixtureFonts(state: AppState): AppState =
    val fonts = state.persisted.config.editorConfig.fontConfig.copy(
      codeFontFamily = codeFont.getFamily,
      fontSize = codeFont.getSize2D,
      textFontFamily = textFont.getFamily,
      textFontSize = textFont.getSize2D
    )
    state.copy(persisted = state.persisted.copy(config = state.persisted.config.withFontConfig(fonts)))

  /** As in the app, the viewport's size is the pane's content rect in code-grid cells, which the layout writes
    * (`LayoutEngine.updateViewportDimensions`), not the window's.
    */
  private def prose(lines: Vector[String]): AppState =
    val state = withFixtureFonts(stateOf(lines.mkString("\n"), CursorPosition(0, 0)))
    val content =
      Session().settle(state).paneLayouts.getOrElse(paneId, fail("expected a pane layout")).contentRect
    val buffer = state.persisted.buffers(bufferId)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(
          bufferId,
          buffer.copy(viewport = buffer.viewport.copy(visibleLines = content.height, visibleColumns = content.width))
        )
      )
    )

  private def scrolled(state: AppState, event: com.serenity.keystroke.events.TextEntryEvent): AppState =
    new EditorPaneComponent(paneId).processEvent(event, state) match
      case ComponentResult.ReducerUpdate(result) => result.state
      case other                                 => fail(s"expected a reduction, got $other")

  private def viewportOf(state: AppState): Viewport = state.persisted.buffers(bufferId).viewport

  /** The last row the frame draws: its line, whether it ends that line, and whether its band lies inside the pane. */
  private def lastRow(state: AppState, lines: Vector[String]): (Int, Boolean, Boolean) =
    val plan: EditorPaneRenderPlan = Session().draw(state, Damage.Everything)._2
    val layout                     = plan.paneLayouts.getOrElse(paneId, fail("expected a pane layout"))
    val snapshot                   = plan.snapshots.getOrElse(paneId, fail("expected a snapshot"))
    val metrics = TextRowMetrics.forSnapshot(layout.contentRect, cellMetrics, snapshot, snapshot.usesMeasuredLayout)
    val last    = snapshot.visualLines.lastOption.getOrElse(fail("expected a drawn row"))
    val index   = snapshot.visualLines.length - 1
    (
      last.bufferLine,
      last.endColumn >= lines(last.bufferLine).length,
      metrics.bandBottomPx(index) <= metrics.contentBottomPx
    )

  "Scrolling a prose document to its end" should "end on the last line, whole inside the pane" in {
    val state = prose(manyShortLines)

    val (line, endsLine, inside) = lastRow(scrolled(state, ScrollDown(10_000)), manyShortLines)

    line shouldBe manyShortLines.length - 1
    endsLine shouldBe true
    inside shouldBe true
  }

  it should "end on the last row of a last paragraph that wraps" in {
    val state = prose(proseLines)

    val (line, endsLine, inside) = lastRow(scrolled(state, ScrollDown(10_000)), proseLines)

    line shouldBe proseLines.length - 1
    endsLine shouldBe true
    inside shouldBe true
  }

  it should "stay put when scrolled further down" in {
    val atEnd = scrolled(prose(manyShortLines), ScrollDown(10_000))

    viewportOf(scrolled(atEnd, ScrollDown(10_000))) shouldBe viewportOf(atEnd)
  }

  it should "come back up from the end" in {
    val atEnd = scrolled(prose(manyShortLines), ScrollDown(10_000))

    viewportOf(scrolled(atEnd, ScrollUp(10_000))).topLine shouldBe 0
  }
