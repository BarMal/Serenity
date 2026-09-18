package com.serenity

import java.awt.Font

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{RenderContext, RendererPaneSetup}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Column-based document layout (issue #1338, Phase 1 rendering): `RendererPaneSetup.snapshotForBuffer` must paint the
  * active column's own chunk of visual lines -- wrapped at the (narrower) column width, not the pane's full width --
  * whenever `columnModeEnabled && wordWrapEnabled`, and must be byte-for-byte unchanged otherwise.
  */
class RendererPaneSetupColumnModeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val monoFont    = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val cellMetrics = CellMetrics.fromFont(monoFont)
  private val contentRect = LayoutRect(0, 0, 100, 10)

  private def renderContext(surface: MockRenderSurface): RenderContext =
    RenderContext(
      surface = surface,
      layout = CalculatedLayout(
        editorPanelRect = LayoutRect(0, 0, 100, 10),
        leftSpacerRect = LayoutRect(0, 0, 0, 0),
        rightSpacerRect = LayoutRect(0, 0, 0, 0)
      ),
      codeFont = monoFont,
      textFont = monoFont,
      uiFont = monoFont,
      cellMetrics = cellMetrics,
      uiMetrics = cellMetrics
    )

  private def stateWithBuffer(buffer: Buffer, config: AppConfig): AppState =
    val base = AppState.initial
    base.copy(persisted =
      base.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        theme = Theme.light,
        config = config
      )
    )

  private def bufferOfLines(count: Int): Buffer =
    val content = (0 until count).map(i => s"line-$i").mkString("\n")
    Buffer.fromString(BufferId(1), content)

  // A target width of 40 cells (default gap 2) fits two columns into the 100-cell pane, so the rendered column is
  // genuinely narrower than the full pane -- the default 80-cell target fits only one column at the pane's own full
  // width, which would not prove wrapping used the column width rather than the pane width.
  private def twoColumnConfig: AppConfig =
    AppConfig.default.withoutStatusLine.withLineNumbers(false).withColumnMode(true).withColumnTargetWidth(40)

  "RendererPaneSetup.snapshotForBuffer" should "show only the active column's rows when column mode is on" in {
    val buffer  = bufferOfLines(60)
    val config  = twoColumnConfig
    val state   = stateWithBuffer(buffer, config)
    val surface = new MockRenderSurface(200, 40)
    val context = renderContext(surface)

    val snapshot = RendererPaneSetup.snapshotForBuffer(buffer, contentRect, state, context)

    snapshot.panelWidthPx should be < (contentRect.width * cellMetrics.charWidth)
    snapshot.visualLines should not be empty
    snapshot.visualLines.map(_.bufferLine) shouldBe snapshot.visualLines.map(_.bufferLine).sorted
  }

  it should "advance to the next column's rows once the viewport is scrolled past the first column" in {
    val buffer = bufferOfLines(60)
    val config = twoColumnConfig

    val firstColumnState = stateWithBuffer(buffer, config)
    val surface          = new MockRenderSurface(200, 40)
    val context          = renderContext(surface)
    val firstSnapshot    = RendererPaneSetup.snapshotForBuffer(buffer, contentRect, firstColumnState, context)
    val firstColumnLines = firstSnapshot.visualLines.map(_.bufferLine)

    val visibleLines   = firstSnapshot.visualLines.length
    val scrolledBuffer = buffer.copy(viewport = buffer.viewport.copy(topLine = visibleLines))
    val secondState    = stateWithBuffer(scrolledBuffer, config)
    val secondSnapshot = RendererPaneSetup.snapshotForBuffer(scrolledBuffer, contentRect, secondState, context)

    secondSnapshot.visualLines.map(_.bufferLine) should not be firstColumnLines
    secondSnapshot.visualLines.headOption.map(_.bufferLine) shouldBe Some(visibleLines)
  }

  it should "render exactly like the non-column path when column mode is off" in {
    val buffer      = bufferOfLines(60)
    val columnOff   = AppConfig.default.withoutStatusLine.withLineNumbers(false).withColumnMode(false)
    val stateOff    = stateWithBuffer(buffer, columnOff)
    val surfaceOff  = new MockRenderSurface(200, 40)
    val contextOff  = renderContext(surfaceOff)
    val snapshotOff = RendererPaneSetup.snapshotForBuffer(buffer, contentRect, stateOff, contextOff)

    val expected = TextLayoutSnapshot.fromBuffer(
      buffer.copy(viewport = LayoutEngine.updateBufferViewportDimensions(buffer, contentRect, wordWrapEnabled = true)),
      contentRect.width * cellMetrics.charWidth,
      monoFont,
      surfaceOff.fontRenderContext.getOrElse(fail("missing frc")),
      wordWrapEnabled = true,
      cellMetricsOverride = Some(cellMetrics)
    )

    snapshotOff.visualLines.map(_.bufferLine) shouldBe expected.visualLines.map(_.bufferLine)
    snapshotOff.panelWidthPx shouldBe expected.panelWidthPx
  }
