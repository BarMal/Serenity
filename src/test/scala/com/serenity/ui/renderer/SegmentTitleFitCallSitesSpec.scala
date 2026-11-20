package com.serenity.ui.renderer

import java.awt.Font
import java.nio.file.Path

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{CompositionPreview, CompositionRevisions}
import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.*
import com.serenity.ui.theme.Theme
import com.serenity.{MockRenderSurface, TestWorkspaceTrees}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The tab strip is the one place a segment carries a `SegmentTitleFit`. What production does with it is pinned here,
  * so `SegmentTextFit` leaving a fitted title uncut when it is given no advances is checked, not read: a surface that
  * measures text hands the strip the render owner's advances and the titles are measured through them; a surface that
  * measures none is given no advances, and measures nothing.
  */
class SegmentTitleFitCallSitesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val font     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val metrics  = TestMeasure.cellMetrics(font)
  private val viewport = ViewportSize(60, 12)
  private val paneId   = PaneId(0)
  private val ids      = (1 to 3).map(BufferId(_)).toList

  private def state: AppState =
    val buffers = ids.map { id =>
      val buffer = Buffer.fromString(id, "text\n" * 10)
      buffer.copy(document = buffer.document.copy(filePath = Some(Path.of(s"a_considerably_long_file_name_$id.scala"))))
    }
    val base = AppState.initial(AppConfig.conventional.withTabMaxWidth(14))
    base.copy(persisted =
      base.persisted.copy(
        buffers = buffers.map(b => b.id -> b).toMap,
        bufferOrder = ids,
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, ids.last)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        theme = Theme.light,
        focus = Focus.EditorPane(paneId)
      )
    )

  private def advancesMeasuredWhenRendering(measuresText: Boolean): Long =
    val caches = TestMeasure.renderCaches()
    val surface = MockRenderSurface(
      viewport.width * metrics.charWidth,
      viewport.height * metrics.lineHeight,
      measuresText = measuresText
    )
    RendererEntryPoints.render(
      CompositionPreview.displayed(state, new CompositionRevisions),
      cursorVisible = true,
      surface,
      viewport,
      FontSpec.fromAwt(font),
      FontSpec.fromAwt(font),
      metrics,
      None,
      caches
    )
    caches.measuredAdvances.measurements

  "A tab strip on a surface that measures text" should "measure its fitted titles through the owner's advances" in {
    advancesMeasuredWhenRendering(measuresText = true) should be > 0L
  }

  "A tab strip on a surface that measures no text" should "measure nothing and leave its titles as the layout counted them" in {
    advancesMeasuredWhenRendering(measuresText = false) shouldBe 0L
  }
