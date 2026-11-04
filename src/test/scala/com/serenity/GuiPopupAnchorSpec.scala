package com.serenity

import com.serenity.config.{AppConfig, HeadingsConfig}
import com.serenity.richtext.{ParagraphRole, RichTextDocument, RichTextParagraph}
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, TextRowMetrics, ViewportSize}
import com.serenity.ui.renderer.{EditorPaneRenderPlan, RendererFramePlanner}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** In the windowed editor a heading's space is measured in pixels and a row can be taller than a cell, so the rows a
  * caret sits below are not a count of cells. A popup still hangs off the cell the caret is painted in.
  */
class GuiPopupAnchorSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val fonts    = FontLoader.FontConfig()
  private val codeFont = FontLoader.previewCodeFont(fonts)
  private val textFont = FontLoader.previewTextFont(fonts)
  private val metrics  = CellMetrics.fromFont(codeFont)
  private val logicalW = 900
  private val logicalH = 600
  private val viewport = ViewportSize(logicalW / metrics.charWidth, logicalH / metrics.lineHeight)

  private val document = RichTextDocument(
    List(
      RichTextParagraph.plain("Opening line."),
      RichTextParagraph.plain("Headline", role = ParagraphRole.Heading(1)),
      RichTextParagraph.plain("Under the headline."),
      RichTextParagraph.plain("Subhead", role = ParagraphRole.Heading(2)),
      RichTextParagraph.plain("Final line.")
    )
  )

  private val spaced = AppConfig.conventional.withoutStatusLine
    .withLineNumbers(false)
    .withHeadingsConfig(
      HeadingsConfig()
        .withLevel(1)(_.copy(spaceBefore = Some(2.0f), spaceAfter = Some(1.0f)))
        .withLevel(2)(_.copy(spaceBefore = Some(2.0f), spaceAfter = Some(1.0f)))
    )

  private def stateOf(config: AppConfig, cursor: CursorPosition, placement: SurfacePlacement): AppState =
    val base = Buffer.fromString(bufferId, document.plainText)
    val buffer = base.copy(
      richText = RichTextState(richTextDocument = Some(document)),
      editing = EditingState(List(cursor)),
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = viewport.height, visibleColumns = viewport.width)
    )
    val initial = AppState.initial(AppConfig.conventional)
    initial.copy(
      persisted = initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = initial.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        ),
        focus = Focus.EditorPane(paneId),
        theme = Theme.light,
        config = config
      ),
      runtime = initial.runtime.copy(
        viewportSize = Some(viewport),
        uiSurfaces = List(
          UiSurface(
            SurfaceId("popup"),
            SurfaceContent.QuickInfo("hover text"),
            SurfacePresentation.Floating(Some(cursor), placement)
          )
        )
      )
    )

  private def planOf(state: AppState): EditorPaneRenderPlan =
    val caches = RenderCaches.create()
    val image  = SkiaFrames.newFrame(logicalW, logicalH)
    val surface =
      SkiaFrames.surfaceOver(image, metrics, codeFont, logicalW, logicalH, 1.0, persists = false, caches)
    RendererFramePlanner
      .renderFrame(
        state,
        cursorVisible = false,
        surface,
        viewport,
        caches.authoritativeScene.forState(state, viewport, codeFont, textFont),
        codeFont,
        textFont,
        codeFont,
        metrics,
        metrics,
        None,
        None,
        Damage.Everything,
        caches
      )
      .getOrElse(fail("expected an editor frame"))

  /** The cell row holding the top of the caret as the renderer paints it. */
  private def caretRow(plan: EditorPaneRenderPlan, cursor: CursorPosition): Int =
    val snapshot = plan.snapshots(paneId)
    val content  = plan.paneLayouts(paneId).contentRect
    val rowIndex = snapshot.navigationGeometry
      .visualRowIndexFor(cursor)
      .getOrElse(fail("caret is not on a laid-out row"))
    val rows = TextRowMetrics.forSnapshot(content, metrics, snapshot, measured = snapshot.usesMeasuredLayout)
    metrics.toRow(rows.cursorTopPx(rowIndex))

  private val cursor = CursorPosition(4, 3)

  "A popup below the caret" should "start under the row the caret is painted in, with heading space above" in {
    val plan = planOf(stateOf(spaced, cursor, SurfacePlacement.BelowCursor))
    val plain = planOf(
      stateOf(
        AppConfig.conventional.withoutStatusLine.withLineNumbers(false),
        CursorPosition(0, 3),
        SurfacePlacement.BelowCursor
      )
    )
    val gap = plain.layoutContract.belowCursorOverlayRects.head._2.y - caretRow(plain, CursorPosition(0, 3)) - 1

    plan.layoutContract.belowCursorOverlayRects.head._2.y shouldBe caretRow(plan, cursor) + 1 + gap
  }
