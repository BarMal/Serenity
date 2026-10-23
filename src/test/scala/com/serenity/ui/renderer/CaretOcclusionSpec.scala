package com.serenity.ui.renderer

import java.awt.image.BufferedImage
import java.awt.{Color, Rectangle}

import com.serenity.TestWorkspaceTrees
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, PixelRect, ViewportSize}
import com.serenity.ui.terminal.SwingWindow.CaretPaint
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The caret is filled over the presented frame, above everything in it, so a surface floating over the text would
  * have the caret drawn across it. The caret leaves out the pixels a floating surface covers.
  */
class CaretOcclusionSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val white = Color.WHITE

  "CaretOcclusion.visible" should "leave a caret nothing covers as it is" in {
    val carets = List(CaretPaint(new Rectangle(10, 0, 2, 20), white))

    CaretOcclusion.visible(carets, List(PixelRect(100, 100, 50, 50))) shouldBe carets
  }

  it should "drop a caret that lies wholly under a surface" in {
    val carets = List(CaretPaint(new Rectangle(10, 10, 2, 20), white))

    CaretOcclusion.visible(carets, List(PixelRect(0, 0, 100, 100))) shouldBe Nil
  }

  it should "keep only the part of a caret a surface does not cover, in the caret's colour" in {
    val red    = new Color(200, 0, 0)
    val carets = List(CaretPaint(new Rectangle(10, 0, 2, 20), red))

    CaretOcclusion.visible(carets, List(PixelRect(0, 10, 100, 100))) shouldBe
      List(CaretPaint(new Rectangle(10, 0, 2, 10), red))
  }

  it should "judge each caret on its own" in {
    val covered = CaretPaint(new Rectangle(10, 10, 2, 20), white)
    val clear   = CaretPaint(new Rectangle(300, 10, 2, 20), white)

    CaretOcclusion.visible(List(covered, clear), List(PixelRect(0, 0, 100, 100))) shouldBe List(clear)
  }

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val fonts    = FontLoader.FontConfig()
  private val codeFont = FontLoader.previewCodeFont(fonts)
  private val textFont = FontLoader.previewTextFont(fonts)
  private val metrics  = CellMetrics.fromFont(codeFont)
  private val logicalW = 900
  private val logicalH = 600
  private val viewport = ViewportSize(logicalW / metrics.charWidth, logicalH / metrics.lineHeight)
  private val cursor   = CursorPosition(2, 3)

  private def stateWithPopup(placement: SurfacePlacement): AppState =
    val buffer = Buffer
      .fromString(bufferId, List.fill(12)("a line of text for the caret to sit in").mkString("\n"))
      .copy(
        editing = EditingState(List(cursor)),
        viewport =
          Viewport(topLine = 0, leftColumn = 0, visibleLines = viewport.height, visibleColumns = viewport.width)
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
        theme = Theme.light
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
    val image = new BufferedImage(logicalW, logicalH, BufferedImage.TYPE_INT_RGB)
    val surface = new Java2DRenderSurface(
      image,
      metrics,
      codeFont,
      _ => (),
      logicalWidthPx = logicalW,
      logicalHeightPx = logicalH,
      contentPersists = false,
      layerCacheOwnerOverride = Some(ScreenIdentity(image))
    )
    val caches = RenderCaches.create()
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

  "CaretOcclusion.obstructions" should "cover the pixels of a popup floating below the caret" in {
    val plan  = planOf(stateWithPopup(SurfacePlacement.BelowCursor))
    val popup = RendererChromePlan.pixelRectOf(metrics)(plan.layoutContract.belowCursorOverlayRects.head._2)

    val centre = (popup.xPx + popup.widthPx / 2, popup.yPx + popup.heightPx / 2)

    CaretOcclusion.obstructions(plan, metrics).exists(_.contains(centre._1, centre._2)) shouldBe true
  }

  it should "cover the pixels of a popup floating above the caret" in {
    val plan  = planOf(stateWithPopup(SurfacePlacement.AboveCursor))
    val popup = RendererChromePlan.pixelRectOf(metrics)(plan.layoutContract.aboveCursorOverlayRects.head._2)

    val centre = (popup.xPx + popup.widthPx / 2, popup.yPx + popup.heightPx / 2)

    CaretOcclusion.obstructions(plan, metrics).exists(_.contains(centre._1, centre._2)) shouldBe true
  }
