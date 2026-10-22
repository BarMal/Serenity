package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage
import java.nio.file.Paths
import java.util.concurrent.atomic.AtomicReference

import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, AppMode}
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, Java2DRenderSurface, RendererFramePlanner, ScreenIdentity}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #2365: a selection over an emoji cluster on the cell grid paints the cluster's two cells as one unit, in the same
  * place its glyph is drawn, and touches nothing beside it: the cluster is not cut into pieces, one per codepoint, each
  * placed on its own caret stop.
  */
class GuiCellGridClusterSelectionSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val logicalW = 600
  private val logicalH = 200
  private val theme    = Theme.dark

  private val codeFont =
    Font
      .createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource("/fonts/CourierPrime-Regular.ttf").toURI).toFile)
      .deriveFont(10.0f)

  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)
  private val viewport    = ViewportSize(logicalW / cellMetrics.charWidth, logicalH / cellMetrics.lineHeight)

  private def s(codePoints: Int*): String = String(codePoints.toArray, 0, codePoints.length)

  private def frame(text: String, selection: Option[(Int, Int)]): BufferedImage =
    val config        = AppConfig.conventional.withAppMode(AppMode.Code)
    val (manager, id) = LaptopFrameBenchmarks.stateManagerHolding(text, config, grid = viewport)
    val base          = manager.getCurrentState.unsafeRunSync()
    val cursor =
      selection.fold(Cursor(CursorPosition(0, 0))) { (from, until) =>
        Cursor(CursorPosition(0, until), selectionAnchor = Some(CursorPosition(0, from)))
      }
    val state =
      base.copy(persisted =
        base.persisted.copy(
          theme = theme,
          buffers = base.persisted.buffers.updatedWith(id)(
            _.map(buffer => buffer.copy(editing = EditingState(NonEmptyList.one(cursor))))
          )
        )
      )
    val image  = new BufferedImage(logicalW, logicalH, BufferedImage.TYPE_INT_RGB)
    val caches = RenderCaches.create()
    val screen = ScreenIdentity(this)
    val surface = new Java2DRenderSurface(
      image,
      cellMetrics,
      codeFont,
      _ => (),
      logicalWidthPx = logicalW,
      logicalHeightPx = logicalH,
      deviceScaleX = 1.0,
      deviceScaleY = 1.0,
      contentPersists = true,
      layerCacheOwnerOverride = Some(screen)
    )
    val _ = RendererFramePlanner.renderFrame(
      state,
      cursorVisible = false,
      surface,
      viewport,
      caches.authoritativeScene.forState(state, viewport, codeFont, codeFont),
      codeFont,
      codeFont,
      uiFont,
      cellMetrics,
      uiMetrics,
      None,
      Some(FrameOutput(screen, new AtomicReference(None))),
      Damage.Everything,
      caches = caches
    )
    image

  private val clusters = Vector(
    "variation selector 16" -> s(0x2764, 0xfe0f),
    "emoji modifier"        -> s(0x1f44d, 0x1f3fd),
    "joined family"         -> s(0x1f468, 0x200d, 0x1f469, 0x200d, 0x1f467),
    "flag"                  -> s(0x1f1fa, 0x1f1f8)
  )

  "A selection over an emoji cluster on the cell grid" should "change exactly the cluster's two cells" in {
    val cell = cellMetrics.charWidth
    clusters.foreach { (name, cluster) =>
      val text       = "a" + cluster + "b"
      val selected   = frame(text, Some(1 -> (1 + cluster.length)))
      val unselected = frame(text, None)
      val changed = for
        y <- 0 until selected.getHeight
        x <- 0 until selected.getWidth
        if (selected.getRGB(x, y) & 0xffffff) != (unselected.getRGB(x, y) & 0xffffff)
      yield (x, y)
      // The first row that changed is the text line's; whatever else a selection changes (a status line) is further down.
      val top    = changed.map(_._2).min
      val inLine = changed.collect { case (x, y) if y < top + cellMetrics.lineHeight => x }
      withClue(name) {
        info(s"$name: top=$top width=${inLine.max - inLine.min + 1}")
        (inLine.max - inLine.min + 1) shouldBe (2 * cell) +- 1
      }
    }
  }
