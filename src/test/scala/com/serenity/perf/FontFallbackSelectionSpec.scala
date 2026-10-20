package com.serenity.perf

import java.awt.image.BufferedImage
import java.awt.{Font, GraphicsEnvironment}
import java.nio.file.Paths
import java.util.concurrent.atomic.AtomicReference

import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import com.serenity.config.{AppConfig, AppMode}
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, Java2DRenderSurface, RendererFramePlanner, ScreenIdentity}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866: a selection repaints the text it covers in the selection colours, and that text is drawn in the fonts its row
  * measured it in. The fallback is a test subset font whose 中 is a solid box, so a glyph drawn in the primary font's
  * `.notdef` instead shows far less ink than the same glyph unselected.
  */
class FontFallbackSelectionSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private val logicalW = 600
  private val logicalH = 200
  private val theme    = Theme.dark

  private val subset =
    Font.createFont(
      Font.TRUETYPE_FONT,
      Paths.get(getClass.getResource("/fonts/SerenityFallbackTest.ttf").toURI).toFile
    )

  private val _ = GraphicsEnvironment.getLocalGraphicsEnvironment.registerFont(subset)

  private val codeFont =
    Font
      .createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource("/fonts/MonaspaceNeon-Regular.otf").toURI).toFile)
      .deriveFont(12.0f)

  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)
  private val viewport    = ViewportSize(logicalW / cellMetrics.charWidth, logicalH / cellMetrics.lineHeight)

  private def frame(selection: Option[(Int, Int)]): BufferedImage =
    val config =
      AppConfig.conventional
        .withAppMode(AppMode.Code)
        .withFontConfig(FontConfig(fallbackFamilies = List(subset.getFamily)))
    val (manager, id) = LaptopFrameBenchmarks.stateManagerHolding("ab中cd", config, grid = viewport)
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

  private def pixels(image: BufferedImage): IndexedSeq[(Int, Int, Int)] =
    for
      y <- 0 until image.getHeight
      x <- 0 until image.getWidth
    yield (x, y, image.getRGB(x, y) & 0xffffff)

  private def rgb(color: com.serenity.ui.color.RenderColor): Int = color.toAwt.getRGB & 0xffffff

  "A fallback glyph under a selection" should "keep the ink it has unselected" in {
    val selected   = frame(Some(2 -> 3))
    val unselected = frame(None)
    val changed =
      pixels(selected).zip(pixels(unselected)).collect { case ((x, y, now), (_, _, before)) if now != before => (x, y) }
    changed should not be empty
    val (left, right) = (changed.map(_._1).min, changed.map(_._1).max)
    val (top, bottom) = (changed.map(_._2).min, changed.map(_._2).max)
    def inked(image: BufferedImage, colour: Int): Int =
      pixels(image).count((x, y, now) => x >= left && x <= right && y >= top && y <= bottom && now == colour)
    val selectedInk   = inked(selected, rgb(theme.highlighted.foreground))
    val unselectedInk = inked(unselected, rgb(theme.foreground))
    info(s"selected=$selectedInk unselected=$unselectedInk box=($left,$top)-($right,$bottom)")
    unselectedInk should be > 20
    selectedInk should be > (unselectedInk * 0.8).toInt
  }
