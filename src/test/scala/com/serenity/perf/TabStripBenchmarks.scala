package com.serenity.perf

import java.awt.Font
import java.awt.image.BufferedImage

import com.serenity.config.{AppConfig, DockedCornerStyle}
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.{AppState, Buffer, BufferId, TabListEntry, UiSurface}
import com.serenity.ui.layout.{CellMetrics, LayoutRect, TabBarLayoutSettings, TabBarSurfaceComposition}
import com.serenity.ui.renderer.{
  Java2DRenderSurface,
  MeasuredTextAdvances,
  ShapedTextCache,
  TextOverlayRenderer,
  TextOverlayView
}
import com.serenity.ui.theme.Theme

/** The tab strip as a GUI draws it with rounded corners, which every change to the strip repaints, and as it is laid
  * out, which every pointer move over the strip does again. Ten tabs fit the strip as it is; twenty-four overflow it.
  */
private[perf] object TabStripBenchmarks:

  given Balance = Balance.default

  private val widthPx  = 1200
  private val heightPx = 60
  private val font     = Font(Font.MONOSPACED, Font.PLAIN, 14)
  private val cells    = CellMetrics.fromFont(font)
  private val columns  = widthPx / cells.charWidth
  private val rect     = LayoutRect(0, 0, columns, 1)
  private val advances = MeasuredTextAdvances(MeasuredTextAdvances.DefaultCapacity, ShapedTextCache())
  private val config = AppConfig.conventional.withUiCornerRadiusPx(12).withDockedCornerStyle(DockedCornerStyle.Floating)

  private def stateWith(count: Int): AppState =
    val ids     = (1 to count).map(BufferId(_)).toList
    val base    = AppState.initial(config)
    val buffers = ids.map(id => Buffer.fromString(id, s"buffer $id"))
    base.copy(persisted = base.persisted.copy(buffers = buffers.map(b => b.id -> b).toMap, bufferOrder = ids))

  private def entriesOf(count: Int): List[TabListEntry] =
    (1 to count).toList.map(id => TabListEntry(BufferId(id), s"Quarterly notes $id", isDirty = id % 3 == 0))

  private def repaint(count: Int): () => BufferedImage =
    val state   = stateWith(count)
    val image   = new BufferedImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)
    val caches  = RenderCaches.create()
    val surface = new Java2DRenderSurface(image, cells, font, _ => (), textCaches = caches.surfaceText)
    val active  = Some(BufferId(count))
    val overlay = TextOverlayView(
      rect = rect,
      composition =
        Some(TabBarSurfaceComposition.forTabBar(entriesOf(count), active, rect, TabBarLayoutSettings.of(state))),
      surfaceId = Some(UiSurface.TabBarSurfaceId)
    )
    () =>
      TextOverlayRenderer.render(surface, overlay, Theme.light, config, false, font, cells, advances)
      image

  private def layout(count: Int): () => Int =
    val state   = stateWith(count)
    val entries = entriesOf(count)
    val active  = Some(BufferId(count))
    () =>
      val settings = TabBarLayoutSettings.of(state)
      TabBarSurfaceComposition.forTabBar(entries, active, rect, settings).hitRegions.size +
        TabBarSurfaceComposition.closeAffordances(entries, active, rect, settings).size

  private def hasPixels(image: BufferedImage): Boolean =
    (0 until image.getWidth).exists(x => image.getRGB(x, 6) != image.getRGB(0, 6))

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    List(10, 24).flatMap { count =>
      val painted = repaint(count)
      val laidOut = layout(count)
      List(
        BenchmarkRunner.Benchmark(
          s"render.tab_strip.repaint.${count}_tabs",
          3,
          12,
          () => assert(hasPixels(painted()), s"the $count-tab strip drew nothing"),
          () => painted()
        ),
        BenchmarkRunner.Benchmark(
          s"layout.tab_strip.${count}_tabs",
          3,
          12,
          () => assert(laidOut() > 0, s"the $count-tab strip laid out nothing"),
          () => laidOut()
        )
      )
    }
