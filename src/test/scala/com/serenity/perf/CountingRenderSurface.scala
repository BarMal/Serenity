package com.serenity.perf

import java.awt.font.FontRenderContext

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.PixelRect
import com.serenity.ui.renderer.{
  Effects,
  FontSpec,
  FrameSurfaceFactory,
  FrameSurfaceRequest,
  HardwareCursor,
  LayerBufferSupport,
  LiftedPixels,
  PanelOutlineDrawing,
  PixelCopying,
  PixelDrawing,
  PixelLifting,
  RenderImage,
  RenderSurface,
  ScreenIdentity,
  SurfaceActivity,
  SurfaceContentIdentity,
  TextDrawing
}
import com.serenity.ui.theme.TextStyle

enum DrawKind:

  case PutString, FillRect, DrawRunPx, DrawShapedRunPx, DrawLabelPx, FillPixelRect, FillPixelRects, DrawImage, CompositeLayer,
    CompositeFullSurfaceLayer, ClipPush, ClipPop, StrokeRect, SetAlpha, CopyPixels, Lift, PutBack, SetFont,
    PixelTranslation, FallbackFamily, LogicalPixelRow, ClearViewport, ClearViewportExcept, Flush

/** Single-threaded tallies, one slot per [[DrawKind]], because a frame is drawn on one thread. */
final class DrawCounts:
  private val tallies = new Array[Long](DrawKind.values.length)

  def increment(kind: DrawKind): Unit = tallies(kind.ordinal) += 1L

  def reset(): Unit = java.util.Arrays.fill(tallies, 0L)

  def snapshot: Map[DrawKind, Long] = DrawKind.values.map(kind => kind -> tallies(kind.ordinal)).toMap

  def total: Long = tallies.sum

/** Counts the calls a frame makes and forwards each unchanged to `inner`, including the members whose defaults call
  * other members, so a surface that overrides them (Skia's shaped runs and sprites) keeps its own paths.
  */
final class CountingRenderSurface(inner: RenderSurface, counts: DrawCounts)
    extends RenderSurface
    with TextDrawing
    with PixelDrawing
    with Effects
    with PanelOutlineDrawing:

  def text: TextDrawing                                   = this
  def pixels: PixelDrawing                                = this
  override def effects: Option[Effects]                   = inner.effects.map(_ => this)
  override def panelOutlines: Option[PanelOutlineDrawing] = inner.panelOutlines.map(_ => this)

  override def pixelCopying: Option[PixelCopying] = inner.pixelCopying.map(CountingCopying(_, counts))
  override def pixelLifting: Option[PixelLifting] = inner.pixelLifting.map(CountingLifting(_, counts))

  override def persistentContentKey: Option[SurfaceContentIdentity] = inner.persistentContentKey
  override def layerCacheOwner: ScreenIdentity                      = inner.layerCacheOwner
  override def addressesPixels: Boolean                             = inner.addressesPixels
  override def devicePixelScaleX: Double                            = inner.devicePixelScaleX
  override def devicePixelScaleY: Double                            = inner.devicePixelScaleY
  override def activity: SurfaceActivity                            = inner.activity
  override def layerBuffers: Option[LayerBufferSupport]             = inner.layerBuffers
  override def hardwareCursor: Option[HardwareCursor]               = inner.hardwareCursor
  override def fontRenderContext: Option[FontRenderContext]         = inner.text.fontRenderContext
  def viewportWidth: Int                                            = inner.viewportWidth
  def viewportHeight: Int                                           = inner.viewportHeight

  def setForegroundColor(color: RenderColor): Unit = inner.setForegroundColor(color)
  def setBackgroundColor(color: RenderColor): Unit = inner.setBackgroundColor(color)
  def getBackgroundColor: RenderColor              = inner.getBackgroundColor
  def enableStyle(style: TextStyle): Unit          = inner.enableStyle(style)
  def disableStyle(style: TextStyle): Unit         = inner.disableStyle(style)
  def hideCursor(): Unit                           = inner.hideCursor()

  override def clearViewport(color: RenderColor): Unit =
    counts.increment(DrawKind.ClearViewport)
    inner.clearViewport(color)

  override def clearViewportExcept(color: RenderColor, preserved: List[PixelRect]): Unit =
    counts.increment(DrawKind.ClearViewportExcept)
    inner.clearViewportExcept(color, preserved)

  def putString(x: Int, y: Int, s: String): Unit =
    counts.increment(DrawKind.PutString)
    inner.putString(x, y, s)

  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit =
    counts.increment(DrawKind.FillRect)
    inner.fillRect(x, y, width, height, char)

  def flush(): Unit =
    counts.increment(DrawKind.Flush)
    inner.flush()

  def setFont(font: FontSpec): Unit =
    counts.increment(DrawKind.SetFont)
    inner.text.setFont(font)

  def drawRunPx(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    s: String,
    clipGlyphToRun: Boolean = false
  ): Unit =
    counts.increment(DrawKind.DrawRunPx)
    inner.text.drawRunPx(xPx, yPx, bgWidthPx, lineHeightPx, ascentPx, s, clipGlyphToRun)

  override def shapesStretchesOf(text: String): Boolean = inner.text.shapesStretchesOf(text)

  override def drawShapedRunPx(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    stretch: String,
    from: Int,
    until: Int,
    clipGlyphToRun: Boolean = false
  ): Unit =
    counts.increment(DrawKind.DrawShapedRunPx)
    inner.text.drawShapedRunPx(xPx, yPx, bgWidthPx, lineHeightPx, ascentPx, stretch, from, until, clipGlyphToRun)

  override def drawLabelPx(xPx: Float, yPx: Int, bgWidthPx: Float, lineHeightPx: Int, ascentPx: Int, s: String): Unit =
    counts.increment(DrawKind.DrawLabelPx)
    inner.text.drawLabelPx(xPx, yPx, bgWidthPx, lineHeightPx, ascentPx, s)

  override def withFallbackFamily(family: String, ligaturesOn: Boolean = false)(draw: => Unit): Unit =
    counts.increment(DrawKind.FallbackFamily)
    inner.text.withFallbackFamily(family, ligaturesOn)(draw)

  def withLogicalPixelRow(cellRow: Int, pixelY: Int)(render: => Unit): Unit =
    counts.increment(DrawKind.LogicalPixelRow)
    inner.text.withLogicalPixelRow(cellRow, pixelY)(render)

  def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: RenderColor): Unit =
    counts.increment(DrawKind.FillPixelRect)
    inner.pixels.fillPixelRect(xPx, yPx, widthPx, heightPx, color)

  override def fillPixelRects(rects: IArray[Int], color: RenderColor): Unit =
    counts.increment(DrawKind.FillPixelRects)
    inner.pixels.fillPixelRects(rects, color)

  def drawImage(image: RenderImage, x: Int, y: Int, width: Int, height: Int): Unit =
    counts.increment(DrawKind.DrawImage)
    inner.pixels.drawImage(image, x, y, width, height)

  override def compositeFullSurfaceLayer(image: RenderImage): Unit =
    counts.increment(DrawKind.CompositeFullSurfaceLayer)
    inner.pixels.compositeFullSurfaceLayer(image)

  override def compositeLayer(image: RenderImage, bounds: PixelRect): Unit =
    counts.increment(DrawKind.CompositeLayer)
    inner.pixels.compositeLayer(image, bounds)

  def withPixelTranslation(xPx: Double, yPx: Double)(render: => Unit): Unit =
    counts.increment(DrawKind.PixelTranslation)
    inner.pixels.withPixelTranslation(xPx, yPx)(render)

  def setAlpha(alpha: Float): Unit =
    counts.increment(DrawKind.SetAlpha)
    inner.effects.foreach(_.setAlpha(alpha))

  def strokeRect(x: Int, y: Int, width: Int, height: Int, color: RenderColor, strokeWidth: Float): Unit =
    counts.increment(DrawKind.StrokeRect)
    inner.panelOutlines.foreach(_.strokeRect(x, y, width, height, color, strokeWidth))

  def withRectClip(x: Int, y: Int, width: Int, height: Int)(render: => Unit): Unit =
    counts.increment(DrawKind.ClipPush)
    try inner.panelOutlines.fold(render)(_.withRectClip(x, y, width, height)(render))
    finally counts.increment(DrawKind.ClipPop)

final private class CountingCopying(inner: PixelCopying, counts: DrawCounts) extends PixelCopying:

  def copyPixels(from: PixelRect, dyPx: Int): Unit =
    counts.increment(DrawKind.CopyPixels)
    inner.copyPixels(from, dyPx)

final private class CountingLifting(inner: PixelLifting, counts: DrawCounts) extends PixelLifting:

  def lift(area: PixelRect, spare: Option[LiftedPixels]): LiftedPixels =
    counts.increment(DrawKind.Lift)
    inner.lift(area, spare)

  def putBack(lifted: LiftedPixels): Unit =
    counts.increment(DrawKind.PutBack)
    inner.putBack(lifted)

final class CountingFactory(delegate: FrameSurfaceFactory, counts: DrawCounts) extends FrameSurfaceFactory:
  def create(request: FrameSurfaceRequest): RenderSurface =
    CountingRenderSurface(delegate.create(request), counts)
