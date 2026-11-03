package com.serenity.perf

import java.awt.font.FontRenderContext
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{PixelRect, TextLayoutSnapshot}
import com.serenity.ui.renderer.{
  Effects,
  FontSpec,
  FrameSurfaceFactory,
  FrameSurfaceRequest,
  Java2DRenderSurface,
  PanelOutlineDrawing,
  PixelDrawing,
  RenderImage,
  RenderSurface,
  ScreenIdentity,
  SurfaceContentIdentity,
  TextDrawing
}
import com.serenity.ui.theme.TextStyle

/** The floor of a frame's cost: accepts every draw call and paints nothing, so the renderer's own work can be timed
  * without a rasteriser. It claims what the Skia surface claims (device pixels, persistent content, measured text) so
  * the renderer takes the same paths.
  */
final class NullRenderSurface(
    val viewportWidth: Int,
    val viewportHeight: Int,
    deviceScale: Double,
    onFlush: () => Unit
) extends RenderSurface
    with TextDrawing
    with PixelDrawing
    with Effects
    with PanelOutlineDrawing:

  private val contentIdentity = SurfaceContentIdentity(new Object)
  private val screenIdentity  = ScreenIdentity(new Object)
  private val renderContext   = Some(TextLayoutSnapshot.defaultFontRenderContext())
  private val foreground      = new AtomicReference(RenderColor.White)
  private val background      = new AtomicReference(RenderColor.Black)

  def text: TextDrawing                                   = this
  def pixels: PixelDrawing                                = this
  override def effects: Option[Effects]                   = Some(this)
  override def panelOutlines: Option[PanelOutlineDrawing] = Some(this)

  override def persistentContentKey: Option[SurfaceContentIdentity] = Some(contentIdentity)
  override def layerCacheOwner: ScreenIdentity                      = screenIdentity
  override def addressesPixels: Boolean                             = true
  override def devicePixelScaleX: Double                            = deviceScale
  override def devicePixelScaleY: Double                            = deviceScale
  override def fontRenderContext: Option[FontRenderContext]         = renderContext

  def setForegroundColor(color: RenderColor): Unit = foreground.set(color)
  def setBackgroundColor(color: RenderColor): Unit = background.set(color)
  def getBackgroundColor: RenderColor              = background.get

  override def clearViewport(color: RenderColor): Unit                                   = setBackgroundColor(color)
  override def clearViewportExcept(color: RenderColor, preserved: List[PixelRect]): Unit = setBackgroundColor(color)

  def putString(x: Int, y: Int, s: String): Unit                          = ()
  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit = ()
  def enableStyle(style: TextStyle): Unit                                 = ()
  def disableStyle(style: TextStyle): Unit                                = ()
  def hideCursor(): Unit                                                  = ()
  def flush(): Unit                                                       = onFlush()

  def setFont(font: FontSpec): Unit = ()

  def drawRunPx(
    xPx: Float,
    yPx: Int,
    bgWidthPx: Float,
    lineHeightPx: Int,
    ascentPx: Int,
    s: String,
    clipGlyphToRun: Boolean = false
  ): Unit = ()

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
  ): Unit = ()

  override def drawLabelPx(xPx: Float, yPx: Int, bgWidthPx: Float, lineHeightPx: Int, ascentPx: Int, s: String): Unit =
    ()

  override def withFallbackFamily(family: String, ligaturesOn: Boolean = false)(draw: => Unit): Unit = draw
  def withLogicalPixelRow(cellRow: Int, pixelY: Int)(render: => Unit): Unit                          = render

  def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: RenderColor): Unit = ()
  def drawImage(image: RenderImage, x: Int, y: Int, width: Int, height: Int): Unit             = ()
  override def compositeFullSurfaceLayer(image: RenderImage): Unit                             = ()
  override def compositeLayer(image: RenderImage, bounds: PixelRect): Unit                     = ()
  def withPixelTranslation(xPx: Double, yPx: Double)(render: => Unit): Unit                    = render

  def setAlpha(alpha: Float): Unit = ()

  def strokeRect(x: Int, y: Int, width: Int, height: Int, color: RenderColor, strokeWidth: Float): Unit = ()
  def withRectClip(x: Int, y: Int, width: Int, height: Int)(render: => Unit): Unit                      = render

/** Hands every frame the same [[NullRenderSurface]], as a persistent raster surface would be, and publishes a blank
  * image of the window's device size when the frame flushes.
  */
final class NullSurfaceFactory extends FrameSurfaceFactory:

  private val surface = new AtomicReference(Option.empty[NullRenderSurface])
  private val publish = new AtomicReference[() => Unit](() => ())

  def create(request: FrameSurfaceRequest): RenderSurface =
    val canvas        = request.canvas
    val logicalWidth  = Java2DRenderSurface.logicalCanvasDimension(canvas.getWidth, canvas.getPreferredSize.width)
    val logicalHeight = Java2DRenderSurface.logicalCanvasDimension(canvas.getHeight, canvas.getPreferredSize.height)
    val scale         = Java2DRenderSurface.deviceScaleFor(canvas)
    val widthPx       = Java2DRenderSurface.deviceImageDimension(logicalWidth, scale.x)
    val heightPx      = Java2DRenderSurface.deviceImageDimension(logicalHeight, scale.y)
    publish.set { () =>
      val image = request.acquireImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)
      request.onFlush(image)
    }
    surface.get.getOrElse {
      val created = new NullRenderSurface(
        logicalWidth / request.metrics.charWidth,
        logicalHeight / request.metrics.lineHeight,
        scale.x,
        () => publish.get.apply()
      )
      surface.set(Some(created))
      created
    }
