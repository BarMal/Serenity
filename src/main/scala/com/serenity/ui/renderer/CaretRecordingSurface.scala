package com.serenity.ui.renderer

import java.awt.Color
import java.util.concurrent.atomic.AtomicReference

import com.serenity.state.models.CaretStyle
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}

/** A canvas-shaped surface that records caret fills instead of painting them, so the window can fill carets straight
  * over its presented frame rather than compositing a full-window overlay image. Everything else is the Skia surface it
  * wraps, drawn on a canvas of one pixel that nothing reads: carets are only ever drawn with `fillPixelRect` and
  * `fillCaret`, and the geometry the cursor path measures against is the canvas's own.
  */
final class CaretRecordingSurface private (inner: RenderSurface) extends RenderSurface:
  private val fills = new AtomicReference(Vector.empty[CaretRecordingSurface.CaretFill])

  private object Recording extends PixelDrawing:
    def fillPixelRect(xPx: Int, yPx: Int, widthPx: Int, heightPx: Int, color: RenderColor): Unit =
      fillCaret(xPx, yPx, widthPx, heightPx, color, CaretStyle.Plain)

    override def fillCaret(
      xPx: Int,
      yPx: Int,
      widthPx: Int,
      heightPx: Int,
      color: RenderColor,
      style: CaretStyle
    ): Unit =
      val _ = fills.updateAndGet(
        _ :+ CaretRecordingSurface.CaretFill(
          PixelRect(xPx, yPx, widthPx.max(1), heightPx.max(1)),
          color.toAwt,
          style
        )
      )

    def drawImage(image: RenderImage, x: Int, y: Int, width: Int, height: Int): Unit =
      inner.pixels.drawImage(image, x, y, width, height)

    def withPixelTranslation(xPx: Double, yPx: Double)(render: => Unit): Unit =
      inner.pixels.withPixelTranslation(xPx, yPx)(render)

  // A recorder keeps no pixels, so it offers no key: the renderer would store a whole prepared scene under the identity of
  // a surface that is new for every cursor frame, and never find it again.
  override def persistentContentKey: Option[SurfaceContentIdentity] = None
  override def layerCacheOwner: ScreenIdentity                      = inner.layerCacheOwner
  override def addressesPixels: Boolean                             = inner.addressesPixels
  override def devicePixelScaleX: Double                            = inner.devicePixelScaleX
  override def devicePixelScaleY: Double                            = inner.devicePixelScaleY

  def setForegroundColor(color: RenderColor): Unit = inner.setForegroundColor(color)
  def setBackgroundColor(color: RenderColor): Unit = inner.setBackgroundColor(color)
  def getBackgroundColor: RenderColor              = inner.getBackgroundColor

  override def clearViewport(color: RenderColor): Unit = inner.clearViewport(color)
  override def clearViewportExcept(color: RenderColor, preserved: List[PixelRect]): Unit =
    inner.clearViewportExcept(color, preserved)

  def putString(x: Int, y: Int, s: String): Unit                          = inner.putString(x, y, s)
  def fillRect(x: Int, y: Int, width: Int, height: Int, char: Char): Unit = inner.fillRect(x, y, width, height, char)
  def enableStyle(style: com.serenity.ui.theme.TextStyle): Unit           = inner.enableStyle(style)
  def disableStyle(style: com.serenity.ui.theme.TextStyle): Unit          = inner.disableStyle(style)

  def text: TextDrawing    = inner.text
  def pixels: PixelDrawing = Recording

  override def effects: Option[Effects]                   = inner.effects
  override def pixelCopying: Option[PixelCopying]         = inner.pixelCopying
  override def pixelLifting: Option[PixelLifting]         = inner.pixelLifting
  override def panelOutlines: Option[PanelOutlineDrawing] = inner.panelOutlines
  override def hardwareCursor: Option[HardwareCursor]     = inner.hardwareCursor
  override def activity: SurfaceActivity                  = inner.activity

  def hideCursor(): Unit  = inner.hideCursor()
  def viewportWidth: Int  = inner.viewportWidth
  def viewportHeight: Int = inner.viewportHeight

  def flush(): Unit = inner.flush()

  def recordedFills: List[CaretRecordingSurface.CaretFill] = fills.get().toList

object CaretRecordingSurface:

  final case class CaretFill(rect: PixelRect, color: Color, style: CaretStyle = CaretStyle.Plain)

  def forGeometry(
    canvases: CaretCanvases,
    metrics: CellMetrics,
    font: FontSpec,
    geometry: SurfaceGeometry
  ): CaretRecordingSurface =
    new CaretRecordingSurface(canvases.forGeometry(metrics, font, geometry))
