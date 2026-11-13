package com.serenity.ui.skia

import java.awt.image.BufferedImage

import scala.util.{Failure, Success, Try}

import com.serenity.ui.renderer.{
  BackendCacheKey,
  BackendCaches,
  DeviceArea,
  FontSpec,
  FrameSurfaceFactory,
  FrameSurfaceRequest,
  RenderSurface
}
import org.jetbrains.skia.Surface

/** Paints a window's frames with Skia's CPU rasteriser and hands the Swing canvas an ordinary image to present, so the
  * window keeps its Java2D canvas for input, input methods and accessibility.
  *
  * The owner's [[BackendCaches]] hold the raster surface, which is how a frame starts from the pixels the last one
  * left.
  */
object SkiaRasterSurfaceFactory extends FrameSurfaceFactory:

  private val FramesKey = BackendCacheKey(classOf[RasterFrames])

  /** A raster of the window's device size that holds nothing yet, in the order the window's own frames use. */
  def newSurface(widthPx: Int, heightPx: Int): Surface =
    Surface.Companion.makeRaster(RasterFrames.frameInfo(widthPx, heightPx))

  /** `FrameSurfaceFactory.create` has no failure channel, and a window with no raster has nothing else to paint with,
    * so the failure is thrown for the render loop's crash path to report.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def create(request: FrameSurfaceRequest): RenderSurface =
    Try(build(request)) match
      case Success(surface) => surface
      case Failure(error) =>
        throw SkiaRasterUnavailable(request.geometry.deviceWidthPx, request.geometry.deviceHeightPx, error)

  private def build(request: FrameSurfaceRequest): RenderSurface =
    val geometry      = request.geometry
    val logicalWidth  = geometry.logicalWidthPx
    val logicalHeight = geometry.logicalHeightPx
    val scale         = geometry.deviceScale
    val widthPx       = geometry.deviceWidthPx
    val heightPx      = geometry.deviceHeightPx
    val image         = request.acquireImage(widthPx, heightPx, BufferedImage.TYPE_INT_RGB)
    val frames        = request.backendCaches.getOrCreate(FramesKey)(new RasterFrames)
    val surface =
      if request.contentPersists then frames.surfaceFor(widthPx, heightPx, scale.x)
      else Surface.Companion.makeRaster(RasterFrames.frameInfo(widthPx, heightPx))
    val changes =
      Option.when(request.contentPersists)(ChangeRecorder(scale.x, scale.y, widthPx, heightPx, frames.phases))
    SkiaRenderSurface.retaining(
      surface,
      request.metrics,
      FontSpec.fromAwt(request.font),
      logicalWidth,
      logicalHeight,
      scale.x,
      SkiaTextCaches.of(request.backendCaches),
      request.textCaches.styledFonts,
      onFlush = () =>
        if request.contentPersists then
          val reported = request.changedRegion().map(_.map(DeviceArea.around(_, scale.x, scale.y, widthPx, heightPx)))
          val damage   = reported.map(areas => RasterCopyPlan.disjoint(areas ++ changes.fold(Nil)(_.paintRecords)))
          val moves    = changes.fold(Nil)(_.moves)
          frames.present(surface, image, damage, moves, changes.filter(_ => moves.nonEmpty).map(_.paint))
        else
          RasterFrames.copyWhole(surface, image)
          surface.close()
        request.onFlush(image)
      ,
      layerCacheOwner = Some(geometry.identity),
      changes = changes
    )

  /** How many device pixels the last frame `owner` presented had to copy, if it has presented one. */
  def lastCopiedPixels(owner: BackendCaches): Option[Long] =
    Option.when(owner.holds(FramesKey))(owner.getOrCreate(FramesKey)(new RasterFrames).copiedPixels)

  /** What the frames `owner` presented spent in each [[PresentPhase]] since this was last asked, if it has presented
    * any.
    */
  def drainPhases(owner: BackendCaches): Option[Map[PresentPhase, Long]] =
    Option.when(owner.holds(FramesKey))(owner.getOrCreate(FramesKey)(new RasterFrames).phases.drain())

  /** The surface `owner`'s frames are painted on, for a test to compare with what was presented. */
  private[serenity] def retainedSurface(owner: BackendCaches): Option[org.jetbrains.skia.Surface] =
    Option.when(owner.holds(FramesKey))(owner.getOrCreate(FramesKey)(new RasterFrames).retainedSurface).flatten
