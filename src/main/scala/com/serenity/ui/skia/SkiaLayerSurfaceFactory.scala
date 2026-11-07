package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicReference
import javax.swing.JPanel

import scala.util.{Failure, Success, Try}

import com.serenity.ui.renderer.{FontSpec, FrameSurfaceFactory, FrameSurfaceRequest, RenderSurface}
import com.serenity.ui.terminal.SwingPresentationSite
import org.jetbrains.skia.{PictureRecorder, Surface}
import org.slf4j.LoggerFactory

/** Paints a window's frames through a GPU layer: each frame is recorded as a Skia `Picture` and handed to the window's
  * [[LayerHost]], which replays it on the event thread. A frame redraws everything, as a surface with no persistent
  * pixels must.
  *
  * If the layer cannot be made (no GL context, a native failure) the window keeps working: this window's frames go to
  * `fallback` from then on, instead of a blank canvas.
  */
class LayerSurfaceFactory private[ui] (
    buildHost: JPanel => LayerHost,
    fallback: FrameSurfaceFactory = FrameSurfaceFactory.Java2D,
    frames: LayerFrames = LayerFrames.Picture
) extends FrameSurfaceFactory:

  def create(request: FrameSurfaceRequest): RenderSurface =
    request.site match
      case SwingPresentationSite(canvas) =>
        LayerSurfaceFactory.WindowLayer
          .of(canvas)
          .host(request, canvas, buildHost)
          .fold(fallback.create(request))(host =>
            frames match
              case LayerFrames.Picture => record(request, host)
              case LayerFrames.Image   => paint(request, host)
          )
      case _ => fallback.create(request)

  /** Paints the frame on a raster surface of its own and publishes a snapshot of it. The surface is closed at once; the
    * snapshot keeps the pixels, so the event thread can draw frame N while frame N+1 is being painted.
    */
  private def paint(request: FrameSurfaceRequest, host: LayerHost): RenderSurface =
    val geometry = request.geometry
    val scale    = geometry.deviceScale
    val surface  = Surface.Companion.makeRaster(RasterFrames.frameInfo(geometry.deviceWidthPx, geometry.deviceHeightPx))
    SkiaRenderSurface.retaining(
      surface,
      request.metrics,
      FontSpec.fromAwt(request.font),
      geometry.logicalWidthPx,
      geometry.logicalHeightPx,
      scale.x,
      SkiaTextCaches.of(request.backendCaches),
      request.textCaches.styledFonts,
      onFlush = () =>
        val image = surface.makeImageSnapshot()
        surface.close()
        host.publishImage(image)
      ,
      layerCacheOwner = Some(geometry.identity)
    )

  private def record(request: FrameSurfaceRequest, host: LayerHost): RenderSurface =
    val geometry      = request.geometry
    val logicalWidth  = geometry.logicalWidthPx
    val logicalHeight = geometry.logicalHeightPx
    val scale         = geometry.deviceScale
    val recorder      = new PictureRecorder()
    val recording     = PictureRecording.begin(recorder, geometry.deviceWidthPx, geometry.deviceHeightPx)
    SkiaRenderSurface.onCanvas(
      recording,
      request.metrics,
      FontSpec.fromAwt(request.font),
      logicalWidth,
      logicalHeight,
      scale.x,
      SkiaTextCaches.of(request.backendCaches),
      request.textCaches.styledFonts,
      onFlush = () =>
        val picture = recorder.finishRecordingAsPicture()
        recorder.close()
        host.publish(picture)
      ,
      layerCacheOwner = Some(geometry.identity)
    )

/** Frames for a window whose layer could not be made: painted by Skia on the CPU, and by Java2D only if that cannot
  * make a surface either.
  */
final class RasterThenJava2D(raster: FrameSurfaceFactory) extends FrameSurfaceFactory:

  def create(request: FrameSurfaceRequest): RenderSurface =
    Try(raster.create(request)).getOrElse(FrameSurfaceFactory.Java2D.create(request))

/** The factory a window uses: a Skiko layer made on its canvas, with the Skia raster surface behind it. */
object SkiaLayerSurfaceFactory
    extends LayerSurfaceFactory(
      canvas => SkiaLayerHost.create(canvas),
      new RasterThenJava2D(SkiaRasterSurfaceFactory),
      LayerFrames.configured
    )

/** For a timing run: asks the layer to wait for the GPU after each draw, which Skiko only allows where the canvas has a
  * GPU context; it has none, so this draws like [[SkiaLayerSurfaceFactory]], and the layer's draw stats say so.
  */
object SynchronousSkiaLayerSurfaceFactory
    extends LayerSurfaceFactory(
      canvas => SkiaLayerHost.create(canvas, syncGpu = true),
      frames = LayerFrames.configured
    )

object LayerSurfaceFactory:

  private val log = LoggerFactory.getLogger(classOf[LayerSurfaceFactory])

  /** One window's layer, made when its first frame is drawn, or the record that it could not be. It belongs to the
    * canvas, not to the caches a frame is drawn with: a frame may come with caches of its own, but the canvas holds one
    * heavyweight layer, and a second would be repainted along with the first by every publish.
    */
  final private[skia] class WindowLayer:

    private enum State:
      case Unbuilt
      case Ready(host: LayerHost)
      case Failed

    private val state = new AtomicReference[State](State.Unbuilt)

    def host(request: FrameSurfaceRequest, canvas: JPanel, build: JPanel => LayerHost): Option[LayerHost] =
      state.get() match
        case State.Ready(host) => Some(host)
        case State.Failed      => None
        case State.Unbuilt =>
          Try(build(canvas)) match
            case Success(host) =>
              state.set(State.Ready(host))
              request.attachLayer(host)
              Some(host)
            case Failure(error) =>
              state.set(State.Failed)
              log.warn("The Skia layer could not be made; this window paints with the fallback surface", error)
              None

  object WindowLayer:

    private val Property = "serenity.skia.windowLayer"

    /** The canvas's layer holder, made on first use. */
    def of(canvas: JPanel): WindowLayer =
      classOf[WindowLayer].synchronized {
        canvas.getClientProperty(Property) match
          case existing: WindowLayer => existing
          case _ =>
            val made = new WindowLayer
            canvas.putClientProperty(Property, made)
            made
      }
