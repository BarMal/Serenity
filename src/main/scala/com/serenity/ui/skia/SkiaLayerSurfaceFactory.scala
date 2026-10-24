package com.serenity.ui.skia

import java.util.concurrent.atomic.AtomicReference
import javax.swing.JPanel

import scala.util.{Failure, Success, Try}

import com.serenity.ui.renderer.{
  FontSpec,
  FrameSurfaceFactory,
  FrameSurfaceRequest,
  Java2DRenderSurface,
  RenderSurface,
  ScreenIdentity
}
import org.jetbrains.skia.PictureRecorder
import org.slf4j.LoggerFactory

/** Paints a window's frames through a GPU layer: each frame is recorded as a Skia `Picture` and handed to the window's
  * [[LayerHost]], which replays it on the event thread. A frame redraws everything, as a surface with no persistent
  * pixels must.
  *
  * If the layer cannot be made (no GL context, a native failure) the window keeps working: this window's frames go to
  * Java2D from then on, instead of a blank canvas.
  */
class LayerSurfaceFactory private[ui] (buildHost: JPanel => LayerHost) extends FrameSurfaceFactory:

  def create(request: FrameSurfaceRequest): RenderSurface =
    LayerSurfaceFactory.WindowLayer
      .of(request.canvas)
      .host(request, buildHost)
      .fold(FrameSurfaceFactory.Java2D.create(request))(record(request, _))

  private def record(request: FrameSurfaceRequest, host: LayerHost): RenderSurface =
    val canvas        = request.canvas
    val logicalWidth  = Java2DRenderSurface.logicalCanvasDimension(canvas.getWidth, canvas.getPreferredSize.width)
    val logicalHeight = Java2DRenderSurface.logicalCanvasDimension(canvas.getHeight, canvas.getPreferredSize.height)
    val scale         = Java2DRenderSurface.deviceScaleFor(canvas)
    val recorder      = new PictureRecorder()
    val recording = PictureRecording.begin(
      recorder,
      Java2DRenderSurface.deviceImageDimension(logicalWidth, scale.x),
      Java2DRenderSurface.deviceImageDimension(logicalHeight, scale.y)
    )
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
      layerCacheOwner = Some(ScreenIdentity(canvas))
    )

/** The factory a window uses: a Skiko layer made on its canvas. */
object SkiaLayerSurfaceFactory extends LayerSurfaceFactory(canvas => SkiaLayerHost.create(canvas))

/** For a timing run: each frame is counted as drawn only once the GPU has finished it. */
object SynchronousSkiaLayerSurfaceFactory
    extends LayerSurfaceFactory(canvas => SkiaLayerHost.create(canvas, syncGpu = true))

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

    def host(request: FrameSurfaceRequest, build: JPanel => LayerHost): Option[LayerHost] =
      state.get() match
        case State.Ready(host) => Some(host)
        case State.Failed      => None
        case State.Unbuilt =>
          Try(build(request.canvas)) match
            case Success(host) =>
              state.set(State.Ready(host))
              request.attachLayer(host)
              Some(host)
            case Failure(error) =>
              state.set(State.Failed)
              log.warn("The Skia layer could not be made; this window paints with Java2D", error)
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
