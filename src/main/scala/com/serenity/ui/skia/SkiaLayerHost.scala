package com.serenity.ui.skia

import java.awt.event.{ComponentAdapter, ComponentEvent, MouseAdapter, MouseEvent}
import java.awt.{Cursor, Rectangle}
import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}
import java.util.concurrent.FutureTask
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}
import javax.swing.{JPanel, SwingUtilities}

import scala.util.Try

import com.serenity.ui.terminal.{LayerDrawStats, SwingWindow}
import org.jetbrains.skia.{Canvas, Paint, Picture, PixelGeometry, Rect}
import org.jetbrains.skiko.{RenderFactory, SkiaLayer, SkiaLayerAnalytics, SkiaLayerProperties, SkikoRenderDelegate}

/** A Skiko `SkiaLayer` filling a window canvas. The layer draws on the event thread when asked to, so the thread that
  * renders frames records each as a `Picture` and publishes it. Skiko hands `onRender` a canvas that records into its
  * own picture, which it draws on the GPU later, so each draw adds the published picture to it as a single op and fills
  * the carets over it. That canvas has no GPU context, so there is nothing to keep an offscreen image in, or to wait on
  * for the GPU to finish a frame; the draw stats say so.
  *
  * The canvas keeps the keyboard focus (and with it the input method), so the layer is not focusable. A heavyweight
  * layer takes the mouse events for itself, so they are passed on to the canvas, where the input handler listens.
  */
final class SkiaLayerHost private (canvas: JPanel, layer: SkiaLayer, syncGpu: Boolean, redraw: LayerRedraw)
    extends LayerHost:

  private val slot         = new PictureSlot[Picture](_.close())
  private val carets       = new AtomicReference(List.empty[SwingWindow.CaretPaint])
  private val published    = new AtomicBoolean(false)
  private val renders      = new AtomicInteger(0)
  private val publishes    = new AtomicInteger(0)
  private val listener     = new AtomicReference[Option[() => Unit]](None)
  private val draws        = new AtomicReference(SkiaLayerHost.Draws.Empty)
  private val shown        = new AtomicInteger(0)
  private val pictureOps   = new AtomicLong(0L)
  private val pictureBytes = new AtomicLong(0L)
  private val awaiting     = new AtomicLong(0L)
  private val caretSetAt   = new AtomicLong(0L)

  private val followCanvas = new ComponentAdapter:
    override def componentResized(e: ComponentEvent): Unit = fitLayer()

  private val passMouseToCanvas = new MouseAdapter:
    private def pass(e: MouseEvent): Unit = canvas.dispatchEvent(SwingUtilities.convertMouseEvent(layer, e, canvas))
    override def mousePressed(e: MouseEvent): Unit  = pass(e)
    override def mouseReleased(e: MouseEvent): Unit = pass(e)
    override def mouseClicked(e: MouseEvent): Unit  = pass(e)
    override def mouseEntered(e: MouseEvent): Unit  = pass(e)
    override def mouseExited(e: MouseEvent): Unit   = pass(e)
    override def mouseMoved(e: MouseEvent): Unit    = pass(e)
    override def mouseDragged(e: MouseEvent): Unit  = pass(e)

  private def fitLayer(): Unit = layer.setBounds(0, 0, canvas.getWidth, canvas.getHeight)

  private def install(): Unit =
    layer.setFocusable(false)
    layer.addMouseListener(passMouseToCanvas)
    layer.addMouseMotionListener(passMouseToCanvas)
    layer.setRenderDelegate(
      new SkikoRenderDelegate:
        override def onRender(surface: Canvas, width: Int, height: Int, nanoTime: Long): Unit =
          render(surface)
    )
    canvas.add(layer, 0)
    canvas.addComponentListener(followCanvas)
    fitLayer()
    canvas.revalidate()
    canvas.repaint()

  private def render(surface: Canvas): Unit =
    val _           = renders.incrementAndGet()
    val started     = System.nanoTime()
    val pending     = awaiting.get()
    val caretWaited = Some(caretSetAt.getAndSet(0L)).filter(_ != 0L).map(started - _)
    surface.clear(0xff000000)
    val version    = publishes.get()
    val hasContext = Option(surface.getRecordingContext).isDefined
    val drawn      = slot.drawLatest(picture => SkiaLayerHost.addPicture(surface, picture))
    if drawn then shown.set(version)
    val replayedAt = System.nanoTime()
    val scale      = layer.getContentScale
    val ink        = new Paint()
    carets.get().foreach { caret =>
      ink.setColor(caret.color.getRGB)
      surface.drawRect(
        Rect.Companion.makeXYWH(
          caret.rect.x * scale,
          caret.rect.y * scale,
          caret.rect.width * scale,
          caret.rect.height * scale
        ),
        ink
      )
    }
    ink.close()
    val inked = System.nanoTime()
    val synced = syncGpu && Option(surface.getRecordingContext).fold(false) { context =>
      val _ = context.flush()
      context.submit(true)
      true
    }
    val syncedAt = System.nanoTime()
    val _ = draws.updateAndGet(
      _.drawn(
        waitedNanos = Option.when(drawn && pending != 0L && started >= pending && awaiting.compareAndSet(pending, 0L))(
          started - pending
        ),
        caretWaitedNanos = caretWaited,
        replayNanos = replayedAt - started,
        caretNanos = inked - replayedAt,
        syncNanos = syncedAt - inked,
        synced = synced,
        gpuContext = hasContext
      )
    )
    listener.get().foreach(_.apply())

  def baseAvailable: Boolean = published.get()

  override def publishedFrames: Int = publishes.get()

  override def shownFrames: Int = shown.get()

  def onRendered(callback: () => Unit): Unit = listener.set(Some(callback))

  def renderApi: String = layer.getRenderApi.toString

  override def drawStats: LayerDrawStats =
    val count                     = publishes.get()
    def mean(total: Long): Double = if count == 0 then 0.0 else total.toDouble / count
    draws
      .get()
      .stats(count)
      .copy(pictureOpsMean = mean(pictureOps.get()), pictureBytesMean = mean(pictureBytes.get()))

  def publish(picture: Picture): Unit =
    val _ = awaiting.compareAndSet(0L, System.nanoTime())
    val _ = pictureOps.addAndGet(picture.getApproximateOpCount.toLong)
    val _ = pictureBytes.addAndGet(picture.getApproximateBytesUsed.toLong)
    slot.publish(picture)
    published.set(true)
    val _ = publishes.incrementAndGet()
    redraw.afterPublish()

  def showCarets(shown: List[SwingWindow.CaretPaint]): Unit =
    carets.set(shown)
    val _ = caretSetAt.compareAndSet(0L, System.nanoTime())
    redraw.afterCaretsChanged()

  def setCursor(cursor: Cursor): Unit = SkiaLayerHost.onEventThread(layer.setCursor(cursor))

  def dispose(): Unit =
    SkiaLayerHost.onEventThread {
      canvas.removeComponentListener(followCanvas)
      canvas.remove(layer)
      layer.dispose()
      canvas.repaint()
    }
    slot.closeAll()

  private[skia] def renderInto(surface: Canvas): Unit = render(surface)
  private[skia] def renderCount: Int                  = renders.get()
  private[skia] def publishCount: Int                 = publishes.get()
  private[skia] def closedPictures: Int               = slot.closedCount
  private[skia] def layerBounds: Rectangle            = layer.getBounds()
  private[skia] def layerCursorType: Int              = layer.getCursor.getType

object SkiaLayerHost:

  /** The totals of a layer's draws, in nanoseconds. */
  final private case class Draws(
      renders: Int,
      waits: Int,
      waitNanos: Long,
      longestWaitNanos: Long,
      replayNanos: Long,
      caretNanos: Long,
      syncNanos: Long,
      syncedDraws: Int,
      gpuContext: Boolean,
      caretWaits: Int,
      caretWaitNanos: Long,
      longestCaretWaitNanos: Long
  ):

    def drawn(
      waitedNanos: Option[Long],
      caretWaitedNanos: Option[Long],
      replayNanos: Long,
      caretNanos: Long,
      syncNanos: Long,
      synced: Boolean,
      gpuContext: Boolean
    ): Draws =
      copy(
        renders = renders + 1,
        waits = waits + waitedNanos.size,
        waitNanos = waitNanos + waitedNanos.getOrElse(0L),
        longestWaitNanos = longestWaitNanos.max(waitedNanos.getOrElse(0L)),
        replayNanos = this.replayNanos + replayNanos,
        caretNanos = this.caretNanos + caretNanos,
        syncNanos = this.syncNanos + syncNanos,
        syncedDraws = syncedDraws + (if synced then 1 else 0),
        gpuContext = gpuContext,
        caretWaits = caretWaits + caretWaitedNanos.size,
        caretWaitNanos = caretWaitNanos + caretWaitedNanos.getOrElse(0L),
        longestCaretWaitNanos = longestCaretWaitNanos.max(caretWaitedNanos.getOrElse(0L))
      )

    def stats(publishes: Int): LayerDrawStats =
      def mean(nanos: Long, count: Int): Double = if count == 0 then 0.0 else nanos / 1e6 / count
      LayerDrawStats(
        renders = renders,
        publishes = publishes,
        publishToRenderMeanMs = mean(waitNanos, waits),
        publishToRenderMaxMs = longestWaitNanos / 1e6,
        replayMeanMs = mean(replayNanos, renders),
        caretsMeanMs = mean(caretNanos, renders),
        syncMeanMs = mean(syncNanos, renders),
        syncedDraws = syncedDraws,
        gpuContext = gpuContext,
        caretWaits = caretWaits,
        caretToRenderMeanMs = mean(caretWaitNanos, caretWaits),
        caretToRenderMaxMs = longestCaretWaitNanos / 1e6
      )

  private object Draws:
    val Empty: Draws = Draws(0, 0, 0L, 0L, 0L, 0L, 0L, 0, false, 0, 0L, 0L)

  /** Skiko's `Canvas.drawPicture` has a hyphen in its JVM name, because its matrix parameter is an inline class, and
    * Scala cannot call a name like that, so it is looked up by name. If a Skiko release renames it, pictures are played
    * back op by op instead, which draws the same pixels.
    */
  private val drawPicture: Option[MethodHandle] =
    Try(
      MethodHandles
        .publicLookup()
        .findVirtual(
          classOf[Canvas],
          "drawPicture-gqNt1-A",
          MethodType.methodType(classOf[Canvas], classOf[Picture], classOf[Array[Float]], classOf[Paint])
        )
    ).toOption

  /** Adds `picture` to `canvas` as one op, which Skia keeps a reference to, so closing the picture afterwards is safe.
    * No matrix and no paint is a null each.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def addPicture(canvas: Canvas, picture: Picture): Unit =
    drawPicture.fold {
      val _ = picture.playback(canvas, () => false)
    } { handle =>
      val _ = handle.invoke(canvas, picture, null, null)
    }

  /** How many Skia layers `canvas` holds; more than one means they are stacked, and every publish redraws all of them.
    */
  def layersOn(canvas: JPanel): Int = canvas.getComponents.count {
    case _: SkiaLayer => true
    case _            => false
  }

  /** The layer is made, added to `canvas` and sized to it on the event thread. With `syncGpu` each frame waits for the
    * GPU to finish before it counts as drawn, which only a timing run wants.
    */
  def create(canvas: JPanel, syncGpu: Boolean = false): SkiaLayerHost =
    createWith(canvas, syncGpu, LayerRedraw.forLayer)

  private[skia] def createWith(canvas: JPanel, syncGpu: Boolean, redraw: SkiaLayer => LayerRedraw): SkiaLayerHost =
    onEventThread {
      val layer = newLayer()
      val host  = new SkiaLayerHost(canvas, layer, syncGpu, redraw(layer))
      host.install()
      host
    }

  // Skiko's accessibility hook is a Kotlin function parameter whose default is "none".
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def newLayer(): SkiaLayer =
    new SkiaLayer(
      null,
      new SkiaLayerProperties(),
      RenderFactory.Companion.getDefault(),
      SkiaLayerAnalytics.Companion.getEmpty(),
      PixelGeometry.UNKNOWN,
      false
    )

  /** Runs `body` on the event thread and waits; a failure in it comes back as the `ExecutionException` that wraps it.
    */
  private[skia] def onEventThread[A](body: => A): A =
    if SwingUtilities.isEventDispatchThread then body
    else
      val task = new FutureTask[A](() => body)
      SwingUtilities.invokeAndWait(task)
      task.get()
