package com.serenity.ui.skia

import java.awt.event.{ComponentAdapter, ComponentEvent, MouseAdapter, MouseEvent}
import java.awt.{Cursor, Rectangle}
import java.util.concurrent.FutureTask
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}
import javax.swing.{JPanel, SwingUtilities}

import com.serenity.ui.terminal.{LayerDrawStats, SwingWindow}
import org.jetbrains.skia.{Canvas, Paint, Picture, PixelGeometry, Rect}
import org.jetbrains.skiko.{RenderFactory, SkiaLayer, SkiaLayerAnalytics, SkiaLayerProperties, SkikoRenderDelegate}

/** A Skiko `SkiaLayer` filling a window canvas. The layer draws on the event thread when asked to, so the thread that
  * renders frames records each as a `Picture` and publishes it; the layer replays the latest one and fills the carets
  * over it, which is why a caret blink never records the frame again.
  *
  * The canvas keeps the keyboard focus (and with it the input method), so the layer is not focusable. A heavyweight
  * layer takes the mouse events for itself, so they are passed on to the canvas, where the input handler listens.
  */
final class SkiaLayerHost private (canvas: JPanel, layer: SkiaLayer, syncGpu: Boolean) extends LayerHost:

  private val slot      = new PictureSlot[Picture](_.close())
  private val carets    = new AtomicReference(List.empty[SwingWindow.CaretPaint])
  private val published = new AtomicBoolean(false)
  private val renders   = new AtomicInteger(0)
  private val publishes = new AtomicInteger(0)
  private val listener  = new AtomicReference[Option[() => Unit]](None)
  private val draws     = new AtomicReference(SkiaLayerHost.Draws.Empty)
  private val awaiting  = new AtomicLong(0L)

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
        override def onRender(surface: Canvas, width: Int, height: Int, nanoTime: Long): Unit = render(surface)
    )
    canvas.add(layer, 0)
    canvas.addComponentListener(followCanvas)
    fitLayer()
    canvas.revalidate()
    canvas.repaint()

  private def render(surface: Canvas): Unit =
    val _       = renders.incrementAndGet()
    val started = System.nanoTime()
    val pending = awaiting.get()
    surface.clear(0xff000000)
    val drawn = slot.drawLatest(picture =>
      val _ = picture.playback(surface, () => false)
    )
    val replayed = System.nanoTime()
    val scale    = layer.getContentScale
    val ink      = new Paint()
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
    if syncGpu then
      Option(surface.getRecordingContext).foreach { context =>
        val _ = context.flush()
        context.submit(true)
      }
    val synced = System.nanoTime()
    val _ = draws.updateAndGet(
      _.drawn(
        waitedNanos = Option.when(drawn && pending != 0L && started >= pending && awaiting.compareAndSet(pending, 0L))(
          started - pending
        ),
        replayNanos = replayed - started,
        caretNanos = inked - replayed,
        syncNanos = synced - inked
      )
    )
    listener.get().foreach(_.apply())

  def baseAvailable: Boolean = published.get()

  def onRendered(callback: () => Unit): Unit = listener.set(Some(callback))

  def renderApi: String = layer.getRenderApi.toString

  override def drawStats: LayerDrawStats = draws.get().stats(publishes.get())

  def publish(picture: Picture): Unit =
    val _ = awaiting.compareAndSet(0L, System.nanoTime())
    slot.publish(picture)
    published.set(true)
    val _ = publishes.incrementAndGet()
    layer.repaint()

  def showCarets(shown: List[SwingWindow.CaretPaint]): Unit =
    carets.set(shown)
    layer.repaint()

  def setCursor(cursor: Cursor): Unit = SkiaLayerHost.onEventThread(layer.setCursor(cursor))

  def dispose(): Unit =
    SkiaLayerHost.onEventThread {
      canvas.removeComponentListener(followCanvas)
      canvas.remove(layer)
      layer.dispose()
      canvas.repaint()
    }
    slot.closeAll()

  private[skia] def renderCount: Int       = renders.get()
  private[skia] def publishCount: Int      = publishes.get()
  private[skia] def closedPictures: Int    = slot.closedCount
  private[skia] def layerBounds: Rectangle = layer.getBounds()
  private[skia] def layerCursorType: Int   = layer.getCursor.getType

object SkiaLayerHost:

  /** The totals of a layer's draws, in nanoseconds. */
  final private case class Draws(
      renders: Int,
      waits: Int,
      waitNanos: Long,
      longestWaitNanos: Long,
      replayNanos: Long,
      caretNanos: Long,
      syncNanos: Long
  ):

    def drawn(waitedNanos: Option[Long], replayNanos: Long, caretNanos: Long, syncNanos: Long): Draws =
      copy(
        renders = renders + 1,
        waits = waits + waitedNanos.size,
        waitNanos = waitNanos + waitedNanos.getOrElse(0L),
        longestWaitNanos = longestWaitNanos.max(waitedNanos.getOrElse(0L)),
        replayNanos = this.replayNanos + replayNanos,
        caretNanos = this.caretNanos + caretNanos,
        syncNanos = this.syncNanos + syncNanos
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
        syncMeanMs = mean(syncNanos, renders)
      )

  private object Draws:
    val Empty: Draws = Draws(0, 0, 0L, 0L, 0L, 0L, 0L)

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
    onEventThread {
      val layer = newLayer()
      val host  = new SkiaLayerHost(canvas, layer, syncGpu)
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
