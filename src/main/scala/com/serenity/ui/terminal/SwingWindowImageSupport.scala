package com.serenity.ui.terminal

import java.awt.*
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

import com.serenity.ui.layout.PixelRect

/** Frame-buffer pooling and cursor-overlay repaint math for [[SwingWindow]]. Mixed into that class's companion object
  * so callers keep seeing `SwingWindow.ReusableImagePool` etc.; split into its own file to keep `SwingWindow.scala`
  * within the architecture ratchet's line target.
  */
private[terminal] trait SwingWindowImageSupport:

  final private[serenity] case class CaretPaint(rect: Rectangle, color: Color)

  /** A two-image frame pool: the render thread draws into the spare while the EDT presents the published image.
    *
    * The EDT leases the published image for the length of a paint. A paint that started before the next publish can
    * still be reading that image once it has become the spare, so the spare is never handed back while leased. Rather
    * than allocate a third full-window image (about 50MB at 2x) for that rare overlap, `acquire` waits a short while
    * for the paint to finish; only a paint stuck past [[MaxLeaseWaitNanos]] makes it allocate, so a hung event thread
    * can never stall rendering.
    */
  final private[serenity] class ReusableImagePool:

    final private case class Slots(
        published: Option[BufferedImage],
        spare: Option[BufferedImage],
        leased: Option[BufferedImage]
    ):
      def spareIsLeased: Boolean = spare.exists(image => leased.exists(_ eq image))

    private val slots     = new AtomicReference(Slots(None, None, None))
    private val leaseLock = new Object

    @annotation.tailrec
    private def awaitSpareRelease(deadlineNanos: Long): Unit =
      val remainingNanos = deadlineNanos - System.nanoTime()
      if slots.get().spareIsLeased && remainingNanos > 0 then
        val interrupted =
          leaseLock.synchronized {
            if slots.get().spareIsLeased then
              try
                leaseLock.wait(remainingNanos / 1_000_000L, (remainingNanos % 1_000_000L).toInt)
                false
              catch
                case _: InterruptedException =>
                  Thread.currentThread().interrupt()
                  true
            else false
          }
        if !interrupted then awaitSpareRelease(deadlineNanos)

    def acquire(width: Int, height: Int, imageType: Int): BufferedImage =
      awaitSpareRelease(System.nanoTime() + MaxLeaseWaitNanos)
      val before = slots.getAndUpdate(current => if current.spareIsLeased then current else current.copy(spare = None))
      before.spare
        .filterNot(_ => before.spareIsLeased)
        .filter(image => image.getWidth == width && image.getHeight == height && image.getType == imageType)
        .getOrElse(new BufferedImage(width, height, imageType))

    def publish(image: BufferedImage): Unit =
      val _ = slots.getAndUpdate { current =>
        current.published match
          case Some(previous) if !(previous eq image) => current.copy(published = Some(image), spare = Some(previous))
          case _                                      => current.copy(published = Some(image))
      }

    def leasePublished(): Option[BufferedImage] =
      slots.updateAndGet(current => current.copy(leased = current.published)).leased

    def releaseLease(): Unit =
      val _ = slots.updateAndGet(_.copy(leased = None))
      leaseLock.synchronized(leaseLock.notifyAll())

  /** A paint takes a few milliseconds; this is generous for one and short enough that a stuck one costs a single frame.
    */
  private val MaxLeaseWaitNanos = 50_000_000L

  def shouldRepaintBaseFrameBeforeCursorOverlay(cursorVisible: Boolean): Boolean =
    !cursorVisible

  /** What the canvas still has to repaint for the frames published since it last painted. */
  private[serenity] enum CanvasRepaint:
    case Whole
    case Rects(rects: scala.List[Rectangle])

  /** The repaint for a base frame plus the carets drawn over it.
    *
    * An unbounded base frame (`None`) is always [[CanvasRepaint.Whole]], since a structural change may have moved
    * pixels the caret rects alone wouldn't cover. Otherwise it is the base frame's own rects plus every caret rect from
    * both the previous and current frame -- a caret that stopped being drawn still needs its last position repainted --
    * each kept apart unless it touches another, so a caret far from the rest of the change is not joined to it by one
    * rect spanning the space between. Zero-sized rects (the `(0, 0, 0, 0)` "no cursor" sentinel among them) are
    * dropped.
    */
  private[serenity] def cursorRepaint(
    baseDirtyRects: Option[scala.List[Rectangle]],
    previousCursorRects: scala.List[Rectangle],
    currentCursorRects: scala.List[Rectangle]
  ): CanvasRepaint =
    baseDirtyRects.fold(CanvasRepaint.Whole) { base =>
      CanvasRepaint.Rects(coalescedRectangles(base ::: previousCursorRects ::: currentCursorRects))
    }

  /** The bounds of [[cursorRepaint]]'s rects: `None` for the whole canvas, an empty rect when nothing changed. */
  private[serenity] def combinedCursorRepaintRegion(
    baseDirtyRegion: Option[Rectangle],
    previousCursorRects: scala.List[Rectangle],
    currentCursorRects: scala.List[Rectangle]
  ): Option[Rectangle] =
    cursorRepaint(baseDirtyRegion.map(scala.List(_)), previousCursorRects, currentCursorRects) match
      case CanvasRepaint.Whole        => None
      case CanvasRepaint.Rects(rects) => Some(rects.reduceOption(_.union(_)).getOrElse(new Rectangle(0, 0, 0, 0)))

  /** A repaint still pending when another frame is published covers both frames' changes. */
  private[serenity] def mergedRepaint(pending: CanvasRepaint, next: CanvasRepaint): CanvasRepaint =
    (pending, next) match
      case (CanvasRepaint.Rects(earlier), CanvasRepaint.Rects(later)) =>
        CanvasRepaint.Rects(coalescedRectangles(earlier ::: later))
      case _ => CanvasRepaint.Whole

  private def coalescedRectangles(rects: scala.List[Rectangle]): scala.List[Rectangle] =
    PixelRect
      .coalesced(rects.map(rect => PixelRect(rect.x, rect.y, rect.width, rect.height)), PixelRect.RepaintRectLimit)
      .map(rect => new Rectangle(rect.xPx, rect.yPx, rect.widthPx, rect.heightPx))

  private[serenity] def publishRenderedBaseFrame(
    image: BufferedImage,
    replaceRenderedImage: Boolean,
    setRenderedImage: Option[BufferedImage] => Unit,
    repaint: () => Unit
  ): Unit =
    if replaceRenderedImage then
      setRenderedImage(Some(image))
      repaint()

  def copyImage(source: BufferedImage): BufferedImage =
    val copy = new BufferedImage(source.getWidth, source.getHeight, source.getType)
    val g    = copy.createGraphics()
    try g.drawImage(source, 0, 0, null)
    finally g.dispose()
    copy

  /** The image pixels `clip` covers when `image` is stretched over a `panelWidth` x `panelHeight` panel, rounded
    * outwards and clamped to the image -- a HiDPI image is larger than the logical panel it fills.
    */
  private[serenity] def imageSourceRegion(
    clip: Rectangle,
    panelWidth: Int,
    panelHeight: Int,
    imageWidth: Int,
    imageHeight: Int
  ): Option[Rectangle] =
    Option
      .when(panelWidth > 0 && panelHeight > 0 && imageWidth > 0 && imageHeight > 0) {
        val scaleX = imageWidth.toDouble / panelWidth
        val scaleY = imageHeight.toDouble / panelHeight
        val x0     = math.floor(clip.x * scaleX).toInt.max(0).min(imageWidth)
        val y0     = math.floor(clip.y * scaleY).toInt.max(0).min(imageHeight)
        val x1     = math.ceil((clip.x.toDouble + clip.width) * scaleX).toInt.max(0).min(imageWidth)
        val y1     = math.ceil((clip.y.toDouble + clip.height) * scaleY).toInt.max(0).min(imageHeight)
        Option.when(x1 > x0 && y1 > y0)(new Rectangle(x0, y0, x1 - x0, y1 - y0))
      }
      .flatten

  /** Present a frame into `g`, touching only its clip: Java2D bounds a software image upload by the destination
    * rectangle, not the clip, so drawing the whole image for a caret-sized repaint copies the whole window.
    */
  private[serenity] def paintPresentedFrame(
    g: Graphics2D,
    base: Option[BufferedImage],
    carets: scala.List[CaretPaint],
    panelWidth: Int,
    panelHeight: Int
  ): Unit =
    val clip       = Option(g.getClipBounds).getOrElse(new Rectangle(0, 0, panelWidth, panelHeight))
    val background = g.create().asInstanceOf[Graphics2D]
    try SwingWindow.paintCanvasBackground(background, panelWidth, panelHeight)
    finally background.dispose()
    base.foreach(image => drawImageRegion(g, image, clip, panelWidth, panelHeight))
    carets.filter(_.rect.intersects(clip)).foreach { caret =>
      g.setColor(caret.color)
      g.fillRect(caret.rect.x, caret.rect.y, caret.rect.width.max(1), caret.rect.height.max(1))
    }

  /** Scaling `g` to image pixels keeps the image-to-panel mapping exact even when the scale is fractional. */
  private def drawImageRegion(
    g: Graphics2D,
    image: BufferedImage,
    clip: Rectangle,
    panelWidth: Int,
    panelHeight: Int
  ): Unit =
    imageSourceRegion(clip, panelWidth, panelHeight, image.getWidth, image.getHeight).foreach { source =>
      val scaled = g.create().asInstanceOf[Graphics2D]
      try
        scaled.scale(panelWidth.toDouble / image.getWidth, panelHeight.toDouble / image.getHeight)
        val right  = source.x + source.width
        val bottom = source.y + source.height
        val _      = scaled.drawImage(image, source.x, source.y, right, bottom, source.x, source.y, right, bottom, null)
      finally scaled.dispose()
    }
