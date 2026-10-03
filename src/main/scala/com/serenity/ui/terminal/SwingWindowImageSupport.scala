package com.serenity.ui.terminal

import java.awt.*
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicReference

/** Frame-buffer pooling and cursor-overlay repaint math for [[SwingWindow]]. Mixed into that class's companion object
  * so callers keep seeing `SwingWindow.ReusableImagePool` etc.; split into its own file to keep `SwingWindow.scala`
  * within the architecture ratchet's line target.
  */
private[terminal] trait SwingWindowImageSupport:

  final private[serenity] case class CaretPaint(rect: Rectangle, color: Color)

  /** A two-image frame pool: the render thread draws into the spare while the EDT presents the published image.
    *
    * The EDT leases the published image for the length of a paint. A paint that started before the next publish can
    * still be reading that image once it has become the spare, so the spare is never handed back while leased.
    */
  final private[serenity] class ReusableImagePool:

    final private case class Slots(
        published: Option[BufferedImage],
        spare: Option[BufferedImage],
        leased: Option[BufferedImage]
    ):
      def spareIsLeased: Boolean = spare.exists(image => leased.exists(_ eq image))

    private val slots = new AtomicReference(Slots(None, None, None))

    def acquire(width: Int, height: Int, imageType: Int): BufferedImage =
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

  def shouldRepaintBaseFrameBeforeCursorOverlay(cursorVisible: Boolean): Boolean =
    !cursorVisible

  /** The bound `onCursorOverlayReady` should pass to `canvas.repaint(...)`.
    *
    * `None` (an unbounded base frame) always wins, since a structural change may have moved pixels the cursor rects
    * alone wouldn't cover. Otherwise the result covers the base region plus every cursor rect from both the previous
    * and current frame -- a rect that isn't part of the union is either off-screen or zero-sized, since a cursor that
    * stopped being drawn still needs its last position repainted. Zero-sized rects (including the `(0, 0, 0, 0)`
    * sentinel callers use for "nothing" and "no cursor") are dropped before unioning: `Rectangle` still treats a
    * zero-sized rect as covering its `(x, y)` corner, which would otherwise drag every union back to the origin.
    */
  private[serenity] def combinedCursorRepaintRegion(
    baseDirtyRegion: Option[Rectangle],
    previousCursorRects: scala.List[Rectangle],
    currentCursorRects: scala.List[Rectangle]
  ): Option[Rectangle] =
    baseDirtyRegion.map { base =>
      (base :: previousCursorRects ::: currentCursorRects)
        .filter(rect => rect.width > 0 && rect.height > 0)
        .reduceOption(_.union(_))
        .getOrElse(new Rectangle(0, 0, 0, 0))
    }

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
