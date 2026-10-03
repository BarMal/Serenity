package com.serenity.ui.terminal

import java.awt.*
import java.awt.geom.RoundRectangle2D
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

/** Frame-buffer pooling, rounded-corner masking, and cursor-overlay repaint math for [[SwingWindow]]. Mixed into that
  * class's companion object so callers keep seeing `SwingWindow.ReusableImagePool` etc.; split into its own file to
  * keep `SwingWindow.scala` within the architecture ratchet's line target.
  */
private[terminal] trait SwingWindowImageSupport:
  private val RoundedCornerMaskScale = 2

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

  final private[serenity] class RoundedCornerMaskBufferCache:
    private val buffersRef = new AtomicReference[Option[RoundedCornerMaskBuffers]](None)

    @annotation.tailrec
    final def acquire(width: Int, height: Int, cornerArc: Int): RoundedCornerMaskBuffers =
      buffersRef.get() match
        case Some(buffers) if buffers.matches(width, height, cornerArc) => buffers
        case current =>
          val replacement = RoundedCornerMaskBuffers.create(width, height, cornerArc)
          if buffersRef.compareAndSet(current, Some(replacement)) then replacement
          else acquire(width, height, cornerArc)

  final private[serenity] case class CornerTile(bounds: Rectangle, mask: BufferedImage)

  /** One corner tile of the window at a time is re-rendered off-screen and masked; the rest of the window is painted
    * directly, so a repaint away from the corners costs nothing extra.
    */
  final private[serenity] class RoundedCornerMaskBuffers private (
      val width: Int,
      val height: Int,
      val cornerArc: Int,
      val corners: scala.List[CornerTile],
      private val contents: BufferedImage,
      private val masked: BufferedImage
  ):

    def matches(otherWidth: Int, otherHeight: Int, otherCornerArc: Int): Boolean =
      width == otherWidth && height == otherHeight && cornerArc == otherCornerArc.max(0)

    def cornersTouching(clip: Rectangle): scala.List[CornerTile] =
      corners.filter(_.bounds.intersects(clip))

    /** `paintContents` paints in window coordinates; the result is `tile`-sized and only valid until the next call. */
    def render(tile: CornerTile, paintContents: Graphics => Unit): BufferedImage =
      val bounds           = tile.bounds
      val contentsGraphics = contents.createGraphics()
      try
        contentsGraphics.setComposite(AlphaComposite.Clear)
        contentsGraphics.fillRect(0, 0, contents.getWidth, contents.getHeight)
        contentsGraphics.setComposite(AlphaComposite.SrcOver)
        contentsGraphics.translate(-bounds.x, -bounds.y)
        contentsGraphics.clipRect(bounds.x, bounds.y, bounds.width, bounds.height)
        paintContents(contentsGraphics)
      finally contentsGraphics.dispose()
      val maskedGraphics = masked.createGraphics()
      try
        maskedGraphics.setComposite(AlphaComposite.Src)
        maskedGraphics.drawImage(contents, 0, 0, null)
        maskedGraphics.setComposite(AlphaComposite.DstIn)
        maskedGraphics.drawImage(tile.mask, 0, 0, null)
        masked
      finally maskedGraphics.dispose()

  private[serenity] object RoundedCornerMaskBuffers:

    def create(width: Int, height: Int, cornerArc: Int): RoundedCornerMaskBuffers =
      val normalizedWidth  = width.max(1)
      val normalizedHeight = height.max(1)
      val normalizedArc    = cornerArc.max(0)
      val tiles = cornerTileBounds(normalizedWidth, normalizedHeight, normalizedArc).map(bounds =>
        CornerTile(bounds, cornerMask(bounds, normalizedWidth, normalizedHeight, normalizedArc))
      )
      val tileWidth  = tiles.headOption.fold(1)(_.bounds.width)
      val tileHeight = tiles.headOption.fold(1)(_.bounds.height)
      new RoundedCornerMaskBuffers(
        normalizedWidth,
        normalizedHeight,
        normalizedArc,
        tiles,
        new BufferedImage(tileWidth, tileHeight, BufferedImage.TYPE_INT_ARGB),
        new BufferedImage(tileWidth, tileHeight, BufferedImage.TYPE_INT_ARGB)
      )

    /** The `bounds` part of the whole-window rounded rectangle, antialiased by 2x supersampling. The tile's inner edges
      * lie where the mask is uniformly opaque, so downsampling a tile alone gives the same pixels as the whole window.
      */
    private def cornerMask(bounds: Rectangle, width: Int, height: Int, cornerArc: Int): BufferedImage =
      val scale        = RoundedCornerMaskScale
      val supersampled = new BufferedImage(bounds.width * scale, bounds.height * scale, BufferedImage.TYPE_INT_ARGB)
      val maskGraphics = supersampled.createGraphics()
      try
        maskGraphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        maskGraphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
        maskGraphics.setColor(Color.WHITE)
        maskGraphics.translate(-bounds.x * scale, -bounds.y * scale)
        maskGraphics.fill(
          new RoundRectangle2D.Double(
            0,
            0,
            width.toDouble * scale,
            height.toDouble * scale,
            cornerArc.toDouble * scale,
            cornerArc.toDouble * scale
          )
        )
      finally maskGraphics.dispose()

      val mask               = new BufferedImage(bounds.width, bounds.height, BufferedImage.TYPE_INT_ARGB)
      val downsampleGraphics = mask.createGraphics()
      try
        downsampleGraphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
        downsampleGraphics.setRenderingHint(
          RenderingHints.KEY_INTERPOLATION,
          RenderingHints.VALUE_INTERPOLATION_BICUBIC
        )
        downsampleGraphics.drawImage(supersampled, 0, 0, bounds.width, bounds.height, null)
      finally downsampleGraphics.dispose()
      mask

  private[serenity] def cornerTileBounds(width: Int, height: Int, cornerArc: Int): scala.List[Rectangle] =
    if cornerArc <= 0 then Nil
    else
      val tileWidth  = cornerArc.min(width)
      val tileHeight = cornerArc.min(height)
      val right      = width - tileWidth
      val bottom     = height - tileHeight
      scala
        .List((0, 0), (right, 0), (0, bottom), (right, bottom))
        .map((x, y) => new Rectangle(x, y, tileWidth, tileHeight))

  final private[serenity] class CoalescedEdtUpdate(update: () => Unit):
    private val queued = new AtomicBoolean(false)

    def schedule(enqueue: Runnable => Unit): Unit =
      if queued.compareAndSet(false, true) then
        enqueue(
          new Runnable:
            def run(): Unit =
              queued.set(false)
              update()
        )

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
    panelHeight: Int,
    transparent: Boolean
  ): Unit =
    val clip       = Option(g.getClipBounds).getOrElse(new Rectangle(0, 0, panelWidth, panelHeight))
    val background = g.create().asInstanceOf[Graphics2D]
    try SwingWindow.paintCanvasBackground(background, panelWidth, panelHeight, transparent)
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
