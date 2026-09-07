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

  final private[serenity] case class PublishedImages(
      base: Option[BufferedImage],
      overlay: Option[BufferedImage]
  )

  private[serenity] object PublishedImages:
    val empty: PublishedImages = PublishedImages(None, None)

  final private[serenity] class ReusableImagePool:
    private val published = new AtomicReference[Option[BufferedImage]](None)
    private val spare     = new AtomicReference[Option[BufferedImage]](None)

    def acquire(width: Int, height: Int, imageType: Int): BufferedImage =
      spare
        .getAndSet(None)
        .filter(image => image.getWidth == width && image.getHeight == height && image.getType == imageType)
        .getOrElse(new BufferedImage(width, height, imageType))

    def publish(image: BufferedImage): Unit =
      val previous = published.getAndSet(Some(image))
      previous.filterNot(_ eq image).foreach(previousImage => spare.set(Some(previousImage)))

    def clearPublished(): Unit =
      published.getAndSet(None).foreach(image => spare.set(Some(image)))

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

  final private[serenity] class RoundedCornerMaskBuffers private (
      val width: Int,
      val height: Int,
      val cornerArc: Int,
      private val contents: BufferedImage,
      private val maskImage: BufferedImage,
      private val masked: BufferedImage
  ):

    def matches(otherWidth: Int, otherHeight: Int, otherCornerArc: Int): Boolean =
      width == otherWidth && height == otherHeight && cornerArc == otherCornerArc.max(0)

    def render(paintContents: Graphics => Unit): BufferedImage =
      val contentsGraphics = contents.createGraphics()
      try
        contentsGraphics.setComposite(AlphaComposite.Clear)
        contentsGraphics.fillRect(0, 0, width, height)
        contentsGraphics.setComposite(AlphaComposite.SrcOver)
        paintContents(contentsGraphics)
      finally contentsGraphics.dispose()
      mask(contents)

    def mask(source: BufferedImage): BufferedImage =
      val maskedGraphics = masked.createGraphics()
      try
        maskedGraphics.setComposite(AlphaComposite.Src)
        maskedGraphics.drawImage(source, 0, 0, null)
        maskedGraphics.setComposite(AlphaComposite.DstIn)
        maskedGraphics.drawImage(maskImage, 0, 0, null)
        masked
      finally maskedGraphics.dispose()

  private[serenity] object RoundedCornerMaskBuffers:

    def create(width: Int, height: Int, cornerArc: Int): RoundedCornerMaskBuffers =
      val normalizedWidth    = width.max(1)
      val normalizedHeight   = height.max(1)
      val normalizedArc      = cornerArc.max(0)
      val supersampledWidth  = normalizedWidth * RoundedCornerMaskScale
      val supersampledHeight = normalizedHeight * RoundedCornerMaskScale
      val supersampledArc    = normalizedArc * RoundedCornerMaskScale
      val supersampledMask = new BufferedImage(
        supersampledWidth,
        supersampledHeight,
        BufferedImage.TYPE_INT_ARGB
      )
      val maskGraphics = supersampledMask.createGraphics()
      try
        maskGraphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        maskGraphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
        maskGraphics.setColor(Color.WHITE)
        maskGraphics.fill(
          new RoundRectangle2D.Double(0, 0, supersampledWidth, supersampledHeight, supersampledArc, supersampledArc)
        )
      finally maskGraphics.dispose()

      val mask               = new BufferedImage(normalizedWidth, normalizedHeight, BufferedImage.TYPE_INT_ARGB)
      val downsampleGraphics = mask.createGraphics()
      try
        downsampleGraphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
        downsampleGraphics.setRenderingHint(
          RenderingHints.KEY_INTERPOLATION,
          RenderingHints.VALUE_INTERPOLATION_BICUBIC
        )
        downsampleGraphics.drawImage(supersampledMask, 0, 0, normalizedWidth, normalizedHeight, null)
      finally downsampleGraphics.dispose()

      new RoundedCornerMaskBuffers(
        normalizedWidth,
        normalizedHeight,
        normalizedArc,
        new BufferedImage(normalizedWidth, normalizedHeight, BufferedImage.TYPE_INT_ARGB),
        mask,
        new BufferedImage(normalizedWidth, normalizedHeight, BufferedImage.TYPE_INT_ARGB)
      )

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

  private[serenity] def clearImage(image: BufferedImage): Unit =
    val graphics = image.createGraphics()
    try
      graphics.setComposite(AlphaComposite.Clear)
      graphics.fillRect(0, 0, image.getWidth, image.getHeight)
    finally graphics.dispose()
