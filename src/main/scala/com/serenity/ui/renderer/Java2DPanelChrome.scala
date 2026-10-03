package com.serenity.ui.renderer

import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.awt.{AlphaComposite, Color, Graphics2D, RenderingHints}

import com.serenity.ui.layout.{CellMetrics, PixelRect}

/** Panel drop shadows and bodies for [[Java2DRenderSurface]], painted at device resolution.
  *
  * The shadow is four stacked translucent round rects. Software alpha fills cost an order of magnitude more per pixel
  * than opaque ones, and the stack spans the whole panel, so it is rendered once per geometry into a cached sprite and
  * only the sprite's non-uniform rim is blitted; its uniform interior becomes one fill. Under a body sitting on a known
  * opaque colour, that interior and the body over it collapse further into a single precomputed opaque fill.
  */
private[renderer] object Java2DPanelChrome:

  private val ShadowLayers   = List(6 -> 0.025f, 5 -> 0.035f, 4 -> 0.05f, 3 -> 0.07f)
  private val NearestOffset  = 3
  private val FarthestOffset = 6

  /** A panel's logical-pixel rect and the user-space graphics it is painted through. */
  final case class Placement(g: Graphics2D, target: BufferedImage, rect: PixelRect, arcPx: Int)

  object Placement:

    def apply(
      g: Graphics2D,
      target: BufferedImage,
      metrics: CellMetrics,
      x: Int,
      y: Int,
      width: Int,
      height: Int,
      arcPx: Int
    ): Placement =
      val rect =
        PixelRect(metrics.toPixelX(x), metrics.toPixelY(y), width * metrics.charWidth, height * metrics.lineHeight)
      Placement(g, target, rect, arcPx)

  /** The opaque colour under `body`, when the body over it can be precomputed and painted at device resolution. */
  def precomputableBeneath(body: PanelBodyFill, transform: AffineTransform): Option[Color] =
    body.opaqueBeneath.filter(beneath =>
      beneath.getAlpha == 255 && body.colour.getAlpha == 255 && paintsAtDeviceResolution(transform)
    )

  final case class ShadowSpriteKey(
      widthPx: Int,
      heightPx: Int,
      arcPx: Int,
      argb: Int,
      scaleX: Double,
      scaleY: Double,
      phaseX: Double,
      phaseY: Double,
      antialiasing: AnyRef
  )

  final private case class ShadowSprite(image: BufferedImage, bounds: PixelRect):
    def argbAt(deviceX: Int, deviceY: Int): Int = image.getRGB(deviceX - bounds.xPx, deviceY - bounds.yPx)

  /** Device blits only line up with user-space fills under a pure scale-and-translate transform. */
  def paintsAtDeviceResolution(transform: AffineTransform): Boolean =
    val scaleAndTranslate =
      AffineTransform.TYPE_TRANSLATION | AffineTransform.TYPE_UNIFORM_SCALE | AffineTransform.TYPE_GENERAL_SCALE
    (transform.getType & ~scaleAndTranslate) == 0 && transform.getScaleX > 0 && transform.getScaleY > 0

  def drawShadowLayers(g: Graphics2D, rect: PixelRect, arcPx: Int, color: Color): Unit =
    val savedComposite = g.getComposite
    try
      ShadowLayers.foreach { (offset, alpha) =>
        g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha))
        g.setColor(color)
        g.fillRoundRect(rect.xPx + offset, rect.yPx + offset, rect.widthPx, rect.heightPx, arcPx * 2, arcPx * 2)
      }
    finally g.setComposite(savedComposite)

  def drawShadow(placement: Placement, color: Color, scratch: Java2DScratchBuffers): Unit =
    val sprite   = shadowSprite(placement, color, scratch)
    val interior = shadowInterior(placement)
    withDeviceGraphics(placement) { device =>
      blitOutside(device, sprite, interior)
      interior.foreach { hole =>
        device.setColor(new Color(sprite.argbAt(hole.xPx, hole.yPx), true))
        fill(device, hole)
      }
    }

  /** The shadow (if any) and a fully opaque `body` at `alpha`, over `beneath`, which must be opaque too. */
  def fillBodyOverOpaque(
    placement: Placement,
    shadow: Option[Color],
    body: Color,
    alpha: Float,
    beneath: Color,
    scratch: Java2DScratchBuffers
  ): Unit =
    val panel  = deviceRect(placement.g.getTransform, placement.rect)
    val sprite = shadow.map(shadowSprite(placement, _, scratch))
    val opaque = sprite.fold(Option(panel))(_ => shadowInterior(placement).flatMap(intersection(panel, _)))
    val underBody = (sprite, opaque) match
      case (Some(s), Some(hole)) =>
        val shadowColour = new Color(s.argbAt(hole.xPx, hole.yPx), true)
        over(shadowColour, shadowColour.getAlpha / 255.0, beneath)
      case _ => beneath
    withDeviceGraphics(placement) { device =>
      sprite.foreach(blitOutside(device, _, opaque))
      device.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha))
      device.setColor(body)
      PixelRect.uncoveredWithin(panel, opaque.toList).foreach(fill(device, _))
      device.setComposite(AlphaComposite.SrcOver)
      opaque.foreach { hole =>
        device.setColor(over(body, alpha.toDouble, underBody))
        fill(device, hole)
      }
    }

  private def shadowSprite(placement: Placement, color: Color, scratch: Java2DScratchBuffers): ShadowSprite =
    val transform = placement.g.getTransform
    val rect      = placement.rect
    val originX   = deviceX(transform, rect.xPx + NearestOffset)
    val originY   = deviceY(transform, rect.yPx + NearestOffset)
    val left      = math.floor(originX).toInt
    val top       = math.floor(originY).toInt
    val key = ShadowSpriteKey(
      rect.widthPx,
      rect.heightPx,
      placement.arcPx,
      color.getRGB,
      transform.getScaleX,
      transform.getScaleY,
      originX - left,
      originY - top,
      Option(placement.g.getRenderingHint(RenderingHints.KEY_ANTIALIASING))
        .getOrElse(RenderingHints.VALUE_ANTIALIAS_DEFAULT)
    )
    val image = scratch.shadowSprite(key)(renderShadowSprite(key, color))
    ShadowSprite(image, PixelRect(left, top, image.getWidth, image.getHeight))

  private def renderShadowSprite(key: ShadowSpriteKey, color: Color): BufferedImage =
    val spread = FarthestOffset - NearestOffset
    val width  = math.ceil(key.phaseX + (key.widthPx + spread) * key.scaleX).toInt.max(1)
    val height = math.ceil(key.phaseY + (key.heightPx + spread) * key.scaleY).toInt.max(1)
    val image  = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g      = image.createGraphics()
    try
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, key.antialiasing)
      g.translate(key.phaseX, key.phaseY)
      g.scale(key.scaleX, key.scaleY)
      drawShadowLayers(g, PixelRect(-NearestOffset, -NearestOffset, key.widthPx, key.heightPx), key.arcPx, color)
    finally g.dispose()
    image

  /** Device pixels every shadow layer covers in full, so the stack is one uniform colour there: the overlap of the four
    * offset rects, inset by the corner radius so no rounded corner reaches it.
    */
  private def shadowInterior(placement: Placement): Option[PixelRect] =
    val transform = placement.g.getTransform
    val rect      = placement.rect
    val inset     = placement.arcPx
    val left      = math.ceil(deviceX(transform, rect.xPx + FarthestOffset + inset)).toInt
    val top       = math.ceil(deviceY(transform, rect.yPx + FarthestOffset + inset)).toInt
    val right     = math.floor(deviceX(transform, rect.rightPx + NearestOffset - inset)).toInt
    val bottom    = math.floor(deviceY(transform, rect.bottomPx + NearestOffset - inset)).toInt
    Option.when(left < right && top < bottom)(PixelRect(left, top, right - left, bottom - top))

  private def blitOutside(device: Graphics2D, sprite: ShadowSprite, hole: Option[PixelRect]): Unit =
    PixelRect.uncoveredWithin(sprite.bounds, hole.toList).foreach { piece =>
      val sourceX = piece.xPx - sprite.bounds.xPx
      val sourceY = piece.yPx - sprite.bounds.yPx
      val _ = device.drawImage(
        sprite.image,
        piece.xPx,
        piece.yPx,
        piece.rightPx,
        piece.bottomPx,
        sourceX,
        sourceY,
        sourceX + piece.widthPx,
        sourceY + piece.heightPx,
        Java2DRenderSurface.NoOpImageObserver
      )
    }

  private def withDeviceGraphics(placement: Placement)(paint: Graphics2D => Unit): Unit =
    val activeClip = Option(placement.g.getClip).map(placement.g.getTransform.createTransformedShape)
    val device     = placement.target.createGraphics()
    try
      activeClip.foreach(device.clip)
      paint(device)
    finally device.dispose()

  /** The device pixels a user-space fill of `rect` covers: those whose centres fall inside it. */
  private def deviceRect(transform: AffineTransform, rect: PixelRect): PixelRect =
    val left   = math.ceil(deviceX(transform, rect.xPx) - 0.5).toInt
    val top    = math.ceil(deviceY(transform, rect.yPx) - 0.5).toInt
    val right  = math.ceil(deviceX(transform, rect.rightPx) - 0.5).toInt
    val bottom = math.ceil(deviceY(transform, rect.bottomPx) - 0.5).toInt
    PixelRect(left, top, (right - left).max(0), (bottom - top).max(0))

  private def intersection(a: PixelRect, b: PixelRect): Option[PixelRect] =
    val left   = a.xPx.max(b.xPx)
    val top    = a.yPx.max(b.yPx)
    val right  = a.rightPx.min(b.rightPx)
    val bottom = a.bottomPx.min(b.bottomPx)
    Option.when(left < right && top < bottom)(PixelRect(left, top, right - left, bottom - top))

  private def deviceX(transform: AffineTransform, logicalX: Int): Double =
    logicalX * transform.getScaleX + transform.getTranslateX

  private def deviceY(transform: AffineTransform, logicalY: Int): Double =
    logicalY * transform.getScaleY + transform.getTranslateY

  private def fill(device: Graphics2D, rect: PixelRect): Unit =
    device.fillRect(rect.xPx, rect.yPx, rect.widthPx, rect.heightPx)

  /** `top`'s colour at `alpha` composited over the opaque `under`. */
  private def over(top: Color, alpha: Double, under: Color): Color =
    def channel(topChannel: Int, underChannel: Int): Int =
      math.round(topChannel * alpha + underChannel * (1.0 - alpha)).toInt.max(0).min(255)
    new Color(
      channel(top.getRed, under.getRed),
      channel(top.getGreen, under.getGreen),
      channel(top.getBlue, under.getBlue)
    )
