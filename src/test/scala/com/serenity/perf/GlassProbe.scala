package com.serenity.perf

import java.awt.{Dimension, RenderingHints}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import javax.swing.{JFrame, SwingUtilities, WindowConstants}
import java.awt.geom.RoundRectangle2D
import java.awt.image.{BufferedImage, DataBufferInt}
import java.lang.invoke.{MethodHandles, MethodType}

import scala.annotation.nowarn
import scala.util.Random

import org.jetbrains.skia.{
  Canvas,
  ClipMode,
  ColorFilter,
  FilterBlurMode,
  FilterMipmap,
  FilterMode,
  FilterTileMode,
  IRect,
  Image,
  ImageFilter,
  ImageInfo,
  MaskFilter,
  MipmapMode,
  Paint,
  PaintMode,
  RRect,
  Rect,
  Shader,
  Surface
}

/** PROBE for #2465 (throwaway, not product code): what the #1812 glass recipe costs for one floating panel, recomputed
  * every frame against cached, on Skia's CPU raster, with the Java2D-style CPU blur it would otherwise need next to it.
  * Container numbers are CPU-raster numbers; a GPU run is a separate laptop job.
  *
  * Run: `Test/runMain com.serenity.perf.GlassProbe [scale] [iterations]`.
  */
@nowarn
object GlassProbe:

  final private case class Layout(frameW: Int, frameH: Int, panel: Rect, radius: Float, scale: Float)

  private val BlurDivisor = 4
  private val BlurSigma   = 8f

  def main(args: Array[String]): Unit =
    if args.headOption.contains("layer") then layerMode(args.lift(1).flatMap(_.toIntOption).getOrElse(300))
    else cpuMode(args)

  private def cpuMode(args: Array[String]): Unit =
    val scale      = args.headOption.flatMap(_.toFloatOption).getOrElse(2f)
    val iterations = args.lift(1).flatMap(_.toIntOption).getOrElse(150)
    val layout = Layout(
      (1600 * scale).toInt,
      (1000 * scale).toInt,
      Rect.Companion.makeXYWH(450f * scale, 250f * scale, 600f * scale, 400f * scale),
      12f * scale,
      scale
    )
    println(s"GLASS-PROBE frame ${layout.frameW}x${layout.frameH}, panel ${layout.panel.getWidth.toInt}x${layout.panel.getHeight.toInt} px, scale $scale, $iterations iterations, Skia CPU raster")
    val backdrop = backdropImage(layout)
    val frame    = Surface.Companion.makeRaster(ImageInfo.Companion.makeN32Premul(layout.frameW, layout.frameH))
    val saturate = saturationFilter(1.4f)
    val cached   = blurredBackdrop(backdrop, layout, saturate)
    val shadows  = shadowSprite(layout)
    val noise    = noiseTile()

    def baseline(): Unit = frame.getCanvas.drawImage(backdrop, 0f, 0f)

    val variants: List[(String, () => Unit)] = List(
      "V0 opaque panel (no effects)"                         -> (() => opaque(frame.getCanvas, layout)),
      "V1 full recipe recomputed every frame"                -> (() => glass(frame, layout, backdrop, saturate, noise, recompute = true, cachedBlur = cached, cachedShadows = None)),
      "V2 cached blur, shadows recomputed"                   -> (() => glass(frame, layout, backdrop, saturate, noise, recompute = false, cachedBlur = cached, cachedShadows = None)),
      "V3 cached blur and cached shadow sprite"              -> (() => glass(frame, layout, backdrop, saturate, noise, recompute = false, cachedBlur = cached, cachedShadows = Some(shadows))),
      "V4 Java2D-style CPU blur recomputed (box x3, 1/4 res)" -> (() => java2dStyle.run(layout))
    )

    println(f"${"frame reset only (backdrop copy)"}%-58s ${timeIt(iterations, () => baseline(), () => ())}")
    variants.foreach { (name, work) =>
      println(f"$name%-58s ${timeIt(iterations, () => baseline(), work)}")
    }
    println("-- per step, each alone on the same canvas (p50 / p95) --")
    val rr = panelRRect(layout)
    def stepPaint(configure: Paint => Unit): Paint = { val p = new Paint(); configure(p); p }
    val steps: List[(String, () => Unit)] = List(
      "1-3 blur backdrop at 1/4 res + saturate (recompute)" -> (() => { val _ = blurredBackdrop(backdrop, layout, saturate) }),
      "composite cached blur into the clipped panel" -> (() => {
        val c = frame.getCanvas; val _ = c.save(); c.clipRRect(rr, true)
        c.drawImageRect(cached, Rect.Companion.makeXYWH(0f, 0f, cached.getWidth.toFloat, cached.getHeight.toFloat), layout.panel,
          FilterMipmap(FilterMode.LINEAR, MipmapMode.NONE), null, true)
        c.restore() }),
      "tint (one rect, alpha 0.72)" -> (() => frame.getCanvas.drawRect(layout.panel, stepPaint(_.setColor(0xb82a2f40.toInt)))),
      "noise (tiled sprite, alpha 0.02)" -> (() => drawTiled(frame.getCanvas, noise, layout.panel, stepPaint(_.setAlphaf(0.02f)))),
      "two soft shadows (mask-filter blur)" -> (() => drawShadows(frame.getCanvas, layout)),
      "shadow sprite copy" -> (() => frame.getCanvas.drawImage(shadows._1, layout.panel.getLeft - shadows._2, layout.panel.getTop - shadows._2)),
      "highlight + hairline (two strokes)" -> (() => {
        val e = stepPaint { p => p.setMode(PaintMode.STROKE); p.setStrokeWidth(layout.scale); p.setColor(0x40ffffff) }
        frame.getCanvas.drawRRect(rr, e); frame.getCanvas.drawRRect(rr, e) })
    )
    steps.foreach { (name, work) => println(f"$name%-58s ${timeIt(iterations, () => baseline(), work)}") }
    java2dStyle.close()

  /** The same panel on Skiko's GPU layer in a real window, one variant after another, each drawn as fast as Skiko
    * lets it (run with -Dskiko.vsync.enabled=false so frames are not paced to the display). The figure is the time
    * between frames, which on a GPU includes the GPU finishing them; the render API is printed so a software fallback
    * cannot be read as a GPU result.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private def layerMode(framesPerVariant: Int): Unit =
    val frame = new JFrame("glass probe")
    val layer = new org.jetbrains.skiko.SkiaLayer(
      null,
      new org.jetbrains.skiko.SkiaLayerProperties(),
      org.jetbrains.skiko.RenderFactory.Companion.getDefault(),
      org.jetbrains.skiko.SkiaLayerAnalytics.Companion.getEmpty(),
      org.jetbrains.skia.PixelGeometry.UNKNOWN,
      false
    )
    @volatile var prepared: Option[(Layout, Image, ColorFilter, Image, Image, (Image, Float), Image)] = None
    val variant = new AtomicInteger(0)
    val count   = new AtomicInteger(0)
    val last    = new AtomicLong(0L)
    val deltas  = Array.fill(4)(Array.ofDim[Long](framesPerVariant))
    val names   = Vector("G0 opaque panel", "G1 backdrop blur on the GPU every frame", "G2 sprite of the finished glass panel", "G3 blur recomputed from the cached blur image")
    def prepare(scale: Float): (Layout, Image, ColorFilter, Image, Image, (Image, Float), Image) =
      val layout   = Layout((1600 * scale).toInt, (1000 * scale).toInt, Rect.Companion.makeXYWH(450f * scale, 250f * scale, 600f * scale, 400f * scale), 12f * scale, scale)
      val backdrop = backdropImage(layout)
      val saturate = saturationFilter(1.4f)
      val cachedBlur = blurredBackdrop(backdrop, layout, saturate)
      val noise      = noiseTile()
      val shadows    = shadowSprite(layout)
      val bleed      = shadows._2
      val spriteW    = (layout.panel.getWidth + 2 * bleed).toInt
      val spriteH    = (layout.panel.getHeight + 2 * bleed).toInt
      val sprite     = Surface.Companion.makeRaster(ImageInfo.Companion.makeN32Premul(spriteW, spriteH))
      val inSprite   = Layout(spriteW, spriteH, Rect.Companion.makeXYWH(bleed, bleed, layout.panel.getWidth, layout.panel.getHeight), layout.radius, scale)
      val spriteBack = Surface.Companion.makeRaster(ImageInfo.Companion.makeN32Premul(spriteW, spriteH))
      spriteBack.getCanvas.drawImageRect(backdrop, layout.panel.inflate(bleed), Rect.Companion.makeXYWH(0f, 0f, spriteW.toFloat, spriteH.toFloat))
      sprite.getCanvas.drawImage(spriteBack.makeImageSnapshot(), 0f, 0f)
      glass(sprite, inSprite, backdrop, saturate, noise, recompute = false, cachedBlur = cachedBlur, cachedShadows = Some(shadows))
      (layout, backdrop, saturate, cachedBlur, noise, shadows, sprite.makeImageSnapshot())
    layer.setRenderDelegate(new org.jetbrains.skiko.SkikoRenderDelegate:
      override def onRender(canvas: Canvas, width: Int, height: Int, nanoTime: Long): Unit =
        val ready = prepared.getOrElse {
          val made = prepare(layer.getContentScale)
          prepared = Some(made)
          made
        }
        val (layout, backdrop, saturate, cachedBlur, noise, shadows, sprite) = ready
        canvas.drawImage(backdrop, 0f, 0f)
        val v = variant.get()
        v match
          case 0 => opaque(canvas, layout)
          case 1 =>
            drawShadows(canvas, layout)
            val rr   = panelRRect(layout)
            val blur = ImageFilter.Companion.makeBlur(BlurSigma * layout.scale, BlurSigma * layout.scale, FilterTileMode.CLAMP, null, null)
            val both = ImageFilter.Companion.makeColorFilter(saturate, blur, null)
            val _    = canvas.save()
            canvas.clipRRect(rr, true)
            val _ = canvas.saveLayer(new Canvas.SaveLayerRec(layout.panel, null, both, null, new Canvas.SaveLayerFlags(0)))
            canvas.restore()
            val tint = new Paint()
            tint.setColor(0xb82a2f40.toInt)
            canvas.drawRect(layout.panel, tint)
            val grain = new Paint()
            grain.setAlphaf(0.02f)
            drawTiled(canvas, noise, layout.panel, grain)
            canvas.restore()
            tint.close(); grain.close()
          case 2 => canvas.drawImage(sprite, layout.panel.getLeft - shadows._2, layout.panel.getTop - shadows._2)
          case _ =>
            val _ = canvas.save()
            canvas.clipRRect(panelRRect(layout), true)
            canvas.drawImageRect(cachedBlur, Rect.Companion.makeXYWH(0f, 0f, cachedBlur.getWidth.toFloat, cachedBlur.getHeight.toFloat), layout.panel,
              FilterMipmap(FilterMode.LINEAR, MipmapMode.NONE), null, true)
            canvas.restore()
        val now = System.nanoTime()
        val n   = count.getAndIncrement()
        if n > 0 && v < deltas.length && n - 1 < framesPerVariant then deltas(v)(n - 1) = now - last.get()
        last.set(now)
        if n >= framesPerVariant then
          count.set(0)
          if variant.incrementAndGet() >= deltas.length then finish()
        layer.needRedraw()
      def finish(): Unit =
        println(s"GLASS-PROBE layer mode, render API ${layer.getRenderApi}, content scale ${layer.getContentScale}, $framesPerVariant frames per variant")
        deltas.zip(names).foreach { (d, name) =>
          val sorted = d.sorted
          println(f"$name%-48s p50 ${sorted(sorted.length / 2) / 1e6}%7.3f ms  p95 ${sorted((sorted.length * 0.95).toInt) / 1e6}%7.3f ms  max ${sorted.last / 1e6}%7.3f ms")
        }
        sys.exit(0)
    )
    SwingUtilities.invokeAndWait { () =>
      frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      layer.setPreferredSize(new Dimension(1600, 1000))
      frame.getContentPane.add(layer)
      frame.pack()
      frame.setVisible(true)
    }
    SwingUtilities.invokeLater(() => layer.needRedraw())

  private def timeIt(iterations: Int, reset: () => Unit, work: () => Unit): String =
    val samples = Array.ofDim[Long](iterations)
    (0 until 40).foreach { _ => reset(); work() }
    (0 until iterations).foreach { i =>
      reset()
      val started = System.nanoTime()
      work()
      samples(i) = System.nanoTime() - started
    }
    val sorted = samples.sorted
    def at(p: Double) = sorted(((iterations - 1) * p).toInt) / 1e6
    f"p50 ${at(0.5)}%7.3f ms   p95 ${at(0.95)}%7.3f ms   max ${at(1.0)}%7.3f ms"

  private def backdropImage(layout: Layout): Image =
    val surface = Surface.Companion.makeRaster(ImageInfo.Companion.makeN32Premul(layout.frameW, layout.frameH))
    val canvas  = surface.getCanvas
    canvas.clear(0xff1e2230.toInt)
    val paint  = new Paint()
    val random = new Random(42)
    val lineH  = (18 * layout.scale).toInt
    (0 until layout.frameH / lineH).foreach { row =>
      var x = (40 * layout.scale).toInt
      while x < layout.frameW - 80 do
        val w = ((20 + random.nextInt(120)) * layout.scale).toInt
        paint.setColor(0xff000000 | (0x60 + random.nextInt(0x90) << 16) | (0x60 + random.nextInt(0x90) << 8) | (0x80 + random.nextInt(0x70)))
        canvas.drawRect(Rect.Companion.makeXYWH(x.toFloat, (row * lineH + 3).toFloat, w.toFloat, (lineH - 8).toFloat), paint)
        x += w + (8 * layout.scale).toInt
    }
    paint.close()
    surface.makeImageSnapshot()

  private def saturationFilter(s: Float): ColorFilter =
    val matrix = Array(
      0.213f + 0.787f * s, 0.715f - 0.715f * s, 0.072f - 0.072f * s, 0f, 0f,
      0.213f - 0.213f * s, 0.715f + 0.285f * s, 0.072f - 0.072f * s, 0f, 0f,
      0.213f - 0.213f * s, 0.715f - 0.715f * s, 0.072f + 0.928f * s, 0f, 0f,
      0f, 0f, 0f, 1f, 0f
    )
    val handle = MethodHandles
      .publicLookup()
      .findVirtual(
        ColorFilter.Companion.getClass,
        "makeMatrix-4761tnY",
        MethodType.methodType(classOf[ColorFilter], classOf[Array[Float]])
      )
    val result: Object = handle.invoke(ColorFilter.Companion, matrix)
    result match
      case filter: ColorFilter => filter
      case other               => throw new IllegalStateException(s"no colour filter: $other")

  private def panelRRect(layout: Layout): RRect = RRect.Companion.makeLTRB(
    layout.panel.getLeft, layout.panel.getTop, layout.panel.getRight, layout.panel.getBottom, layout.radius
  )

  /** The backdrop behind the panel, taken at 1/4 resolution, blurred and saturated: steps 1 to 3 of the recipe. */
  private def blurredBackdrop(backdrop: Image, layout: Layout, saturate: ColorFilter): Image =
    val w     = (layout.panel.getWidth / BlurDivisor).toInt
    val h     = (layout.panel.getHeight / BlurDivisor).toInt
    val small = Surface.Companion.makeRaster(ImageInfo.Companion.makeN32Premul(w, h))
    small.getCanvas.drawImageRect(
      backdrop,
      layout.panel,
      Rect.Companion.makeXYWH(0f, 0f, w.toFloat, h.toFloat),
      FilterMipmap(FilterMode.LINEAR, MipmapMode.NONE),
      null,
      true
    )
    val blurred = Surface.Companion.makeRaster(ImageInfo.Companion.makeN32Premul(w, h))
    val paint   = new Paint()
    paint.setImageFilter(ImageFilter.Companion.makeBlur(BlurSigma, BlurSigma, FilterTileMode.CLAMP, null, null))
    paint.setColorFilter(saturate)
    blurred.getCanvas.drawImage(small.makeImageSnapshot(), 0f, 0f, paint)
    paint.close()
    blurred.makeImageSnapshot()

  private def shadowPaint(layout: Layout, sigma: Float, alpha: Int): Paint =
    val paint = new Paint()
    paint.setColor((alpha << 24) | 0x000000)
    paint.setMaskFilter(MaskFilter.Companion.makeBlur(FilterBlurMode.NORMAL, sigma * layout.scale, true))
    paint

  private def drawShadows(canvas: Canvas, layout: Layout): Unit =
    val _ = canvas.save()
    val rr = panelRRect(layout)
    canvas.clipRRect(rr, ClipMode.DIFFERENCE, true)
    val far  = shadowPaint(layout, 12f, 0x59)
    val near = shadowPaint(layout, 3f, 0x40)
    canvas.save()
    canvas.translate(0f, 10f * layout.scale)
    canvas.drawRRect(rr, far)
    canvas.restore()
    canvas.save()
    canvas.translate(0f, 2f * layout.scale)
    canvas.drawRRect(rr, near)
    canvas.restore()
    canvas.restore()
    far.close()
    near.close()

  /** Both shadows drawn once into an image the size of the panel plus its bleed, to be copied on later frames. */
  private def shadowSprite(layout: Layout): (Image, Float) =
    val bleed   = 48f * layout.scale
    val w       = (layout.panel.getWidth + 2 * bleed).toInt
    val h       = (layout.panel.getHeight + 2 * bleed).toInt
    val surface = Surface.Companion.makeRaster(ImageInfo.Companion.makeN32Premul(w, h))
    val shifted = Layout(w, h, Rect.Companion.makeXYWH(bleed, bleed, layout.panel.getWidth, layout.panel.getHeight), layout.radius, layout.scale)
    drawShadows(surface.getCanvas, shifted)
    (surface.makeImageSnapshot(), bleed)

  /** The noise layer is a tile rendered once, as a shipped recipe would have it; evaluating a procedural noise shader on
    * every pixel of the panel on the CPU every frame is not what the recipe means.
    */
  private def noiseTile(): Image =
    val surface = Surface.Companion.makeRaster(ImageInfo.Companion.makeN32Premul(256, 256))
    val paint   = new Paint()
    paint.setShader(Shader.Companion.makeFractalNoise(0.9f, 0.9f, 2, 3f, org.jetbrains.skia.ISize.Companion.make(256, 256)))
    surface.getCanvas.drawRect(Rect.Companion.makeXYWH(0f, 0f, 256f, 256f), paint)
    paint.close()
    surface.makeImageSnapshot()

  private def drawTiled(canvas: Canvas, tile: Image, area: Rect, paint: Paint): Unit =
    var y = area.getTop
    while y < area.getBottom do
      var x = area.getLeft
      while x < area.getRight do
        canvas.drawImage(tile, x, y, paint)
        x += tile.getWidth
      y += tile.getHeight

  private def opaque(canvas: Canvas, layout: Layout): Unit =
    val paint = new Paint()
    paint.setColor(0xff2a2f40.toInt)
    canvas.drawRRect(panelRRect(layout), paint)
    paint.setMode(PaintMode.STROKE)
    paint.setStrokeWidth(layout.scale)
    paint.setColor(0x66ffffff)
    canvas.drawRRect(panelRRect(layout), paint)
    paint.close()

  private def glass(
    frame: Surface,
    layout: Layout,
    backdrop: Image,
    saturate: ColorFilter,
    noise: Image,
    recompute: Boolean,
    cachedBlur: Image,
    cachedShadows: Option[(Image, Float)]
  ): Unit =
    val canvas = frame.getCanvas
    cachedShadows match
      case Some((sprite, bleed)) =>
        canvas.drawImage(sprite, layout.panel.getLeft - bleed, layout.panel.getTop - bleed)
      case None => drawShadows(canvas, layout)
    val blurred =
      if recompute then
        val region = frame.makeImageSnapshot(
          IRect.Companion.makeXYWH(
            layout.panel.getLeft.toInt, layout.panel.getTop.toInt, layout.panel.getWidth.toInt, layout.panel.getHeight.toInt
          )
        )
        blurredBackdrop(backdrop, layout, saturate).tap(_ => region.close())
      else cachedBlur
    val rr = panelRRect(layout)
    val _  = canvas.save()
    canvas.clipRRect(rr, true)
    canvas.drawImageRect(blurred, Rect.Companion.makeXYWH(0f, 0f, blurred.getWidth.toFloat, blurred.getHeight.toFloat), layout.panel,
      FilterMipmap(FilterMode.LINEAR, MipmapMode.NONE), null, true)
    val tint = new Paint()
    tint.setColor(0xb82a2f40.toInt)
    canvas.drawRect(layout.panel, tint)
    val grain = new Paint()
    grain.setAlphaf(0.02f)
    drawTiled(canvas, noise, layout.panel, grain)
    canvas.restore()
    val edge = new Paint()
    edge.setMode(PaintMode.STROKE)
    edge.setStrokeWidth(layout.scale)
    edge.setColor(0x40ffffff)
    canvas.drawRRect(rr, edge)
    edge.setColor(0x66000000)
    canvas.drawRRect(RRect.Companion.makeLTRB(layout.panel.getLeft - layout.scale, layout.panel.getTop - layout.scale, layout.panel.getRight + layout.scale, layout.panel.getBottom + layout.scale, layout.radius + layout.scale), edge)
    tint.close()
    grain.close()
    edge.close()

  extension [A](a: A)
    private def tap(f: A => Unit): A =
      f(a)
      a

  /** What Java2D has: no blur filter, so a downscale, three box passes on the pixels and an upscale, all on the CPU. */
  private object java2dStyle:

    private val frame = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
    private var state: Option[(BufferedImage, BufferedImage)] = None

    private def images(layout: Layout): (BufferedImage, BufferedImage) =
      state.getOrElse {
        val back = new BufferedImage(layout.frameW, layout.frameH, BufferedImage.TYPE_INT_ARGB)
        val g    = back.createGraphics()
        g.setColor(new java.awt.Color(0x1e2230))
        g.fillRect(0, 0, layout.frameW, layout.frameH)
        val random = new Random(42)
        val lineH  = (18 * layout.scale).toInt
        (0 until layout.frameH / lineH).foreach { row =>
          var x = (40 * layout.scale).toInt
          while x < layout.frameW - 80 do
            val w = ((20 + random.nextInt(120)) * layout.scale).toInt
            g.setColor(new java.awt.Color(0x60 + random.nextInt(0x90), 0x60 + random.nextInt(0x90), 0x80 + random.nextInt(0x70)))
            g.fillRect(x, row * lineH + 3, w, lineH - 8)
            x += w + (8 * layout.scale).toInt
        }
        g.dispose()
        val target = new BufferedImage(layout.frameW, layout.frameH, BufferedImage.TYPE_INT_ARGB)
        state = Some((back, target))
        (back, target)
      }

    def run(layout: Layout): Unit =
      val (back, target) = images(layout)
      val x = layout.panel.getLeft.toInt
      val y = layout.panel.getTop.toInt
      val w = layout.panel.getWidth.toInt
      val h = layout.panel.getHeight.toInt
      val sw = w / BlurDivisor
      val sh = h / BlurDivisor
      val small = new BufferedImage(sw, sh, BufferedImage.TYPE_INT_ARGB)
      val sg    = small.createGraphics()
      sg.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      sg.drawImage(back, 0, 0, sw, sh, x, y, x + w, y + h, null)
      sg.dispose()
      val pixels = small.getRaster.getDataBuffer match
        case data: DataBufferInt => data.getData
        case _                   => throw new IllegalStateException("not int pixels")
      (0 until 3).foreach(_ => boxBlur(pixels, sw, sh, 3))
      val tg = target.createGraphics()
      tg.drawImage(back, 0, 0, null)
      tg.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      tg.setClip(new RoundRectangle2D.Float(x.toFloat, y.toFloat, w.toFloat, h.toFloat, layout.radius * 2, layout.radius * 2))
      tg.drawImage(small, x, y, w, h, null)
      tg.setComposite(java.awt.AlphaComposite.getInstance(java.awt.AlphaComposite.SRC_OVER, 0.72f))
      tg.setColor(new java.awt.Color(0x2a2f40))
      tg.fillRect(x, y, w, h)
      tg.dispose()

    private def boxBlur(px: Array[Int], w: Int, h: Int, radius: Int): Unit =
      val tmp = Array.ofDim[Int](px.length)
      def pass(src: Array[Int], dst: Array[Int], lineLen: Int, lines: Int, stride: Int, step: Int): Unit =
        (0 until lines).foreach { line =>
          val base = line * stride
          (0 until lineLen).foreach { i =>
            var a = 0; var r = 0; var g = 0; var b = 0; var n = 0
            var k = -radius
            while k <= radius do
              val j = (i + k).max(0).min(lineLen - 1)
              val p = src(base + j * step)
              a += p >>> 24; r += (p >> 16) & 0xff; g += (p >> 8) & 0xff; b += p & 0xff; n += 1
              k += 1
            dst(base + i * step) = ((a / n) << 24) | ((r / n) << 16) | ((g / n) << 8) | (b / n)
          }
        }
      pass(px, tmp, w, h, w, 1)
      pass(tmp, px, h, w, 1, w)

    def close(): Unit = state = None
