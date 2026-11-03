package com.serenity.ui.skia

import java.awt.image.BufferedImage
import java.awt.{Color, Font}

import com.serenity.SkiaPixels
import com.serenity.diagnostics.{FrameCounter, FrameCounters}
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.renderer.{FontSpec, RenderImage, StyledFontCache, SurfaceActivity}
import com.serenity.ui.theme.TextStyle
import org.jetbrains.skia.Surface
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[SkiaRenderSurface]] on an off-screen raster `Surface`: the CPU rasteriser, so it needs no window and runs in CI.
  *
  * Colours, rectangles, clips and copies are asserted to the pixel, because Skia fills a pixel-aligned rectangle
  * exactly. Text is asserted by geometry only (where ink lies, how far a run advances), never by the shape of a glyph,
  * since two rasterisers disagree about antialiasing.
  */
class SkiaRenderSurfaceSpec extends AnyFlatSpec with Matchers:

  private val metrics     = CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
  private val caches      = SkiaTextCaches.fresh()
  private val styledFonts = StyledFontCache()
  private val awtFont     = new Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val font        = FontSpec.fromAwt(awtFont)
  private val width       = 210
  private val height      = 130
  private val navy        = new Color(10, 20, 60)

  final private class Canvas(val surface: Surface, val scale: Double):
    val renderSurface: SkiaRenderSurface =
      SkiaRenderSurface.retaining(surface, metrics, font, width, height, scale, caches, styledFonts)

    def pixels: Array[Int] = SkiaPixels.read(surface)

    def deviceWidth: Int = surface.getWidth

    def at(x: Int, y: Int): Int = pixels(y * deviceWidth + x)

  private def canvasAt(scale: Double = 1.0): Canvas =
    new Canvas(
      Surface.Companion.makeRasterN32Premul(
        SkiaRenderSurface.deviceDimension(width, scale),
        SkiaRenderSurface.deviceDimension(height, scale)
      ),
      scale
    )

  private def colour(awt: Color): RenderColor = RenderColor.fromAwt(awt)

  private def inkBox(canvas: Canvas, background: Int): Option[(Int, Int, Int, Int)] =
    val pixels = canvas.pixels
    val inked = for
      y <- 0 until canvas.surface.getHeight
      x <- 0 until canvas.deviceWidth
      if pixels(y * canvas.deviceWidth + x) != background
    yield (x, y)
    Option.when(inked.nonEmpty)(
      (inked.map(_._1).min, inked.map(_._2).min, inked.map(_._1).max + 1, inked.map(_._2).max + 1)
    )

  private def painted(canvas: Canvas)(draw: SkiaRenderSurface => Unit): Canvas =
    draw(canvas.renderSurface)
    canvas.renderSurface.flush()
    canvas

  private def blackWhite(surface: SkiaRenderSurface): Unit =
    surface.clearViewport(RenderColor.Black)
    surface.setBackgroundColor(RenderColor.Black)
    surface.setForegroundColor(RenderColor.White)

  "A surface over a window's canvas" should "declare that it keeps nothing between frames and has no layer buffers" in {
    val surface = Surface.Companion.makeRasterN32Premul(width, height)
    val onCanvas =
      SkiaRenderSurface.onCanvas(
        surface.getCanvas,
        metrics,
        font,
        width,
        height,
        deviceScale = 1.0,
        caches = caches,
        styledFonts = styledFonts
      )

    onCanvas.persistentContentKey shouldBe None
    onCanvas.pixelCopying shouldBe None
    onCanvas.layerBuffers shouldBe None
    onCanvas.addressesPixels shouldBe true
    onCanvas.effects should not be empty
    onCanvas.panelOutlines should not be empty
    onCanvas.hardwareCursor shouldBe None
  }

  "A surface retaining a raster surface" should "key its content on that surface and copy pixels at whole device scales" in {
    val first  = Surface.Companion.makeRasterN32Premul(width, height)
    val second = Surface.Companion.makeRasterN32Premul(width, height)
    def retaining(over: Surface, scale: Double) =
      SkiaRenderSurface.retaining(over, metrics, font, width, height, scale, caches, styledFonts)

    retaining(first, 1.0).persistentContentKey shouldBe retaining(first, 1.0).persistentContentKey
    retaining(first, 1.0).persistentContentKey should not be retaining(second, 1.0).persistentContentKey
    retaining(first, 2.0).pixelCopying should not be empty
    retaining(first, 1.5).pixelCopying shouldBe None
    retaining(first, 1.0).layerBuffers shouldBe None
  }

  it should "report the viewport in cells and the device scale it was built with" in {
    val surface =
      SkiaRenderSurface.retaining(canvasAt(2.0).surface, metrics, font, width, height, 2.0, caches, styledFonts)

    surface.viewportWidth shouldBe width / metrics.charWidth
    surface.viewportHeight shouldBe height / metrics.lineHeight
    surface.devicePixelScaleX shouldBe 2.0
    surface.devicePixelScaleY shouldBe 2.0
  }

  it should "measure text in the render context layout shares, scaled to the device" in {
    val surface =
      SkiaRenderSurface.retaining(canvasAt(2.0).surface, metrics, font, width, height, 2.0, caches, styledFonts)

    val context = surface.text.fontRenderContext.getOrElse(fail("expected a font render context"))
    context.getTransform.getScaleX shouldBe 2.0
    context.getTransform.getScaleY shouldBe 2.0
    context.isAntiAliased shouldBe true
    context.usesFractionalMetrics shouldBe true
  }

  "fillPixelRect" should "paint exactly the device pixels of a logical rectangle, at 2x" in {
    val canvas = painted(canvasAt(2.0))(surface => surface.pixels.fillPixelRect(10, 20, 5, 3, colour(Color.RED)))

    canvas.at(20, 40) shouldBe Color.RED.getRGB
    canvas.at(29, 45) shouldBe Color.RED.getRGB
    canvas.at(19, 40) shouldBe 0
    canvas.at(30, 40) shouldBe 0
    canvas.at(20, 39) shouldBe 0
    canvas.at(20, 46) shouldBe 0
  }

  it should "paint at least one pixel for an empty rectangle, as Java2D does" in {
    val canvas = painted(canvasAt())(surface => surface.pixels.fillPixelRect(10, 20, 0, 0, colour(Color.GREEN)))

    canvas.at(10, 20) shouldBe Color.GREEN.getRGB
  }

  "setAlpha" should "blend the fills that follow instead of replacing the pixels" in {
    val canvas = painted(canvasAt()) { surface =>
      blackWhite(surface)
      surface.effects.getOrElse(fail("expected alpha compositing")).setAlpha(0.5f)
      surface.pixels.fillPixelRect(0, 0, 10, 10, colour(Color.WHITE))
    }

    val grey = canvas.at(5, 5) & 0xff
    // 0.5 * 255 = 127.5 rounds one way or the other inside Skia's premultiplied blend
    grey should (be >= 127 and be <= 129)
  }

  "clearViewport" should "cover the whole viewport with the colour" in {
    val canvas = painted(canvasAt(2.0))(_.clearViewport(colour(navy)))

    canvas.at(0, 0) shouldBe navy.getRGB
    canvas.at(2 * width - 1, 2 * height - 1) shouldBe navy.getRGB
  }

  it should "leave black for a fully transparent colour on a surface with no alpha of its own" in {
    val canvas = painted(canvasAt()) { surface =>
      surface.pixels.fillPixelRect(0, 0, width, height, colour(Color.WHITE))
      surface.clearViewport(RenderColor.fromArgb(0x00102030))
    }

    canvas.at(3, 3) shouldBe Color.BLACK.getRGB
  }

  it should "leave transparent pixels for a fully transparent colour on a surface that has alpha" in {
    val target = Surface.Companion.makeRasterN32Premul(width, height)
    val surface = new SkiaRenderSurface(
      target.getCanvas,
      metrics,
      font,
      () => (),
      width,
      height,
      opaque = false,
      caches = caches,
      styledFonts = styledFonts
    )
    surface.pixels.fillPixelRect(0, 0, width, height, colour(Color.WHITE))
    surface.clearViewport(RenderColor.fromArgb(0x00102030))
    surface.flush()

    SkiaPixels.read(target)(3 * width + 3) shouldBe 0
  }

  "clearViewportExcept" should "leave preserved rectangles alone and clear everything else, at each scale" in {
    val preserved = List(PixelRect(30, 40, 50, 20), PixelRect(150, 10, 40, 100))
    List(1.0, 2.0, 1.25).foreach { scale =>
      val canvas = canvasAt(scale)
      canvas.renderSurface.pixels.fillPixelRect(0, 0, width, height, colour(Color.WHITE))
      canvas.renderSurface.clearViewportExcept(colour(navy), preserved)
      canvas.renderSurface.flush()

      def device(value: Int): Int = math.round(value * scale).toInt
      // One device pixel either side of a preserved edge may round either way at a fractional scale, so only pixels
      // clear of every edge are asserted there.
      val margin = if scale == scale.floor then 0 else 2
      preserved.foreach { rect =>
        withClue(s"scale $scale, inside $rect: ") {
          canvas.at(device(rect.xPx) + margin, device(rect.yPx) + margin) shouldBe Color.WHITE.getRGB
          canvas.at(device(rect.rightPx) - 1 - margin, device(rect.bottomPx) - 1 - margin) shouldBe Color.WHITE.getRGB
        }
      }
      withClue(s"scale $scale, outside: ") {
        canvas.at(device(5), device(5)) shouldBe navy.getRGB
        canvas.at(device(100), device(100)) shouldBe navy.getRGB
        canvas.at(device(width - 3), device(height - 3)) shouldBe navy.getRGB
      }
    }
  }

  it should "clear everything when nothing is preserved" in {
    val canvas = painted(canvasAt()) { surface =>
      surface.pixels.fillPixelRect(0, 0, width, height, colour(Color.WHITE))
      surface.clearViewportExcept(colour(navy), Nil)
    }

    canvas.at(100, 60) shouldBe navy.getRGB
  }

  "withRectClip" should "keep text and fills inside the cell rectangle and restore the clip afterwards" in {
    val canvas = painted(canvasAt()) { surface =>
      blackWhite(surface)
      surface.panelOutlines.getOrElse(fail("expected rect clipping")).withRectClip(1, 2, 2, 1) {
        surface.pixels.fillPixelRect(0, 0, width, height, colour(Color.RED))
      }
      surface.pixels.fillPixelRect(100, 100, 4, 4, colour(Color.BLUE))
    }

    canvas.at(7, 26) shouldBe Color.RED.getRGB
    canvas.at(20, 38) shouldBe Color.RED.getRGB
    canvas.at(6, 26) shouldBe Color.BLACK.getRGB
    canvas.at(21, 26) shouldBe Color.BLACK.getRGB
    canvas.at(7, 25) shouldBe Color.BLACK.getRGB
    canvas.at(7, 39) shouldBe Color.BLACK.getRGB
    canvas.at(101, 101) shouldBe Color.BLUE.getRGB
  }

  "withPixelTranslation" should "move fills by the offset and move them back afterwards" in {
    val canvas = painted(canvasAt()) { surface =>
      surface.pixels.withPixelTranslation(30.0, 12.0)(surface.pixels.fillPixelRect(0, 0, 4, 4, colour(Color.RED)))
      surface.pixels.fillPixelRect(0, 0, 4, 4, colour(Color.GREEN))
    }

    canvas.at(31, 13) shouldBe Color.RED.getRGB
    canvas.at(1, 1) shouldBe Color.GREEN.getRGB
  }

  "strokeRect" should "draw a border of the stroke width inside the cell rectangle and leave the inside alone" in {
    val canvas = painted(canvasAt()) { surface =>
      blackWhite(surface)
      surface.panelOutlines.getOrElse(fail("expected outlines")).strokeRect(1, 1, 4, 3, colour(Color.RED), 2f)
    }

    // cells (1,1)-(5,4) are pixels x 7..35, y 13..52
    canvas.at(8, 30) shouldBe Color.RED.getRGB
    canvas.at(20, 14) shouldBe Color.RED.getRGB
    canvas.at(33, 30) shouldBe Color.RED.getRGB
    canvas.at(20, 51) shouldBe Color.RED.getRGB
    canvas.at(20, 30) shouldBe Color.BLACK.getRGB
    canvas.at(3, 30) shouldBe Color.BLACK.getRGB
  }

  "drawImage" should "stretch the image over its cell rectangle and clip to it" in {
    val source = new BufferedImage(2, 2, BufferedImage.TYPE_INT_ARGB)
    List((0, 0), (1, 0), (0, 1), (1, 1)).foreach((x, y) => source.setRGB(x, y, Color.RED.getRGB))
    val canvas = painted(canvasAt()) { surface =>
      blackWhite(surface)
      surface.pixels.drawImage(RenderImage.fromAwt(source), 2, 1, 3, 2)
    }

    canvas.at(14, 13) shouldBe Color.RED.getRGB
    canvas.at(34, 38) shouldBe Color.RED.getRGB
    canvas.at(13, 20) shouldBe Color.BLACK.getRGB
    canvas.at(35, 20) shouldBe Color.BLACK.getRGB
    canvas.at(20, 12) shouldBe Color.BLACK.getRGB
    canvas.at(20, 39) shouldBe Color.BLACK.getRGB
  }

  it should "convert a source image once, however often it is drawn" in {
    val cache  = new SkiaImageCache()
    val source = new BufferedImage(4, 4, BufferedImage.TYPE_INT_ARGB)

    val first = cache.imageOf(source)

    cache.imageOf(source) should be theSameInstanceAs first
    cache.size shouldBe 1
  }

  it should "keep the transparency of the source" in {
    val source = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
    source.setRGB(0, 0, 0x00ff0000)
    val canvas = painted(canvasAt()) { surface =>
      surface.clearViewport(colour(navy))
      surface.pixels.drawImage(RenderImage.fromAwt(source), 0, 0, 2, 2)
    }

    canvas.at(3, 3) shouldBe navy.getRGB
  }

  "copyPixels" should "move a band of pixels down, reading every source pixel as it was before the copy" in {
    val canvas  = canvasAt()
    val surface = canvas.renderSurface
    surface.pixels.fillPixelRect(0, 0, width, 10, colour(Color.RED))
    surface.pixels.fillPixelRect(0, 10, width, 10, colour(Color.GREEN))
    surface.pixels.fillPixelRect(0, 20, width, 10, colour(Color.BLUE))

    surface.pixelCopying.getOrElse(fail("expected pixel copying")).copyPixels(PixelRect(0, 0, width, 30), 10)
    surface.flush()

    canvas.at(5, 5) shouldBe Color.RED.getRGB
    canvas.at(5, 15) shouldBe Color.RED.getRGB
    canvas.at(5, 25) shouldBe Color.GREEN.getRGB
    canvas.at(5, 35) shouldBe Color.BLUE.getRGB
    canvas.at(5, 45) shouldBe 0
  }

  it should "move a band up, and by device pixels at 2x" in {
    val canvas  = canvasAt(2.0)
    val surface = canvas.renderSurface
    surface.pixels.fillPixelRect(0, 30, width, 10, colour(Color.RED))

    surface.pixelCopying.getOrElse(fail("expected pixel copying")).copyPixels(PixelRect(0, 30, width, 10), -20)
    surface.flush()

    canvas.at(10, 2 * 10) shouldBe Color.RED.getRGB
    canvas.at(10, 2 * 20 - 1) shouldBe Color.RED.getRGB
    canvas.at(10, 2 * 10 - 1) shouldBe 0
    canvas.at(10, 2 * 20) shouldBe 0
    canvas.at(10, 2 * 35) shouldBe Color.RED.getRGB
  }

  "A text run" should "paint its background across the run and its ink inside it" in {
    val canvas = painted(canvasAt()) { surface =>
      blackWhite(surface)
      surface.setBackgroundColor(colour(navy))
      surface.text.drawRunPx(14f, 26, 56f, metrics.lineHeight, metrics.ascent, "Hello")
    }

    canvas.at(60, 26) shouldBe navy.getRGB
    canvas.at(69, 38) shouldBe navy.getRGB
    canvas.at(13, 30) shouldBe Color.BLACK.getRGB
    canvas.at(70, 30) shouldBe Color.BLACK.getRGB
    val (left, top, right, bottom) = inkBox(canvas, Color.BLACK.getRGB).getOrElse(fail("expected a run"))
    left should be >= 14
    right should be <= 70
    top should be >= 26
    bottom should be <= 39
  }

  it should "start its ink where the run starts and end it within the measured advance" in {
    val text = "Hello world"
    val canvas = painted(canvasAt()) { surface =>
      blackWhite(surface)
      surface.text.drawRunPx(14f, 26, 100f, metrics.lineHeight, metrics.ascent, text)
    }

    val advance             = caches.fonts.faceFor(awtFont).font.measureTextWidth(text)
    val (left, _, right, _) = inkBox(canvas, Color.BLACK.getRGB).getOrElse(fail("expected ink"))
    // A glyph's ink can sit up to a pixel inside its advance box and antialiasing adds one more pixel each side.
    left should (be >= 13 and be <= 17)
    right.toFloat should (be >= 14f + advance - 3f and be <= 14f + advance + 1f)
  }

  it should "paint no ink for an empty run, only its background" in {
    val canvas = painted(canvasAt()) { surface =>
      blackWhite(surface)
      surface.setBackgroundColor(colour(navy))
      surface.text.drawRunPx(14f, 26, 56f, metrics.lineHeight, metrics.ascent, "")
    }

    inkBox(canvas, Color.BLACK.getRGB).map(box => (box._1, box._3)) shouldBe Some((14, 70))
    canvas.at(30, 30) shouldBe navy.getRGB
  }

  it should "clip its glyphs to the run when asked to" in {
    val canvas = painted(canvasAt()) { surface =>
      blackWhite(surface)
      surface.text.drawRunPx(14f, 26, 14f, metrics.lineHeight, metrics.ascent, "MMMMMMMM", clipGlyphToRun = true)
    }

    val (left, _, right, _) = inkBox(canvas, Color.BLACK.getRGB).getOrElse(fail("expected ink"))
    left should be >= 14
    right should be <= 28
  }

  it should "count the runs it draws" in {
    val activity = SurfaceActivity()
    val target   = Surface.Companion.makeRasterN32Premul(width, height)
    val surface = new SkiaRenderSurface(
      target.getCanvas,
      metrics,
      font,
      () => (),
      width,
      height,
      caches = caches,
      styledFonts = styledFonts,
      activity = activity
    )
    surface.text.drawRunPx(14f, 26, 56f, metrics.lineHeight, metrics.ascent, "a")
    surface.text.drawRunPx(14f, 39, 56f, metrics.lineHeight, metrics.ascent, "b")

    val counters = FrameCounters()
    counters.setEnabled(true)
    activity.drainInto(counters)
    counters.frameFinished()

    counters.lastFrame(FrameCounter.RunsDrawn) shouldBe 2L
  }

  "withFallbackFamily" should "shape a plain run only when asked to draw with ligatures on" in {
    def shapingsOf(ligaturesOn: Boolean): Long =
      val activity = SurfaceActivity()
      val target   = Surface.Companion.makeRasterN32Premul(width, height)
      val surface = new SkiaRenderSurface(
        target.getCanvas,
        metrics,
        font,
        () => (),
        width,
        height,
        caches = caches,
        styledFonts = styledFonts,
        activity = activity
      )
      surface.text.withFallbackFamily(Font.SERIF, ligaturesOn) {
        surface.text.drawRunPx(14f, 26, 56f, metrics.lineHeight, metrics.ascent, "office")
      }
      val counters = FrameCounters()
      counters.setEnabled(true)
      activity.drainInto(counters)
      counters.frameFinished()
      counters.lastFrame(FrameCounter.ShapingCalls)

    shapingsOf(ligaturesOn = false) shouldBe 0L
    shapingsOf(ligaturesOn = true) should be > 0L
  }

  "A styled run" should "draw an underline across the run and a strikethrough through it" in {
    def rows(style: TextStyle): Canvas =
      painted(canvasAt()) { surface =>
        blackWhite(surface)
        surface.enableStyle(style)
        surface.text.drawRunPx(14f, 26, 56f, metrics.lineHeight, metrics.ascent, "MMMMMMMM")
        surface.disableStyle(style)
      }
    def fullInkRow(canvas: Canvas, from: Int, until: Int): Boolean =
      (from until until).exists(y => (16 until 68).forall(x => canvas.at(x, y) != Color.BLACK.getRGB))

    fullInkRow(rows(TextStyle.underlined), 26 + metrics.ascent, 26 + metrics.lineHeight + 2) shouldBe true
    fullInkRow(rows(TextStyle.normal), 26 + metrics.ascent, 26 + metrics.lineHeight + 2) shouldBe false
    fullInkRow(rows(TextStyle(isStrikethrough = true)), 26 + 2, 26 + metrics.ascent) shouldBe true
    fullInkRow(rows(TextStyle.normal), 26 + 2, 26 + metrics.ascent) shouldBe false
  }

  "A run in a ligature font" should "report that its stretches are worth shaping and draw ranges that continue each other" in {
    val ligated = awtFont.deriveFont(
      java.util.Map.of(java.awt.font.TextAttribute.LIGATURES, java.awt.font.TextAttribute.LIGATURES_ON)
    )
    val target = Surface.Companion.makeRasterN32Premul(width, height)
    val surface =
      new SkiaRenderSurface(
        target.getCanvas,
        metrics,
        FontSpec.fromAwt(ligated),
        () => (),
        width,
        height,
        caches = caches,
        styledFonts = styledFonts
      )
    val stretch = "if a != b then c => d"
    surface.shapesStretchesOf(stretch) shouldBe true

    def paint(draw: SkiaRenderSurface => Unit): Array[Int] =
      val over = Surface.Companion.makeRasterN32Premul(width, height)
      val s = new SkiaRenderSurface(
        over.getCanvas,
        metrics,
        FontSpec.fromAwt(ligated),
        () => (),
        width,
        height,
        caches = caches,
        styledFonts = styledFonts
      )
      blackWhite(s)
      draw(s)
      s.flush()
      SkiaPixels.read(over)

    val shaped  = caches.shapedText.stretchOf(stretch, caches.fonts.faceFor(ligated).font)
    val splitAt = 8
    val splitX  = shaped.leftEdgeOf(splitAt).getOrElse(fail("expected a glyph at the split"))
    val whole =
      paint(_.text.drawShapedRunPx(14f, 26, 150f, metrics.lineHeight, metrics.ascent, stretch, 0, stretch.length))
    val halves = paint { s =>
      s.text.drawShapedRunPx(14f, 26, 150f, metrics.lineHeight, metrics.ascent, stretch, 0, splitAt)
      s.text.drawShapedRunPx(
        14f + splitX,
        26,
        150f,
        metrics.lineHeight,
        metrics.ascent,
        stretch,
        splitAt,
        stretch.length
      )
    }

    // Blobs are positioned at 14 and 14 + splitX in both cases, so the ink must agree except where the second half's
    // background fill covers the first half's last antialiased column.
    val differing = whole.indices.count(index => whole(index) != halves(index))
    differing should be <= 2 * metrics.lineHeight
  }

  "A Skia surface over a window's canvas" should "offer no pixel lifting, since nothing it draws is kept" in {
    val target = Surface.Companion.makeRasterN32Premul(width, height)
    val surface = SkiaRenderSurface.onCanvas(
      target.getCanvas,
      metrics,
      font,
      width,
      height,
      deviceScale = 1.0,
      caches = caches,
      styledFonts = styledFonts
    )

    surface.pixelLifting shouldBe None
  }

  "A Skia surface over a retained raster surface" should "offer pixel lifting along with the pixels it keeps" in {
    val target  = Surface.Companion.makeRasterN32Premul(width, height)
    val surface = SkiaRenderSurface.retaining(target, metrics, font, width, height, 1.0, caches, styledFonts)

    surface.pixelLifting shouldBe defined
    surface.persistentContentKey shouldBe defined
  }

  "SkiaFontCache" should "hand the same face for equal fonts and build each font once, whichever thread asks first" in {
    val cache = new SkiaFontCache()
    val fonts = (0 until 6).map(size => new Font(Font.SANS_SERIF, Font.PLAIN, 10 + size)).toVector
    val pool  = java.util.concurrent.Executors.newFixedThreadPool(8)
    try
      val asked = (0 until 64).map { index =>
        pool.submit(
          new java.util.concurrent.Callable[SkiaFace]:
            def call(): SkiaFace = cache.faceFor(fonts(index % fonts.size))
        )
      }
      val faces = asked.map(_.get())
      faces.groupBy(face => System.identityHashCode(face)).size shouldBe fonts.size
      cache.size shouldBe fonts.size
    finally pool.shutdown()
  }

  it should "size a face from the AWT font, with unhinted fractional metrics" in {
    val face = new SkiaFontCache().faceFor(new Font(Font.SERIF, Font.PLAIN, 17))

    face.font.getSize shouldBe 17f
    face.strikethrough shouldBe None
  }

  it should "give a strikethrough font a rule to draw" in {
    val struck = new Font(Font.SERIF, Font.PLAIN, 17)
      .deriveFont(
        java.util.Map.of(java.awt.font.TextAttribute.STRIKETHROUGH, java.awt.font.TextAttribute.STRIKETHROUGH_ON)
      )

    new SkiaFontCache().faceFor(struck).strikethrough should not be empty
  }

  it should "forget its faces rather than grow without bound" in {
    val cache = new SkiaFontCache(maxFaces = 4)

    (10 until 30).foreach(size => cache.faceFor(new Font(Font.SERIF, Font.PLAIN, size)))

    cache.size should be <= 4
  }
