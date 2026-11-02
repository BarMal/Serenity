package com.serenity.perf

import java.awt.image.{BufferedImage, DataBufferInt}
import java.awt.{Color, Font as AwtFont, RenderingHints}
import java.lang.management.ManagementFactory
import java.nio.{ByteBuffer, ByteOrder}

import org.jetbrains.skia.impl.BufferUtil
import org.jetbrains.skia.{
  Bitmap,
  BlendMode,
  Font as SkFont,
  FontEdging,
  FontHinting,
  FontMgr,
  IRect,
  ImageInfo,
  Paint,
  Pixmap,
  Surface
}

// The loops and counters here are what is being measured: a copy loop written as a fold would time the fold.
// scalafix:off DisableSyntax

/** Scratch probe for #2380: the unit cost of each step the Skia raster path takes, against the alternatives that could
  * replace it. Nothing here is production code.
  */
private[perf] object SkiaCostProbe:

  private val bean = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]

  final case class Cost(ms: Double, allocMb: Double)

  private def measure(name: String, reps: Int)(body: => Unit): Unit =
    (0 until 15).foreach(_ => body)
    val times = new Array[Double](reps)
    var alloc = 0L
    val tid   = Thread.currentThread().threadId()
    var i     = 0
    while i < reps do
      val a0 = bean.getThreadAllocatedBytes(tid)
      val t0 = System.nanoTime()
      body
      times(i) = (System.nanoTime() - t0) / 1e6
      alloc += bean.getThreadAllocatedBytes(tid) - a0
      i += 1
    val sorted = times.sorted
    println(
      f"$name%-62s p50 ${sorted(reps / 2)}%8.3f ms  p95 ${sorted((reps * 0.95).toInt)}%8.3f ms  alloc ${alloc.toDouble / reps / 1e6}%8.2f MB/call"
    )

  private def filled(width: Int, height: Int): Surface =
    val surface = Surface.Companion.makeRasterN32Premul(width, height)
    val paint   = Paint()
    val random  = new scala.util.Random(1L)
    (0 until 400).foreach { _ =>
      paint.setColor(0xff000000 | random.nextInt(0xffffff))
      surface.getCanvas.drawRect(
        org.jetbrains.skia.Rect.Companion
          .makeXYWH(random.nextInt(width - 50).toFloat, random.nextInt(height - 50).toFloat, 50f, 50f),
        paint
      )
    }
    surface

  private def currentCopyInto(
    surface: Surface,
    image: BufferedImage,
    left: Int,
    top: Int,
    width: Int,
    height: Int
  ): Unit =
    val info   = ImageInfo.Companion.makeN32Premul(width, height)
    val bitmap = Bitmap()
    val read   = bitmap.allocPixels(info) && surface.readPixels(bitmap, left, top)
    val bytes  = if read then Option(bitmap.readPixels(info, width * 4, 0, 0)) else None
    bytes.foreach { raw =>
      val pixels = new Array[Int](width * height)
      ByteBuffer.wrap(raw).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer().get(pixels)
      val data = image.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
      var row  = 0
      while row < height do
        System.arraycopy(pixels, row * width, data, (top + row) * image.getWidth + left, width)
        row += 1
    }

  /** No `int[]` in between: the byte array is viewed as ints and each row is read straight into the image. */
  private def viewedCopyInto(
    surface: Surface,
    image: BufferedImage,
    left: Int,
    top: Int,
    width: Int,
    height: Int
  ): Unit =
    val info   = ImageInfo.Companion.makeN32Premul(width, height)
    val bitmap = Bitmap()
    val read   = bitmap.allocPixels(info) && surface.readPixels(bitmap, left, top)
    Option.when(read)(bitmap.readPixels(info, width * 4, 0, 0)).flatMap(Option(_)).foreach { raw =>
      val ints = ByteBuffer.wrap(raw).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer()
      val data = image.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
      var row  = 0
      while row < height do
        ints.get(row * width, data, (top + row) * image.getWidth + left, width)
        row += 1
    }

  /** The floor: straight from the surface's own memory into the image's array, through a direct buffer over it with no
    * intermediate at all. This is the route `RasterFrames.copyInto` takes.
    */
  private def mappedCopyInto(
    surface: Surface,
    pixmap: Pixmap,
    image: BufferedImage,
    left: Int,
    top: Int,
    width: Int,
    height: Int
  ): Unit =
    if surface.peekPixels(pixmap) then
      val data     = image.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
      val rowBytes = pixmap.getRowBytes
      val ints = BufferUtil.INSTANCE
        .getByteBufferFromPointer(pixmap.getAddr, rowBytes * surface.getHeight)
        .order(ByteOrder.LITTLE_ENDIAN)
        .asIntBuffer()
      var row = 0
      while row < height do
        ints.get((top + row) * (rowBytes / 4) + left, data, (top + row) * image.getWidth + left, width)
        row += 1

  /** Public API only: the surface's own memory is viewed as a `Data` and `rowsPerRead` rows are copied out at a time.
    */
  private def bufferedCopyInto(
    surface: Surface,
    pixmap: Pixmap,
    image: BufferedImage,
    left: Int,
    top: Int,
    width: Int,
    height: Int,
    rowsPerRead: Int
  ): Unit =
    if surface.peekPixels(pixmap) then
      val data     = image.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
      val rowBytes = pixmap.getRowBytes
      val buffer   = pixmap.getBuffer
      var row      = 0
      while row < height do
        val rows  = math.min(rowsPerRead, height - row)
        val raw   = buffer.getBytes((top + row) * rowBytes, rows * rowBytes)
        val ints  = ByteBuffer.wrap(raw).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer()
        var inner = 0
        while inner < rows do
          ints.get(inner * (rowBytes / 4) + left, data, (top + row + inner) * image.getWidth + left, width)
          inner += 1
        row += rows

  /** Draws lines of text on a raster surface for `seconds` seconds, for a native profiler to look at. */
  private def textLoop(seconds: Int, size: Float): Unit =
    val surface  = Surface.Companion.makeRasterN32Premul(3840, 2160)
    val canvas   = surface.getCanvas
    val typeface = FontMgr.Companion.getDefault().matchFamily("DejaVu Sans Mono").getTypeface(0)
    val font     = new SkFont(typeface, size)
    font.setSubpixel(true)
    font.setLinearMetrics(true)
    font.setHinting(FontHinting.NONE)
    font.setEdging(FontEdging.ANTI_ALIAS)
    val paint = Paint()
    paint.setAntiAlias(true)
    paint.setColor(0xffdddddd.toInt)
    val line     = sampleLine
    val deadline = System.nanoTime() + seconds * 1000000000L
    var calls    = 0L
    while System.nanoTime() < deadline do
      var n = 0
      while n < 20 do
        canvas.drawString(line, 4f + (calls % 4) * 0.25f, 40f + n * 30f, font, paint)
        n += 1
      calls += 20
    println(s"text loop: $calls lines of ${line.length} characters at $size px in $seconds s")

  def main(args: Array[String]): Unit =
    if args.headOption.contains("text-loop") then
      textLoop(args.lift(1).fold(20)(_.toInt), args.lift(2).fold(24f)(_.toFloat))
    else if args.headOption.contains("paint-variants") then paintVariants()
    else probe(args)

  /** What a paint can change about the mask blit Skia picks for anti-aliased text on a raster surface. */
  private def paintVariants(): Unit =
    val surface  = Surface.Companion.makeRasterN32Premul(3840, 2160)
    val canvas   = surface.getCanvas
    val typeface = FontMgr.Companion.getDefault().matchFamily("DejaVu Sans Mono").getTypeface(0)
    val font     = new SkFont(typeface, 24f)
    font.setSubpixel(true)
    font.setLinearMetrics(true)
    font.setHinting(FontHinting.NONE)
    font.setEdging(FontEdging.ANTI_ALIAS)
    val line = ("lorem ipsum dolor sit amet " * 4).take(100)
    val ink  = 0xffdddddd.toInt
    def paintOf(configure: Paint => Unit): Paint =
      val paint = Paint()
      paint.setAntiAlias(true)
      paint.setColor(ink)
      configure(paint)
      paint
    val variants = List(
      "opaque colour (what the app does)" -> paintOf(_ => ()),
      "alpha 254"                         -> paintOf(_.setAlpha(254)),
      "colour as a shader"                -> paintOf(_.setShader(org.jetbrains.skia.Shader.Companion.makeColor(ink))),
      "colour filter (blend with the colour)" -> paintOf(
        _.setColorFilter(org.jetbrains.skia.ColorFilter.Companion.makeBlend(ink, BlendMode.SRC_IN))
      ),
      "dither"                                    -> paintOf(_.setDither(true)),
      "blend mode SRC_ATOP"                       -> paintOf(_.setBlendMode(BlendMode.SRC_ATOP)),
      "blend mode SRC (replaces the destination)" -> paintOf(_.setBlendMode(BlendMode.SRC))
    )
    variants.foreach { (label, paint) =>
      measure(f"skia raster drawString x20 lines, $label", 40) {
        var n = 0
        while n < 20 do
          canvas.drawString(line, 4f, 40f + n * 30f, font, paint)
          n += 1
      }
    }

  /** The surface a probe copies out of, the image it copies into, and the pixmap that views the surface. */
  final private class Rig(val width: Int, val height: Int):
    val surface: Surface     = filled(width, height)
    val image: BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val pixmap: Pixmap       = new Pixmap()

  private def skiaFont(
    typeface: org.jetbrains.skia.Typeface,
    subpixel: Boolean,
    edging: FontEdging,
    hinting: FontHinting
  ): SkFont =
    val font = new SkFont(typeface, 24f)
    font.setSubpixel(subpixel)
    font.setLinearMetrics(true)
    font.setHinting(hinting)
    font.setEdging(edging)
    font

  private val sampleLine = ("lorem ipsum dolor sit amet " * 4).take(100)

  private def probe(args: Array[String]): Unit =
    val rig = new Rig(args.headOption.map(_.toInt).getOrElse(3840), args.lift(1).map(_.toInt).getOrElse(2160))
    println(s"surface ${rig.width}x${rig.height} (${rig.width.toLong * rig.height * 4 / 1000000} MB)")
    copyOutProbe(rig)
    reusedBufferProbe(rig)
    scrollProbe(rig)
    textProbe(rig)
    java2dTextProbe(rig)
    rectProbe(rig)

  private def copyOutProbe(rig: Rig): Unit =
    import rig.{height, image, pixmap, surface, width}
    println("\n-- copy-out of one area into the image")
    val areas = List(
      "whole frame"                    -> (0, 0, width, height),
      "scroll area (all but 160 rows)" -> (0, 80, width, height - 160),
      "typing: 2 rows x 40 px"         -> (0, 1000, width, 80),
      "typing: 1 row, 40 px"           -> (0, 1000, width, 40)
    )
    areas.foreach {
      case (label, (l, t, w, h)) =>
        measure(s"current   copyInto   $label", 40)(currentCopyInto(surface, image, l, t, w, h))
        measure(s"viewed    (no int[]) $label", 40)(viewedCopyInto(surface, image, l, t, w, h))
        List(1, 16, 128).foreach(n =>
          measure(s"buffered  (Pixmap.buffer, $n rows/read) $label", 40)(
            bufferedCopyInto(surface, pixmap, image, l, t, w, h, n)
          )
        )
        measure(s"mapped    (BufferUtil) $label", 40)(mappedCopyInto(surface, pixmap, image, l, t, w, h))
    }

  private def reusedBufferProbe(rig: Rig): Unit =
    import rig.{height, image, surface, width}
    println("\n-- copy-out of the whole frame without a fresh array per chunk (can a buffer be reused?)")
    val rowBytesOfSurface = width * 4
    val rowsPerChunk      = 16
    val chunkInfo         = ImageInfo.Companion.makeN32Premul(width, rowsPerChunk)
    val chunkBitmap       = Bitmap()
    chunkBitmap.allocPixels(chunkInfo)
    val chunkBytes = new Array[Byte](rowsPerChunk * rowBytesOfSurface)
    measure("pooled   Surface.readPixels(kept Bitmap) + readPixels$skiko(kept byte[])", 40) {
      val data = image.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
      var row  = 0
      while row < height do
        val rows = math.min(rowsPerChunk, height - row)
        val info = if rows == rowsPerChunk then chunkInfo else ImageInfo.Companion.makeN32Premul(width, rows)
        val dst =
          if rows == rowsPerChunk then chunkBitmap
          else
            val b = Bitmap(); b.allocPixels(info); b
        surface.readPixels(dst, 0, row)
        dst.`readPixels$skiko`(chunkBytes, info, rowBytesOfSurface, 0, 0)
        val ints = ByteBuffer.wrap(chunkBytes).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer()
        ints.get(0, data, row * width, rows * width)
        row += rows
    }
    val borrowed    = new Array[Byte](rowsPerChunk * rowBytesOfSurface)
    val borrowedMap = Bitmap()
    borrowedMap.installPixels(chunkInfo, borrowed, rowBytesOfSurface)
    java.util.Arrays.fill(borrowed, 0.toByte)
    surface.readPixels(borrowedMap, 0, 40)
    println(
      s"installPixels(byte[]) wraps without copying: ${borrowed.exists(_ != 0)} (surface.readPixels wrote into the Java array)"
    )

  private def scrollProbe(rig: Rig): Unit =
    import rig.{height, image, pixmap, surface, width}
    println("\n-- scroll: move the scroll area up by 6 device rows inside the surface")
    val (sl, st, sw, sh) = (0, 80, width, height - 160)
    val ink              = Paint()
    ink.setBlendMode(BlendMode.SRC)
    ink.setColor(0xff000000)
    measure("current   makeImageSnapshot(subset) + drawImage(SRC)", 40) {
      val snap = surface.makeImageSnapshot(IRect.Companion.makeXYWH(sl, st, sw, sh))
      surface.getCanvas.drawImage(snap, sl.toFloat, (st - 6).toFloat, ink)
      snap.close()
    }
    val scratch = Bitmap()
    scratch.allocPixels(ImageInfo.Companion.makeN32Premul(sw, sh))

    /** In place, public API only: bands of the scroll area copied inside the surface's own memory, in an order that
      * never reads a row already overwritten.
      */
    def moveInPlace(left: Int, top: Int, w: Int, h: Int, dy: Int): Unit =
      if surface.peekPixels(pixmap) then
        val bandRows = 256
        val starts   = if dy < 0 then (0 until h by bandRows).toList else (0 until h by bandRows).toList.reverse
        starts.foreach { start =>
          val rows   = math.min(bandRows, h - start)
          val source = new Pixmap()
          val target = new Pixmap()
          try
            if pixmap.extractSubset(source, IRect.Companion.makeXYWH(left, top + start, w, rows)) &&
                pixmap.extractSubset(target, IRect.Companion.makeXYWH(left, top + start + dy, w, rows))
            then source.readPixels(target, 0, 0)
          finally
            source.close()
            target.close()
        }
    measure("in place  Pixmap.extractSubset + readPixels, 256-row bands", 40)(moveInPlace(sl, st, sw, sh, -6))
    measure("kept      readPixels(kept bitmap) + writePixels", 40) {
      surface.readPixels(scratch, sl, st)
      surface.writePixels(scratch, sl, st - 6)
    }
    measure("java2d    System.arraycopy rows inside the int[]", 40) {
      val data = image.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
      System.arraycopy(data, st * width, data, (st - 6) * width, sh * width)
    }

  private def textProbe(rig: Rig): Unit =
    import rig.surface
    println("\n-- text: one 100-character line, 2x, same string")
    val line     = sampleLine
    val typeface = FontMgr.Companion.getDefault().matchFamily("DejaVu Sans Mono").getTypeface(0)
    def skFont(subpixel: Boolean, edging: FontEdging, hinting: FontHinting): SkFont =
      skiaFont(typeface, subpixel, edging, hinting)
    val glyphPaint = Paint()
    glyphPaint.setAntiAlias(true)
    glyphPaint.setColor(0xffdddddd.toInt)
    val canvas = surface.getCanvas
    List(
      "app settings (subpixel, ANTI_ALIAS, no hinting)" -> skFont(true, FontEdging.ANTI_ALIAS, FontHinting.NONE),
      "no subpixel positioning"                         -> skFont(false, FontEdging.ANTI_ALIAS, FontHinting.NONE),
      "no antialiasing (ALIAS)"                         -> skFont(true, FontEdging.ALIAS, FontHinting.NONE)
    ).foreach {
      case (label, font) =>
        var x = 0f
        measure(s"skia raster drawString x20 lines, $label", 40) {
          var n = 0
          while n < 20 do
            x = (x + 0.37f) % 7f
            canvas.drawString(line, x, 40f + n * 30f, font, glyphPaint)
            n += 1
        }
    }
    val plain = Paint()
    plain.setAntiAlias(false)
    plain.setColor(0xffdddddd.toInt)
    val appFont = skFont(true, FontEdging.ANTI_ALIAS, FontHinting.NONE)
    measure("skia raster drawString x20, paint AA off (font AA on)", 40) {
      var n = 0
      while n < 20 do
        canvas.drawString(line, 3.3f, 40f + n * 30f, appFont, plain)
        n += 1
    }
    val opaque = Paint()
    opaque.setAntiAlias(true)
    opaque.setBlendMode(BlendMode.SRC)
    opaque.setColor(0xffdddddd.toInt)
    measure("skia raster drawString x20, blend SRC", 40) {
      var n = 0
      while n < 20 do
        canvas.drawString(line, 3.3f, 40f + n * 30f, appFont, opaque)
        n += 1
    }
    val mono = new SkFont(typeface, 24f)
    mono.setHinting(FontHinting.NONE)
    mono.setEdging(FontEdging.ANTI_ALIAS)
    measure("skia raster drawString x20, defaults (no subpixel, no linear metrics)", 40) {
      var n = 0
      while n < 20 do
        canvas.drawString(line, 3.3f, 40f + n * 30f, mono, glyphPaint)
        n += 1
    }
    measure("skia raster drawString x20, same x every call (cache-friendly)", 40) {
      var n = 0
      while n < 20 do
        canvas.drawString(line, 4.0f, 40f + n * 30f, appFont, glyphPaint)
        n += 1
    }
    val small = skFont(true, FontEdging.ANTI_ALIAS, FontHinting.NONE)
    small.setSize(12f)
    measure("skia raster drawString x20, 12 px font (half size)", 40) {
      var n = 0
      while n < 20 do
        canvas.drawString(line, 4.0f, 40f + n * 30f, small, glyphPaint)
        n += 1
    }
    val lcd = skFont(true, FontEdging.SUBPIXEL_ANTI_ALIAS, FontHinting.NONE)
    measure("skia raster drawString x20, SUBPIXEL_ANTI_ALIAS (LCD)", 40) {
      var n = 0
      while n < 20 do
        canvas.drawString(line, 4.0f, 40f + n * 30f, lcd, glyphPaint)
        n += 1
    }

  private def java2dTextProbe(rig: Rig): Unit =
    import rig.image
    val line = sampleLine
    val g    = image.createGraphics()
    g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
    g.setFont(new AwtFont("DejaVu Sans Mono", AwtFont.PLAIN, 24))
    g.setColor(new Color(0xdddddd))
    var ax = 0f
    measure("java2d    drawString x20 lines (grayscale AA, fractional metrics)", 40) {
      var n = 0
      while n < 20 do
        ax = (ax + 0.37f) % 7f
        g.drawString(line, ax, 40f + n * 30f)
        n += 1
    }
    g.dispose()

  private def rectProbe(rig: Rig): Unit =
    import rig.{image, surface}
    val canvas = surface.getCanvas
    val g      = image.createGraphics()
    println("\n-- 1 px high rect (misspelling squiggle segments), x500")
    val rectPaint = Paint()
    rectPaint.setAntiAlias(false)
    rectPaint.setColor(0xffcc3333.toInt)
    measure("skia raster drawRect 8x1 x500", 40) {
      var n = 0
      while n < 500 do
        canvas.drawRect(
          org.jetbrains.skia.Rect.Companion.makeXYWH((n * 8 % 3000).toFloat, 1500f + (n % 7), 8f, 1f),
          rectPaint
        )
        n += 1
    }
    g.setColor(new Color(0xcc3333))
    measure("java2d    fillRect 8x1 x500", 40) {
      var n = 0
      while n < 500 do
        g.fillRect(n * 8 % 3000, 1500 + (n % 7), 8, 1)
        n += 1
    }
    g.dispose()
