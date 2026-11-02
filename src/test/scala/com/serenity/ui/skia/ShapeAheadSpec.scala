package com.serenity.ui.skia

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

import scala.concurrent.duration.*
import scala.util.Random

import org.jetbrains.skia.{EncodedImageFormat, Font, Paint, Surface}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Shaping text before a frame asks for it (#2465, warm-text step 1). The cache holds a pure function of (text, font),
  * so what a worker shapes must be what the frame would have shaped itself; and the first gate of the work is that
  * several threads shaping at once, with a frame drawing from the same cache, get the same glyphs as one thread does.
  */
final class ShapeAheadSpec extends AnyFlatSpec with Matchers:

  private val awt = new java.awt.Font(java.awt.Font.SERIF, java.awt.Font.PLAIN, 14)

  /** Latin prose, box drawing and arrows (shaped in our own font runs), accents, scripts that need the shaper's own
    * itemisation (the `TextLine` path), and a combining mark.
    */
  private val Lines: Vector[String] =
    Vector(
      "The quick brown fox jumps over the lazy dog.",
      "A second line, longer than the first, with some punctuation; and a number: 4,096.",
      "│ ├── src/main/scala",
      "└── ui → skia ← renderer",
      "café résumé naïve",
      "Ελληνικά και Русский текст",
      "日本語の文章です。",
      "é combined",
      "  indented with spaces",
      "tab\tseparated\tcolumns"
    )

  private def font(caches: SkiaTextCaches): Font = caches.fonts.faceFor(awt).font

  private def edges(stretch: SkiaShapedStretch, length: Int): Vector[Option[Float]] =
    (-1 to length + 1).map(stretch.leftEdgeOf).toVector

  private def pixels(stretch: SkiaShapedStretch, length: Int): Vector[Byte] =
    val surface = Surface.Companion.makeRasterN32Premul(600, 44)
    val canvas  = surface.getCanvas
    val _       = canvas.clear(0xffffffff)
    val paint   = new Paint()
    paint.setColor(0xff000000)
    stretch.draw(canvas, 0, length, 12f, 30f, paint)
    surface.makeImageSnapshot().encodeToData(EncodedImageFormat.PNG, 100, 0).getBytes.toVector

  "ShapeAhead" should "leave the cache answering from what its workers shaped, with nothing shaped on the draw path" in {
    val caches = SkiaTextCaches.fresh()
    val ahead  = new ShapeAhead(caches.shapedText, workers = 2)
    try
      ahead.request(Lines.map(_ -> font(caches)))
      ahead.awaitIdle(10.seconds) shouldBe true

      Lines.foreach(line => caches.shapedText.stretchOf(line, font(caches)))

      caches.shapedText.shapings shouldBe 0L
      caches.shapedText.aheadShapings shouldBe Lines.distinct.size.toLong
      ahead.failures shouldBe 0L
    finally ahead.close()
  }

  it should "give the edges and the pixels a frame's own shaping gives" in {
    val caches = SkiaTextCaches.fresh()
    val inline = SkiaTextCaches.fresh()
    val ahead  = new ShapeAhead(caches.shapedText, workers = 2)
    try
      ahead.request(Lines.map(_ -> font(caches)))
      ahead.awaitIdle(10.seconds) shouldBe true

      Lines.foreach { line =>
        withClue(line) {
          val warmed    = caches.shapedText.stretchOf(line, font(caches))
          val reference = inline.shapedText.stretchOf(line, font(inline))
          edges(warmed, line.length) shouldBe edges(reference, line.length)
          pixels(warmed, line.length) shouldBe pixels(reference, line.length)
        }
      }
    finally ahead.close()
  }

  it should "skip a stretch the cache already holds" in {
    val caches = SkiaTextCaches.fresh()
    val ahead  = new ShapeAhead(caches.shapedText, workers = 2)
    try
      Lines.foreach(line => caches.shapedText.stretchOf(line, font(caches)))
      val shapedByFrame = caches.shapedText.shapings

      ahead.request(Lines.map(_ -> font(caches)))
      ahead.awaitIdle(10.seconds) shouldBe true

      caches.shapedText.aheadShapings shouldBe 0L
      caches.shapedText.shapings shouldBe shapedByFrame
    finally ahead.close()
  }

  it should "shape each stretch once however many times, and by however many workers, it is asked for" in {
    val caches = SkiaTextCaches.fresh()
    val ahead  = new ShapeAhead(caches.shapedText, workers = 3)
    try
      val requests = Vector.fill(20)(Lines.map(_ -> font(caches))).flatten
      ahead.request(requests)
      ahead.awaitIdle(10.seconds) shouldBe true

      caches.shapedText.aheadShapings should be <= Lines.distinct.size.toLong + 3L
      caches.shapedText.aheadShapings should be >= Lines.distinct.size.toLong
    finally ahead.close()
  }

  it should "stop starting work once it is cancelled" in {
    val caches = SkiaTextCaches.fresh()
    val ahead  = new ShapeAhead(caches.shapedText, workers = 1)
    try
      val many = (0 until 4000).map(i => s"line number $i of a long run, with some text after it" -> font(caches))
      ahead.request(many)
      ahead.cancel()
      ahead.awaitIdle(10.seconds) shouldBe true

      val shaped = caches.shapedText.aheadShapings
      shaped should be < many.size.toLong
      ahead.awaitIdle(1.second) shouldBe true
      caches.shapedText.aheadShapings shouldBe shaped
    finally ahead.close()
  }

  it should "take requests again after a cancel" in {
    val caches = SkiaTextCaches.fresh()
    val ahead  = new ShapeAhead(caches.shapedText, workers = 2)
    try
      ahead.request(Lines.map(_ -> font(caches)))
      ahead.cancel()
      ahead.awaitIdle(10.seconds) shouldBe true
      val after = Lines.map(line => s"$line (again)" -> font(caches))

      ahead.request(after)
      ahead.awaitIdle(10.seconds) shouldBe true

      after.foreach((text, f) => caches.shapedText.stretchOf(text, f))
      caches.shapedText.shapings shouldBe 0L
    finally ahead.close()
  }

  it should "end its threads when it is closed" in {
    val caches = SkiaTextCaches.fresh()
    val ahead  = new ShapeAhead(caches.shapedText, workers = 2)
    ahead.request(Lines.map(_ -> font(caches)))
    ahead.awaitIdle(10.seconds) shouldBe true

    ahead.close()

    ahead.threadsAlive shouldBe 0
  }

  it should "shape nothing for a request made after it was closed" in {
    val caches = SkiaTextCaches.fresh()
    val ahead  = new ShapeAhead(caches.shapedText, workers = 1)
    ahead.close()

    ahead.request(Lines.map(_ -> font(caches)))

    caches.shapedText.aheadShapings shouldBe 0L
  }

  // The first gate of the work (#2465): if this fails, shaping on several threads stops here.
  "Several threads shaping into one cache, with frames drawing from it" should "get the glyphs one thread gets" in {
    val reference = SkiaTextCaches.fresh()
    val expected: Map[String, (Vector[Option[Float]], Vector[Byte])] =
      Lines.map { line =>
        val stretch = reference.shapedText.stretchOf(line, font(reference))
        line -> (edges(stretch, line.length), pixels(stretch, line.length))
      }.toMap

    val failures      = new AtomicReference(List.empty[String])
    val rounds        = new AtomicInteger(0)
    val aheadFailures = new java.util.concurrent.atomic.AtomicLong(0L)
    (0 until 6).foreach { round =>
      val caches  = SkiaTextCaches.fresh()
      val ahead   = new ShapeAhead(caches.shapedText, workers = 3)
      val drawers = Executors.newFixedThreadPool(4)
      try
        val done     = new CountDownLatch(4)
        val shuffled = new Random(round)
        ahead.request(shuffled.shuffle(Vector.fill(8)(Lines).flatten).map(_ -> font(caches)))
        (0 until 4).foreach { drawer =>
          val _ = drawers.submit(
            new Runnable:
              def run(): Unit =
                try
                  val random = new Random(round * 10 + drawer)
                  (0 until 60).foreach { _ =>
                    val line                            = Lines(random.nextInt(Lines.size))
                    val stretch                         = caches.shapedText.stretchOf(line, font(caches))
                    val (expectedEdges, expectedPixels) = expected(line)
                    if edges(stretch, line.length) != expectedEdges || pixels(stretch, line.length) != expectedPixels
                    then failures.updateAndGet(line :: _)
                  }
                catch
                  case error: Throwable =>
                    failures.updateAndGet(s"${error.getClass.getSimpleName}: ${error.getMessage}" :: _)
                finally done.countDown()
          )
        }
        done.await(120, TimeUnit.SECONDS) shouldBe true
        ahead.awaitIdle(30.seconds) shouldBe true
        val _ = aheadFailures.addAndGet(ahead.failures)
        val _ = rounds.incrementAndGet()
      finally
        ahead.close()
        drawers.shutdownNow()
    }

    failures.get shouldBe empty
    rounds.get shouldBe 6
    aheadFailures.get shouldBe 0L
  }

  "A Skia surface's text capability" should "shape ahead into the cache the surface draws with" in {
    val metrics = com.serenity.ui.layout.CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
    val caches  = SkiaTextCaches.fresh()
    val target  = Surface.Companion.makeRasterN32Premul(210, 130)
    val surface = new SkiaRenderSurface(
      target.getCanvas,
      metrics,
      com.serenity.ui.renderer.FontSpec.fromAwt(awt),
      () => (),
      210,
      130,
      caches = caches,
      styledFonts = com.serenity.ui.renderer.StyledFontCache()
    )
    try
      surface.text.shapeAhead(Lines.map(com.serenity.ui.renderer.ShapeAheadRequest(_, awt)))
      caches.ahead.awaitIdle(10.seconds) shouldBe true

      caches.shapedText.aheadShapings shouldBe Lines.distinct.size.toLong
      caches.shapedText.shapings shouldBe 0L
    finally caches.ahead.close()
  }

  it should "drop what has not started when it is cancelled" in {
    val metrics = com.serenity.ui.layout.CellMetrics(charWidth = 7, lineHeight = 13, ascent = 10)
    val caches  = SkiaTextCaches.fresh()
    val target  = Surface.Companion.makeRasterN32Premul(210, 130)
    val surface = new SkiaRenderSurface(
      target.getCanvas,
      metrics,
      com.serenity.ui.renderer.FontSpec.fromAwt(awt),
      () => (),
      210,
      130,
      caches = caches,
      styledFonts = com.serenity.ui.renderer.StyledFontCache()
    )
    try
      surface.text.shapeAhead(
        (0 until 4000).map(i => com.serenity.ui.renderer.ShapeAheadRequest(s"run $i of text", awt))
      )
      surface.text.cancelShapeAhead()
      caches.ahead.awaitIdle(10.seconds) shouldBe true

      caches.shapedText.aheadShapings should be < 4000L
    finally caches.ahead.close()
  }

  "A surface that does not shape ahead" should "ignore the request, by the capability's own default" in {
    val surface = new com.serenity.perf.NullRenderSurface(40, 20, 1.0, () => ())
    val request = com.serenity.ui.renderer.ShapeAheadRequest("anything", new java.awt.Font("Serif", 0, 12))

    noException should be thrownBy {
      surface.shapeAhead(Seq(request))
      surface.cancelShapeAhead()
    }
  }
