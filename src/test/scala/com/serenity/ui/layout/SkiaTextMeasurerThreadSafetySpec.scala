package com.serenity.ui.layout

import java.lang.management.ManagementFactory
import java.util.concurrent.{Callable, CountDownLatch, Executors}

import com.serenity.ui.layout.TextMeasurer.LineFonts
import com.serenity.ui.renderer.FontSpec
import com.serenity.ui.skia.{SkiaFontCache, SkiaTextMeasurer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A Skia font's first use, and a shaper's state, are not documented as safe across threads. Threads that all start
  * cold on the same fonts and text must measure what one thread does.
  */
class SkiaTextMeasurerThreadSafetySpec extends AnyFlatSpec with Matchers:

  private val threads = 4

  private def measure(measurer: SkiaTextMeasurer): Vector[Vector[Int]] =
    for
      pinned <- TextMeasureCorpus.fonts
      line   <- TextMeasureCorpus.lines
    yield
      val spec = FontSpec.fromAwt(pinned.font)
      val xs   = measurer.rawCaretXs(line.text, 0, LineFonts(spec, Vector.empty))
      val cell = measurer.cellMetrics(spec)
      xs.toVector.map(java.lang.Float.floatToRawIntBits) :+ cell.charWidth :+ cell.lineHeight :+ cell.ascent

  "Threads measuring from cold" should "get the numbers a single thread gets" in {
    val reference = measure(new SkiaTextMeasurer(new SkiaFontCache()))
    val pool      = Executors.newFixedThreadPool(threads)
    try
      val shared = new SkiaTextMeasurer(new SkiaFontCache())
      val start  = new CountDownLatch(1)
      val runs = (1 to threads).map { _ =>
        pool.submit(
          new Callable[Vector[Vector[Int]]]:
            def call(): Vector[Vector[Int]] =
              start.await()
              measure(shared)
        )
      }
      start.countDown()
      runs.foreach(run => run.get() shouldBe reference)
    finally pool.shutdown()
  }

  /** Layout counts the rows of several panes on as many threads at once, and each count shapes a paragraph at a time:
    * if shaping goes through one lock the threads take turns, and counting four panes takes as long as counting them
    * one after the other. Time spent blocked on a monitor is what taking turns is, whatever else the machine is doing.
    */
  it should "not block threads that shape distinct paragraphs on one another" in {
    val bean = ManagementFactory.getThreadMXBean
    assume(bean.isThreadContentionMonitoringSupported, "needs thread contention monitoring")
    bean.setThreadContentionMonitoringEnabled(true)
    val fonts     = LineFonts(FontSpec.fromAwt(TextMeasureCorpus.fonts.head.font), Vector.empty)
    val perThread = 150
    val measurer  = new SkiaTextMeasurer(new SkiaFontCache())

    def paragraph(thread: Int, index: Int): String =
      val random = new scala.util.Random(thread * 1_009L + index)
      Vector
        .fill(90)(random.alphanumeric.take(3 + random.nextInt(7)).mkString.toLowerCase)
        .mkString(s"$thread.$index ", " ", ".")

    val _    = measurer.exactCaretXs(paragraph(-1, 0), 0, fonts)
    val pool = Executors.newFixedThreadPool(threads)
    try
      val start = new CountDownLatch(1)
      val tasks = (0 until threads).map { thread =>
        val own = Vector.tabulate(perThread)(index => paragraph(thread, index))
        pool.submit(
          new Callable[Long]:
            def call(): Long =
              start.await()
              own.foreach(text => measurer.exactCaretXs(text, 0, fonts))
              Thread.currentThread.threadId
        )
      }
      val started = System.nanoTime()
      start.countDown()
      val ids       = tasks.map(task => task.get()).toArray
      val wallMs    = (System.nanoTime() - started) / 1_000_000L
      val blockedMs = bean.getThreadInfo(ids).map(info => math.max(info.getBlockedTime, 0L)).sum

      withClue(s"blocked $blockedMs ms across $threads threads in $wallMs ms: ")(
        blockedMs should be <= wallMs * threads / 5
      )
    finally pool.shutdown()
  }
