package com.serenity.perf

import java.awt.Font
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.{Callable, Executors, TimeUnit}

import scala.jdk.CollectionConverters.*

import com.serenity.testkit.TestMeasure
import com.serenity.ui.layout.TextLayoutSnapshot
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** How layout scales across threads that share one owner's measurer, as the compute pool's do: N threads wrap distinct
  * Inter paragraphs through one wrap cache, and the throughput of each N is written next to one thread's to
  * `target/test-reports/layout-contention/summary.tsv`. Every thread must wrap a paragraph to the rows one thread
  * alone wraps it to; the timings are information, nothing fails on them.
  */
class LayoutContentionSpec extends AnyFlatSpec with Matchers:

  private val ThreadCounts = Vector(1, 2, 4, 8)

  private val ParagraphsPerThread = 300

  private val PanelWidthPx = 520

  private lazy val inter: Font =
    val stream = getClass.getResourceAsStream("/fonts/Inter-Regular-Subset.ttf")
    try Font.createFont(Font.TRUETYPE_FONT, stream).deriveFont(14f)
    finally stream.close()

  private def paragraph(run: Int, index: Int): String =
    s"Run $run paragraph $index: " + BenchmarkFixtures.longParagraph(400 + index % 9 * 37)

  private def rowsOf(text: String, cache: com.serenity.ui.layout.WrappedLineCache): Vector[(Int, Int)] =
    TextLayoutSnapshot
      .boundedVisualLinesForText(text, 0, PanelWidthPx, inter, wrapCache = cache)
      .map(line => (line.startColumn, line.endColumn))

  final private case class Timed(threads: Int, paragraphs: Int, seconds: Double):
    def perSecond: Double = paragraphs / seconds

  private def wrapAll(run: Int, threads: Int): (Timed, Map[String, Vector[(Int, Int)]]) =
    val cache = TestMeasure.wrapCache()
    val pool  = Executors.newFixedThreadPool(threads)
    try
      val texts = Vector.tabulate(threads, ParagraphsPerThread)((thread, index) =>
        paragraph(run, thread * ParagraphsPerThread + index)
      )
      val tasks = texts.map(own =>
        new Callable[Vector[(String, Vector[(Int, Int)])]]:
          def call(): Vector[(String, Vector[(Int, Int)])] = own.map(text => text -> rowsOf(text, cache))
      )
      val started = System.nanoTime()
      val results = pool.invokeAll(tasks.asJava).asScala.toVector.flatMap(_.get())
      val seconds = (System.nanoTime() - started) / 1e9
      (Timed(threads, results.length, seconds), results.toMap)
    finally
      pool.shutdown()
      val _ = pool.awaitTermination(1, TimeUnit.MINUTES)

  "Layout on several threads sharing one measurer" should "wrap every paragraph as one thread does, and report its throughput" in {
    ThreadCounts.foreach(threads => wrapAll(run = 100 + threads, threads))
    val measured = ThreadCounts.zipWithIndex.map { (threads, index) =>
      val (timed, rows) = wrapAll(run = index + 1, threads)
      val alone         = TestMeasure.wrapCache()
      rows.foreach((text, wrapped) => withClue(text.take(40))(wrapped shouldBe rowsOf(text, alone)))
      timed
    }
    val single = measured.headOption.map(_.perSecond).getOrElse(1.0)
    val table =
      "threads\tparagraphs\tseconds\tparagraphsPerSecond\tspeedupOverOneThread\n" + measured
        .map(t => f"${t.threads}\t${t.paragraphs}\t${t.seconds}%.3f\t${t.perSecond}%.1f\t${t.perSecond / single}%.2f")
        .mkString("", "\n", "\n")
    val directory = Paths.get("target", "test-reports", "layout-contention")
    val _         = Files.createDirectories(directory)
    val _         = Files.write(directory.resolve("summary.tsv"), table.getBytes(StandardCharsets.UTF_8))
    measured.map(_.threads) shouldBe ThreadCounts
  }
