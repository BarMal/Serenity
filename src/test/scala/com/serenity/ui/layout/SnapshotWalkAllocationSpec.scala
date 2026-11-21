package com.serenity.ui.layout

import java.awt.Font
import java.lang.management.ManagementFactory

import com.serenity.rope.Balance
import com.serenity.state.models.{Buffer, BufferId, Viewport}
import com.serenity.testkit.TestMeasure
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A snapshot walks the document's lines until the viewport's rows are full, so the text it reads from the rope is the
  * text of the lines it uses, not of every line the viewport could have held. Measured on the wrap cache's hit path,
  * where the walk is nearly all that is left (#1908).
  */
class SnapshotWalkAllocationSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bean = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  private val font = Font(Font.MONOSPACED, Font.PLAIN, 14)
  private val words =
    Vector("alpha", "bravo", "charlie", "delta", "echo", "foxtrot", "golf", "hotel", "india", "juliet")

  private def paragraph(index: Int, chars: Int): String =
    val cycle = words.indices.map(step => words((index + 7 * step) % words.length)).mkString(" ")
    Iterator.continually(cycle).take(chars / cycle.length + 1).mkString(" ").take(chars)

  private def paragraphs(count: Int, chars: Int): String = (0 until count).map(paragraph(_, chars)).mkString("\n")

  private def bytesPerBuild(text: String, visibleLines: Int): (Long, TextLayoutSnapshot) =
    val buffer = Buffer
      .fromString(BufferId(1), text)
      .copy(viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = visibleLines, visibleColumns = 100))
    val cache    = TestMeasure.wrapCache()
    def build()  = TextLayoutSnapshot.fromBuffer(buffer, 900, font, wordWrapEnabled = true, wrapCache = cache)
    val snapshot = build()
    (1 to 2000).foreach(_ => build())
    val builds = 300
    val before = bean.getThreadAllocatedBytes(Thread.currentThread.threadId)
    (1 to builds).foreach(_ => build())
    ((bean.getThreadAllocatedBytes(Thread.currentThread.threadId) - before) / builds, snapshot)

  "A snapshot of forty rows that the first of forty long lines fills" should "read only that line from the rope" in {
    val (bytes, snapshot) = bytesPerBuild(paragraphs(40, 20000), 40)
    snapshot.visualLines.map(_.bufferLine).distinct shouldBe Vector(0)
    info(s"$bytes bytes a build")
    withClue(s"$bytes bytes a build: ")(bytes should be < 400000L)
  }

  "A snapshot of forty rows of 550-character paragraphs" should "read only the paragraphs those rows come from" in {
    val (bytes, snapshot) = bytesPerBuild(paragraphs(800, 550), 40)
    snapshot.visualLines.size shouldBe 40
    info(s"$bytes bytes a build")
    withClue(s"$bytes bytes a build: ")(bytes should be < 60000L)
  }
