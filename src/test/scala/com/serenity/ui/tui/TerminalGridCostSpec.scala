package com.serenity.ui.tui

import java.awt.Color
import java.io.StringWriter
import java.lang.management.ManagementFactory

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1890: what a TUI frame costs must follow what changed, not the size of the screen. Allocation is measured on the
  * calling thread, so a flush that snapshots the grid, a clear that builds one object per cell, or any other
  * per-screen-cell work shows up as bytes, independent of timing noise.
  */
class TerminalGridCostSpec extends AnyFlatSpec with Matchers:

  private val white = RenderColor.fromAwt(Color.WHITE)
  private val black = RenderColor.fromAwt(Color.BLACK)
  private val grey  = RenderColor.fromAwt(Color.GRAY)

  /** Far above what a typing frame needs and far below one object per screen cell (12,000 cells at 200x60). */
  private val TypingFrameBudgetBytes = 48L * 1024

  private val allocationCounter: com.sun.management.ThreadMXBean =
    ManagementFactory.getThreadMXBean match
      case counter: com.sun.management.ThreadMXBean => counter
      case other => fail(s"thread allocation counters unavailable on ${other.getClass}")

  private def allocatedBy(work: => Unit): Long =
    val thread = Thread.currentThread().threadId()
    val before = allocationCounter.getThreadAllocatedBytes(thread)
    work
    allocationCounter.getThreadAllocatedBytes(thread) - before

  /** The cheapest of several runs, so a one-off JIT or GC-assist allocation does not decide the result. */
  private def steadyAllocation(work: Int => Unit): Long = (0 until 12).map(i => allocatedBy(work(i))).min

  final private class Screen(val width: Int, val height: Int):
    val writer = new StringWriter()
    val surface =
      new TerminalRenderSurface(width, height, writer, CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0))

    def paintRow(y: Int, seed: Int): Unit =
      surface.setForegroundColor(if y % 2 == 0 then white else grey)
      surface.setBackgroundColor(black)
      surface.putString(0, y, rowText(y, seed))

    def paintAll(): Unit =
      (0 until height).foreach(paintRow(_, 0))
      surface.flush()
      writer.getBuffer.setLength(0)

    def everyRowExcept(row: Int): List[PixelRect] =
      (0 until height).filter(_ != row).map(y => PixelRect(0, y, width, 1)).toList

    /** What the renderer does for one typed character: keep every other row, repaint the edited one. */
    def typeCharacter(row: Int, seed: Int, text: String): Unit =
      surface.clearViewportExcept(black, everyRowExcept(row))
      surface.setForegroundColor(if row % 2 == 0 then white else grey)
      surface.setBackgroundColor(black)
      surface.putString(0, row, text)
      surface.flush()
      writer.getBuffer.setLength(0)

  /** The same text for a given row whatever the seed, except for the one character a keystroke would change. */
  private def rowText(y: Int, seed: Int): String =
    (0 until 200).map(x => if x == 10 then ('a' + seed % 26).toChar else ('a' + (x * 7 + y * 3) % 26).toChar).mkString

  private def typingFrameBytes(width: Int, height: Int): Long =
    val screen = new Screen(width, height)
    screen.paintAll()
    val texts = (0 until 12).map(i => rowText(30, i + 1).take(width))
    steadyAllocation(i => screen.typeCharacter(30, i + 1, texts(i)))

  "a typing frame at 200x60" should "allocate far less than one object per screen cell" in {
    typingFrameBytes(200, 60) should be < TypingFrameBudgetBytes
  }

  "a typing frame" should "cost the same on a screen four times the size" in {
    val small = typingFrameBytes(200, 60)
    val large = typingFrameBytes(400, 120)
    large should be < (small * 2 + 4096)
  }

  "a flush with nothing changed" should "allocate a few kilobytes at most, whatever the screen size" in {
    val screen = new Screen(200, 60)
    screen.paintAll()
    val idle = steadyAllocation(_ => screen.surface.flush())
    idle should be < 4096L
  }

  "clearViewportExcept with a rect per reusable row" should "not pay per screen cell" in {
    val screen = new Screen(200, 60)
    screen.paintAll()
    val preserved = screen.everyRowExcept(30)
    val clear     = steadyAllocation(_ => screen.surface.clearViewportExcept(black, preserved))
    clear should be < 32L * 1024
  }

  it should "blank exactly the cells no preserved rect covers, including overlapping and partly off-screen rects" in {
    val screen = new Screen(12, 4)
    (0 until 4).foreach(screen.paintRow(_, 0))
    screen.surface.flush()

    val preserved = List(PixelRect(1, 0, 3, 2), PixelRect(2, 1, 4, 2), PixelRect(10, 3, 9, 3), PixelRect(-3, 3, 5, 1))
    screen.surface.clearViewportExcept(black, preserved)
    screen.surface.flush()

    val emulator = TerminalEmulator.blank(12, 4).consume(screen.writer.toString)
    val expected = for
      y <- 0 until 4
      x <- 0 until 12
      kept = preserved.exists(r => x >= r.xPx && x < r.rightPx && y >= r.yPx && y < r.bottomPx)
    yield (x, y, kept)
    expected.foreach { (x, y, kept) =>
      if kept then emulator.cellAt(x, y).text shouldBe rowText(y, 0)(x).toString
      else emulator.cellAt(x, y).text shouldBe " "
    }
    expected.count((_, _, kept) => !kept) should be > 0
  }
