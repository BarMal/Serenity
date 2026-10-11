package com.serenity.perf

import java.awt.Color
import java.io.StringWriter

import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{CellMetrics, PixelRect}
import com.serenity.ui.tui.TerminalRenderSurface

/** #1890: what the terminal backend spends per frame on a 200x60 screen -- the cost the grid representation exists to
  * make follow what changed instead of the screen's size. `typing_frame` is the renderer's own sequence for one typed
  * character (clear everything but the other rows, repaint the edited one, flush); `idle_flush` is a flush with nothing
  * written; `clear_preserved_rows` is the clear alone. The timer reports allocation alongside time.
  */
private[perf] object TuiBenchmarks:

  private val Width        = 200
  private val Height       = 60
  private val EditedRow    = 30
  private val white        = RenderColor.fromAwt(Color.WHITE)
  private val grey         = RenderColor.fromAwt(Color.GRAY)
  private val black        = RenderColor.fromAwt(Color.BLACK)
  private val TextVariants = 16
  private val Warmups      = 200
  private val Iterations   = BenchmarkIterationCounts.Tui

  private def rowText(y: Int, variant: Int): String =
    (0 until Width).map(x => if x == 10 then ('a' + variant).toChar else ('a' + (x * 7 + y * 3) % 26).toChar).mkString

  private def paintedSurface(): TerminalRenderSurface =
    val surface = new TerminalRenderSurface(
      Width,
      Height,
      new StringWriter(),
      CellMetrics(charWidth = 1, lineHeight = 1, ascent = 0)
    )
    surface.setBackgroundColor(black)
    (0 until Height).foreach { y =>
      surface.setForegroundColor(if y % 2 == 0 then white else grey)
      surface.putString(0, y, rowText(y, 0))
    }
    surface.flush()
    surface

  private val otherRows: List[PixelRect] =
    (0 until Height).filter(_ != EditedRow).map(y => PixelRect(0, y, Width, 1)).toList

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    List(typingFrame(), idleFlush(), clearPreservedRows())

  private def typingFrame(): BenchmarkRunner.Benchmark =
    val surface = paintedSurface()
    val texts   = Vector.tabulate(TextVariants)(rowText(EditedRow, _))
    val counter = new java.util.concurrent.atomic.AtomicInteger(0)
    BenchmarkRunner.Benchmark(
      "tui.flush.typing_frame.200x60",
      Warmups,
      Iterations,
      () => (),
      () =>
        surface.clearViewportExcept(black, otherRows)
        surface.setForegroundColor(if EditedRow % 2 == 0 then white else grey)
        surface.putString(0, EditedRow, texts(counter.getAndIncrement() % TextVariants))
        surface.flush()
    )

  private def idleFlush(): BenchmarkRunner.Benchmark =
    val surface = paintedSurface()
    BenchmarkRunner.Benchmark("tui.flush.idle.200x60", Warmups, Iterations, () => (), () => surface.flush())

  private def clearPreservedRows(): BenchmarkRunner.Benchmark =
    val surface = paintedSurface()
    BenchmarkRunner.Benchmark(
      "tui.clear.preserved_rows.200x60",
      Warmups,
      Iterations,
      () => (),
      () => surface.clearViewportExcept(black, otherRows)
    )
