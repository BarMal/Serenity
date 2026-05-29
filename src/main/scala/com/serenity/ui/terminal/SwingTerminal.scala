package com.serenity.ui.terminal

import cats.effect.{IO, Resource}
import com.serenity.ui.layout.{CellMetrics, TerminalSize}
import java.awt.Dimension
import java.awt.event.{ComponentAdapter, ComponentEvent, WindowAdapter, WindowEvent}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import javax.swing.{JFrame, JPanel, SwingUtilities, WindowConstants}

class SwingTerminal(initialPixelSize: Dimension, val metrics: CellMetrics):
  private val pixelSize     = new AtomicReference(initialPixelSize)
  private val pendingResize = new AtomicReference[Option[TerminalSize]](None)
  private val closeLatch    = new CountDownLatch(1)

  val canvas: JPanel = new JPanel:
    setBackground(java.awt.Color.BLACK)
    setPreferredSize(initialPixelSize)
    setFocusable(true)
    addComponentListener(new ComponentAdapter:
      override def componentResized(e: ComponentEvent): Unit =
        val d = getSize()
        pixelSize.set(d)
        pendingResize.set(Some(metrics.terminalSize(d.width, d.height)))
    )

  private val frame: JFrame =
    val f = new JFrame("Serenity")
    f.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    f.addWindowListener(new WindowAdapter:
      override def windowClosing(e: WindowEvent): Unit = closeLatch.countDown()
    )
    f.add(canvas)
    f.pack()
    f.setLocationRelativeTo(null)
    f

  def awaitClose: IO[Unit] = IO.blocking(closeLatch.await())

  def start(): Unit =
    SwingUtilities.invokeLater { () =>
      frame.setVisible(true)
      canvas.requestFocusInWindow()
    }

  def stop(): Unit =
    SwingUtilities.invokeLater { () =>
      frame.setVisible(false)
      frame.dispose()
    }

  /** Current terminal size in cell units, derived from the pixel size at call time. */
  def terminalSize: TerminalSize =
    val d = pixelSize.get()
    metrics.terminalSize(d.width, d.height)

  /** Returns a pending resize event if one occurred since the last call, then clears it. */
  def doResizeIfNecessary(): Option[TerminalSize] =
    pendingResize.getAndSet(None)

object SwingTerminal:
  /** Nominal cell metrics for an 8×16 monospace raster — used until Phase 3 derives real FontMetrics. */
  val DefaultMetrics: CellMetrics = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 13)

  def resource(metrics: CellMetrics = DefaultMetrics): Resource[IO, SwingTerminal] =
    Resource.make(
      IO.blocking {
        val term = new SwingTerminal(new Dimension(1024, 768), metrics)
        term.start()
        term
      }
    )(term => IO.blocking(term.stop()))
