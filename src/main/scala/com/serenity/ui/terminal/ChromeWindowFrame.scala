package com.serenity.ui.terminal

import java.awt.*
import java.awt.event.*
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.swing.*

/** The custom-chrome resize border: an invisible glass-pane overlay whose edges and corners drive an edge-drag resize
  * of `frame`. Decoupled from [[SwingWindow]] so it can live in its own file -- `frame` is a thunk rather than a plain
  * reference because this class is built before the enclosing window's `frame` field finishes initializing; the thunk
  * is only ever invoked later, from an event callback, by which point `frame` is set.
  */
final private[terminal] class ResizeGlassPane(
    frame: () => JFrame,
    chromeMetricsRef: AtomicReference[SwingWindow.ChromeMetrics],
    maximizedRef: AtomicBoolean
) extends JComponent:
  setOpaque(false)
  setFocusable(false)

  final private case class ResizeState(
      resizing: Boolean = false,
      resizeDir: Int = 0,
      pressX: Int = 0,
      pressY: Int = 0,
      pressBounds: Rectangle = new Rectangle()
  )

  final private case class BoundsBox(x: Int, y: Int, width: Int, height: Int)
  private val resizeStateRef = new AtomicReference(ResizeState())

  override def contains(x: Int, y: Int): Boolean =
    val margin = chromeMetricsRef.get().margin
    x < margin || x > getWidth - margin || y < margin || y > getHeight - margin

  private def edgeDir(e: MouseEvent): Int =
    val x      = e.getX; val y   = e.getY
    val w      = getWidth; val h = getHeight
    val margin = chromeMetricsRef.get().margin
    scala
      .List(
        Option.when(y < margin)(1),
        Option.when(y > h - margin)(2),
        Option.when(x < margin)(4),
        Option.when(x > w - margin)(8)
      )
      .flatten
      .foldLeft(0)(_ | _)

  private def dirCursor(d: Int): Cursor = d match
    case 1  => Cursor.getPredefinedCursor(Cursor.N_RESIZE_CURSOR)
    case 2  => Cursor.getPredefinedCursor(Cursor.S_RESIZE_CURSOR)
    case 4  => Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR)
    case 8  => Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR)
    case 5  => Cursor.getPredefinedCursor(Cursor.NW_RESIZE_CURSOR)
    case 9  => Cursor.getPredefinedCursor(Cursor.NE_RESIZE_CURSOR)
    case 6  => Cursor.getPredefinedCursor(Cursor.SW_RESIZE_CURSOR)
    case 10 => Cursor.getPredefinedCursor(Cursor.SE_RESIZE_CURSOR)
    case _  => Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)

  private val adapter = new MouseAdapter:
    override def mousePressed(e: MouseEvent): Unit =
      val d = edgeDir(e)
      if d != 0 && !maximizedRef.get() then
        resizeStateRef.set(
          ResizeState(
            resizing = true,
            resizeDir = d,
            pressX = e.getXOnScreen,
            pressY = e.getYOnScreen,
            pressBounds = frame().getBounds
          )
        )

    override def mouseReleased(e: MouseEvent): Unit =
      val _ = resizeStateRef.updateAndGet(_.copy(resizing = false, resizeDir = 0))

    override def mouseMoved(e: MouseEvent): Unit =
      setCursor(dirCursor(edgeDir(e)))

    override def mouseExited(e: MouseEvent): Unit =
      setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR))

    override def mouseDragged(e: MouseEvent): Unit =
      val state = resizeStateRef.get()
      if state.resizing then
        val dx = e.getXOnScreen - state.pressX
        val dy = e.getYOnScreen - state.pressY
        val base = BoundsBox(
          state.pressBounds.x,
          state.pressBounds.y,
          state.pressBounds.width,
          state.pressBounds.height
        )
        val afterNorth =
          if (state.resizeDir & 1) != 0 then
            base.copy(y = state.pressBounds.y + dy, height = state.pressBounds.height - dy)
          else base
        val afterSouth =
          if (state.resizeDir & 2) != 0 then afterNorth.copy(height = state.pressBounds.height + dy)
          else afterNorth
        val afterWest =
          if (state.resizeDir & 4) != 0 then
            afterSouth.copy(x = state.pressBounds.x + dx, width = state.pressBounds.width - dx)
          else afterSouth
        val finalBounds =
          if (state.resizeDir & 8) != 0 then afterWest.copy(width = state.pressBounds.width + dx)
          else afterWest
        val chrome = chromeMetricsRef.get()
        if finalBounds.width >= chrome.minWidth && finalBounds.height >= chrome.minHeight then
          frame().setBounds(finalBounds.x, finalBounds.y, finalBounds.width, finalBounds.height)

  addMouseListener(adapter)
  addMouseMotionListener(adapter)

/** The window's content pane, rendered with soft rounded corners whenever per-pixel translucency lets the frame
  * composite them against the desktop. Decoupled from [[SwingWindow]] so it can live in its own file.
  */
private[terminal] class RoundedContentPane(
    layout: LayoutManager,
    usesCustomChrome: Boolean,
    maximizedRef: AtomicBoolean,
    perPixelTranslucencySupported: Boolean,
    chromeMetricsRef: AtomicReference[SwingWindow.ChromeMetrics],
    roundedContentBuffers: SwingWindow.RoundedCornerMaskBufferCache
) extends JPanel(layout):
  setOpaque(false)

  override def paint(g: Graphics): Unit =
    if SwingWindow.shouldUsePerPixelRoundedCorners(
          usesCustomChrome,
          maximizedRef.get(),
          perPixelTranslucencySupported
        ) && getWidth > 0 && getHeight > 0
    then
      val buffers = roundedContentBuffers.acquire(getWidth, getHeight, chromeMetricsRef.get().cornerArc)
      val _       = g.drawImage(buffers.render(contentsGraphics => super.paint(contentsGraphics)), 0, 0, null)
    else super.paint(g)
