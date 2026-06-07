package com.serenity.ui.terminal

import java.awt.*
import java.awt.event.*
import java.awt.geom.RoundRectangle2D
import java.awt.image.BufferedImage
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.swing.*

import cats.effect.{IO, Resource}

import com.serenity.config.WindowChromeMode
import com.serenity.ui.layout.{CellMetrics, ViewportSize}

class SwingWindow(
    initialPixelSize: Dimension,
    initialMetrics: CellMetrics,
    chromeMode: WindowChromeMode = WindowChromeMode.Native
):

  private val TitleBarH     = 32
  private val BtnW          = 46
  private val Margin        = 6
  private val CornerArc     = 12
  private val MinW          = 400
  private val MinH          = 300
  private val ColBar        = new Color(0x2b2b2b)
  private val ColBarFg      = new Color(0xcccccc)
  private val ColBtnHover   = new Color(0x3f3f3f)
  private val ColCloseHover = new Color(0xc42b1c)

  private val pixelSize           = new AtomicReference(initialPixelSize)
  private val metricsRef          = new AtomicReference(initialMetrics)
  private val pendingResize       = new AtomicReference[Option[ViewportSize]](None)
  private val closeLatch          = new CountDownLatch(1)
  private val renderedImageRef    = new AtomicReference[Option[BufferedImage]](None)
  private val savedBoundsRef      = new AtomicReference[Option[Rectangle]](None)
  private val maximizedRef        = new AtomicBoolean(false)
  private val maxBtnRef           = new AtomicReference[Option[JLabel]](None)
  private val onResizeCallbackRef = new AtomicReference[Option[() => Unit]](None)
  private val usesCustomChrome    = chromeMode == WindowChromeMode.Custom

  def setOnResize(cb: () => Unit): Unit = onResizeCallbackRef.set(Some(cb))

  val canvas: JPanel = new JPanel:
    setBackground(Color.BLACK)
    setPreferredSize(initialPixelSize)
    setFocusable(true)
    setFocusTraversalKeysEnabled(false)
    addComponentListener(
      new ComponentAdapter:
        override def componentResized(e: ComponentEvent): Unit =
          val d = getSize()
          pixelSize.set(d)
          pendingResize.set(Some(metrics.viewportSize(d.width, d.height)))
          onResizeCallbackRef.get().foreach(_.apply())
    )
    override def paintComponent(g: java.awt.Graphics): Unit =
      g.setColor(Color.BLACK)
      g.fillRect(0, 0, getWidth, getHeight)
      renderedImageRef.get().foreach(img => g.drawImage(img, 0, 0, null))

  def onImageReady(image: BufferedImage): Unit =
    renderedImageRef.set(Some(image))
    SwingUtilities.invokeLater(() => canvas.repaint())

  private def updateShape(): Unit =
    if usesCustomChrome && !maximizedRef.get() then
      val d = frame.getSize
      frame.setShape(new RoundRectangle2D.Double(0, 0, d.width, d.height, CornerArc, CornerArc))
    else if usesCustomChrome then frame.setShape(null)

  private def toggleMaximize(): Unit =
    if maximizedRef.get() then
      frame.setExtendedState(Frame.NORMAL)
      savedBoundsRef.get().foreach(frame.setBounds)
    else
      savedBoundsRef.set(Some(frame.getBounds))
      frame.setExtendedState(Frame.MAXIMIZED_BOTH)

  private def makeCtrlBtn(label: String, isClose: Boolean): JLabel =
    new JLabel(label, SwingConstants.CENTER):
      setOpaque(true)
      setBackground(ColBar)
      setForeground(ColBarFg)
      setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 13))
      setPreferredSize(new Dimension(BtnW, TitleBarH))
      addMouseListener(new MouseAdapter:
        override def mouseEntered(e: MouseEvent): Unit =
          setBackground(if isClose then ColCloseHover else ColBtnHover)
        override def mouseExited(e: MouseEvent): Unit =
          setBackground(ColBar)
        override def mouseReleased(e: MouseEvent): Unit =
          if e.getX >= 0 && e.getX < getWidth && e.getY >= 0 && e.getY < getHeight then
            if isClose then closeLatch.countDown()
            else if label == "─" then frame.setExtendedState(Frame.ICONIFIED)
            else toggleMaximize())

  private val titleBar: JPanel =
    val minBtn = makeCtrlBtn("─", isClose = false)
    val maxBtn = makeCtrlBtn("□", isClose = false)
    maxBtnRef.set(Some(maxBtn))
    val closeBtn = makeCtrlBtn("✕", isClose = true)

    val btnPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 0)):
      setBackground(ColBar)
    btnPanel.add(minBtn)
    btnPanel.add(maxBtn)
    btnPanel.add(closeBtn)

    val spacer = new JPanel:
      setBackground(ColBar)
      setPreferredSize(new Dimension(3 * BtnW, TitleBarH))

    val titleLabel = new JLabel("Serenity", SwingConstants.CENTER):
      setForeground(ColBarFg)
      setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 13))

    val dragAdapter = new MouseAdapter:
      private case class DragAnchor(x: Int, y: Int)
      private val anchorRef = new AtomicReference(DragAnchor(0, 0))

      override def mousePressed(e: MouseEvent): Unit =
        anchorRef.set(DragAnchor(e.getXOnScreen, e.getYOnScreen))

      override def mouseDragged(e: MouseEvent): Unit =
        if !maximizedRef.get() then
          val anchor = anchorRef.get()
          val dx     = e.getXOnScreen - anchor.x
          val dy     = e.getYOnScreen - anchor.y
          val loc    = frame.getLocation
          frame.setLocation(loc.x + dx, loc.y + dy)
          anchorRef.set(DragAnchor(e.getXOnScreen, e.getYOnScreen))

      override def mouseClicked(e: MouseEvent): Unit =
        if e.getClickCount == 2 then toggleMaximize()

    val bar = new JPanel(new BorderLayout):
      setBackground(ColBar)
      setPreferredSize(new Dimension(0, TitleBarH))
    bar.add(spacer, BorderLayout.WEST)
    bar.add(titleLabel, BorderLayout.CENTER)
    bar.add(btnPanel, BorderLayout.EAST)
    bar.addMouseListener(dragAdapter)
    bar.addMouseMotionListener(dragAdapter)
    titleLabel.addMouseListener(dragAdapter)
    titleLabel.addMouseMotionListener(dragAdapter)
    bar

  private class ResizeGlassPane extends JComponent:
    setOpaque(false)
    setFocusable(false)

    private case class ResizeState(
        resizing: Boolean = false,
        resizeDir: Int = 0,
        pressX: Int = 0,
        pressY: Int = 0,
        pressBounds: Rectangle = new Rectangle()
    )

    private case class BoundsBox(x: Int, y: Int, width: Int, height: Int)
    private val resizeStateRef = new AtomicReference(ResizeState())

    override def contains(x: Int, y: Int): Boolean =
      x < Margin || x > getWidth - Margin || y < Margin || y > getHeight - Margin

    private def edgeDir(e: MouseEvent): Int =
      val x = e.getX; val y   = e.getY
      val w = getWidth; val h = getHeight
      scala
        .List(
          Option.when(y < Margin)(1),
          Option.when(y > h - Margin)(2),
          Option.when(x < Margin)(4),
          Option.when(x > w - Margin)(8)
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
              pressBounds = frame.getBounds
            )
          )

      override def mouseReleased(e: MouseEvent): Unit =
        resizeStateRef.updateAndGet(_.copy(resizing = false, resizeDir = 0))

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
          if finalBounds.width >= MinW && finalBounds.height >= MinH then
            frame.setBounds(finalBounds.x, finalBounds.y, finalBounds.width, finalBounds.height)

    addMouseListener(adapter)
    addMouseMotionListener(adapter)

  private val frame: JFrame =
    val f = new JFrame("Serenity")
    f.setUndecorated(usesCustomChrome)
    f.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    f.addWindowListener(
      new WindowAdapter:
        override def windowClosing(e: WindowEvent): Unit = closeLatch.countDown()
    )
    f.addWindowStateListener((e: WindowEvent) =>
      val isMax = (e.getNewState & Frame.MAXIMIZED_BOTH) == Frame.MAXIMIZED_BOTH
      maximizedRef.set(isMax)
      SwingUtilities.invokeLater { () =>
        if usesCustomChrome then
          maxBtnRef.get().foreach(_.setText(if isMax then "❐" else "□"))
          updateShape()
      }
    )
    f.addComponentListener(
      new ComponentAdapter:
        override def componentResized(e: ComponentEvent): Unit =
          if usesCustomChrome then SwingUtilities.invokeLater(() => updateShape())
    )
    val content = new JPanel(new BorderLayout):
      setBackground(Color.BLACK)
    if usesCustomChrome then content.add(titleBar, BorderLayout.NORTH)
    content.add(canvas, BorderLayout.CENTER)
    f.setContentPane(content)
    if usesCustomChrome then
      val glassPane = new ResizeGlassPane
      f.setGlassPane(glassPane)
      glassPane.setVisible(true)
    f.pack()
    f.setMinimumSize(new Dimension(MinW, MinH))
    f.setLocationRelativeTo(null)
    f

  def awaitClose: IO[Unit] = IO.blocking(closeLatch.await())

  def start(): Unit =
    SwingUtilities.invokeLater { () =>
      frame.setVisible(true)
      if usesCustomChrome then updateShape()
      canvas.requestFocusInWindow()
    }

  def stop(): Unit =
    SwingUtilities.invokeLater { () =>
      frame.setVisible(false)
      frame.dispose()
    }

  def viewportSize: ViewportSize =
    val d = pixelSize.get()
    metrics.viewportSize(d.width, d.height)

  def metrics: CellMetrics =
    metricsRef.get()

  def updateMetrics(newMetrics: CellMetrics): Unit =
    metricsRef.set(newMetrics)
    val d = pixelSize.get()
    pendingResize.set(Some(newMetrics.viewportSize(d.width, d.height)))
    onResizeCallbackRef.get().foreach(_.apply())

  def doResizeIfNecessary(): Option[ViewportSize] =
    pendingResize.getAndSet(None)

object SwingWindow:
  val DefaultMetrics: CellMetrics = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 13)

  def resource(
    metrics: CellMetrics = DefaultMetrics,
    chromeMode: WindowChromeMode = WindowChromeMode.Native
  ): Resource[IO, SwingWindow] =
    Resource.make(
      IO.blocking {
        val win = new SwingWindow(new Dimension(1024, 768), metrics, chromeMode)
        win.start()
        win
      }
    )(win => IO.blocking(win.stop()))
