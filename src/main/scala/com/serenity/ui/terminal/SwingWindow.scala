package com.serenity.ui.terminal

import cats.effect.{IO, Resource}
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import java.awt.{BorderLayout, Color, Component, Cursor, Dimension, FlowLayout, Font, Frame, Rectangle}
import java.awt.event.{ComponentAdapter, ComponentEvent, MouseAdapter, MouseEvent, WindowAdapter, WindowEvent, WindowStateListener}
import java.awt.image.BufferedImage
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference
import javax.swing.{JComponent, JFrame, JLabel, JPanel, SwingConstants, SwingUtilities, WindowConstants}
import javax.swing.border.LineBorder

class SwingWindow(initialPixelSize: Dimension, val metrics: CellMetrics):

  private val TitleBarH     = 32
  private val BtnW          = 46
  private val Margin        = 6
  private val MinW          = 400
  private val MinH          = 300
  private val ColBar        = new Color(0x2b2b2b)
  private val ColBarFg      = new Color(0xcccccc)
  private val ColBorder     = new Color(0x3c3c3c)
  private val ColBtnHover   = new Color(0x3f3f3f)
  private val ColCloseHover = new Color(0xc42b1c)

  private val pixelSize     = new AtomicReference(initialPixelSize)
  private val pendingResize = new AtomicReference[Option[ViewportSize]](None)
  private val closeLatch    = new CountDownLatch(1)
  @volatile private var renderedImage: BufferedImage = null
  @volatile private var savedBounds: Rectangle = null
  @volatile private var maximized: Boolean = false
  @volatile private var maxBtnRef: JLabel = null

  val canvas: JPanel = new JPanel:
    setBackground(Color.BLACK)
    setPreferredSize(initialPixelSize)
    setFocusable(true)
    addComponentListener(new ComponentAdapter:
      override def componentResized(e: ComponentEvent): Unit =
        val d = getSize()
        pixelSize.set(d)
        pendingResize.set(Some(metrics.viewportSize(d.width, d.height)))
    )
    override def paintComponent(g: java.awt.Graphics): Unit =
      val img = renderedImage
      if img != null then g.drawImage(img, 0, 0, null)
      else
        g.setColor(Color.BLACK)
        g.fillRect(0, 0, getWidth, getHeight)

  def onImageReady(image: BufferedImage): Unit =
    renderedImage = image
    SwingUtilities.invokeLater(() => canvas.repaint())

  private def toggleMaximize(): Unit =
    if maximized then
      frame.setExtendedState(Frame.NORMAL)
      if savedBounds != null then frame.setBounds(savedBounds)
    else
      savedBounds = frame.getBounds
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
        override def mouseClicked(e: MouseEvent): Unit =
          if isClose then closeLatch.countDown()
          else if label == "─" then frame.setExtendedState(Frame.ICONIFIED)
          else toggleMaximize()
      )

  private val titleBar: JPanel =
    val minBtn   = makeCtrlBtn("─", isClose = false)
    val maxBtn   = makeCtrlBtn("□", isClose = false)
    maxBtnRef    = maxBtn
    val closeBtn = makeCtrlBtn("✕", isClose = true)

    val btnPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT, 0, 0)):
      setBackground(ColBar)
    btnPanel.add(minBtn)
    btnPanel.add(maxBtn)
    btnPanel.add(closeBtn)

    val titleLabel = new JLabel("  Serenity"):
      setForeground(ColBarFg)
      setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 13))

    val dragAdapter = new MouseAdapter:
      private var pressX = 0
      private var pressY = 0

      override def mousePressed(e: MouseEvent): Unit =
        pressX = e.getXOnScreen
        pressY = e.getYOnScreen

      override def mouseDragged(e: MouseEvent): Unit =
        if !maximized then
          val dx  = e.getXOnScreen - pressX
          val dy  = e.getYOnScreen - pressY
          val loc = frame.getLocation
          frame.setLocation(loc.x + dx, loc.y + dy)
          pressX = e.getXOnScreen
          pressY = e.getYOnScreen

      override def mouseClicked(e: MouseEvent): Unit =
        if e.getClickCount == 2 then toggleMaximize()

    val bar = new JPanel(new BorderLayout):
      setBackground(ColBar)
      setPreferredSize(new Dimension(0, TitleBarH))
    bar.add(titleLabel, BorderLayout.WEST)
    bar.add(btnPanel, BorderLayout.EAST)
    bar.addMouseListener(dragAdapter)
    bar.addMouseMotionListener(dragAdapter)
    titleLabel.addMouseListener(dragAdapter)
    titleLabel.addMouseMotionListener(dragAdapter)
    bar

  private class ResizeGlassPane extends JComponent:
    setOpaque(false)
    setFocusable(false)

    private var resizing    = false
    private var resizeDir   = 0
    private var pressX      = 0
    private var pressY      = 0
    private var pressBounds = new Rectangle()
    private var dragTarget: Component = null
    private var hoverTarget: Component = null

    private def edgeDir(e: MouseEvent): Int =
      val x = e.getX; val y = e.getY
      val w = getWidth; val h = getHeight
      var d = 0
      if y < Margin then d |= 1
      if y > h - Margin then d |= 2
      if x < Margin then d |= 4
      if x > w - Margin then d |= 8
      d

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

    private def deepestAt(e: MouseEvent): Component =
      val cp = frame.getContentPane
      val pt = SwingUtilities.convertPoint(this, e.getPoint, cp)
      SwingUtilities.getDeepestComponentAt(cp, pt.x, pt.y)

    private def redispatch(e: MouseEvent, target: Component): Unit =
      val pt = SwingUtilities.convertPoint(this, e.getPoint, target)
      target.dispatchEvent(
        new MouseEvent(target, e.getID, e.getWhen, e.getModifiersEx,
          pt.x, pt.y, e.getClickCount, e.isPopupTrigger, e.getButton)
      )

    private def updateHover(e: MouseEvent, newTarget: Component): Unit =
      if newTarget != hoverTarget then
        if hoverTarget != null then
          val pt = SwingUtilities.convertPoint(this, e.getPoint, hoverTarget)
          hoverTarget.dispatchEvent(
            new MouseEvent(hoverTarget, MouseEvent.MOUSE_EXITED, e.getWhen, 0, pt.x, pt.y, 0, false)
          )
        if newTarget != null then
          val pt = SwingUtilities.convertPoint(this, e.getPoint, newTarget)
          newTarget.dispatchEvent(
            new MouseEvent(newTarget, MouseEvent.MOUSE_ENTERED, e.getWhen, 0, pt.x, pt.y, 0, false)
          )
        hoverTarget = newTarget

    private val adapter = new MouseAdapter:
      override def mousePressed(e: MouseEvent): Unit =
        val d = edgeDir(e)
        if d != 0 && !maximized then
          resizing    = true
          resizeDir   = d
          pressX      = e.getXOnScreen
          pressY      = e.getYOnScreen
          pressBounds = frame.getBounds
        else
          dragTarget = deepestAt(e)
          if dragTarget != null then redispatch(e, dragTarget)

      override def mouseReleased(e: MouseEvent): Unit =
        if resizing then
          resizing = false
          resizeDir = 0
        else if dragTarget != null then
          redispatch(e, dragTarget)
          dragTarget = null

      override def mouseClicked(e: MouseEvent): Unit =
        val t = deepestAt(e)
        if t != null then redispatch(e, t)

      override def mouseExited(e: MouseEvent): Unit =
        updateHover(e, null)
        setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR))

      override def mouseMoved(e: MouseEvent): Unit =
        val d = edgeDir(e)
        if d != 0 then
          setCursor(dirCursor(d))
          updateHover(e, null)
        else
          setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR))
          val t = deepestAt(e)
          updateHover(e, t)
          if t != null then redispatch(e, t)

      override def mouseDragged(e: MouseEvent): Unit =
        if resizing then
          val dx = e.getXOnScreen - pressX
          val dy = e.getYOnScreen - pressY
          var nx = pressBounds.x;     var ny = pressBounds.y
          var nw = pressBounds.width; var nh = pressBounds.height
          if (resizeDir & 1) != 0 then { ny = pressBounds.y + dy;    nh = pressBounds.height - dy }
          if (resizeDir & 2) != 0 then   nh = pressBounds.height + dy
          if (resizeDir & 4) != 0 then { nx = pressBounds.x + dx;    nw = pressBounds.width - dx }
          if (resizeDir & 8) != 0 then   nw = pressBounds.width + dx
          if nw >= MinW && nh >= MinH then frame.setBounds(nx, ny, nw, nh)
        else if dragTarget != null then
          redispatch(e, dragTarget)

    addMouseListener(adapter)
    addMouseMotionListener(adapter)

  private val frame: JFrame =
    val f = new JFrame("Serenity")
    f.setUndecorated(true)
    f.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
    f.addWindowListener(new WindowAdapter:
      override def windowClosing(e: WindowEvent): Unit = closeLatch.countDown()
    )
    f.addWindowStateListener((e: WindowEvent) =>
      val isMax = (e.getNewState & Frame.MAXIMIZED_BOTH) == Frame.MAXIMIZED_BOTH
      maximized = isMax
      SwingUtilities.invokeLater(() =>
        if maxBtnRef != null then maxBtnRef.setText(if isMax then "❐" else "□")
      )
    )
    val content = new JPanel(new BorderLayout):
      setBackground(Color.BLACK)
    content.add(titleBar, BorderLayout.NORTH)
    content.add(canvas, BorderLayout.CENTER)
    f.setContentPane(content)
    f.getRootPane.setBorder(new LineBorder(ColBorder, 1))
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

  def doResizeIfNecessary(): Option[ViewportSize] =
    pendingResize.getAndSet(None)

object SwingWindow:
  val DefaultMetrics: CellMetrics = CellMetrics(charWidth = 8, lineHeight = 16, ascent = 13)

  def resource(metrics: CellMetrics = DefaultMetrics): Resource[IO, SwingWindow] =
    Resource.make(
      IO.blocking {
        val win = new SwingWindow(new Dimension(1024, 768), metrics)
        win.start()
        win
      }
    )(win => IO.blocking(win.stop()))
