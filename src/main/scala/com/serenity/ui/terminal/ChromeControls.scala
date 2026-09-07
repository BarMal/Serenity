package com.serenity.ui.terminal

import java.awt.*
import java.awt.event.*
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.swing.*

import com.serenity.animation.WindowSitter

/** A single custom-chrome title-bar button (minimize/maximize/restore/close).
  *
  * Decoupled from [[SwingWindow]] itself -- it takes the palette, preferred size, and activation behaviour it needs
  * as constructor parameters rather than reaching into an enclosing instance, so it can live in its own file.
  */
final private[terminal] class ChromeControlButton(
    initialKind: SwingWindow.ChromeControlKind,
    chromePaletteRef: AtomicReference[SwingWindow.ChromePalette],
    preferredSize: () => Dimension,
    onActivate: SwingWindow.ChromeControlKind => Unit
) extends JComponent:
  private val kindRef    = new AtomicReference(initialKind)
  private val hoverRef   = new AtomicBoolean(false)
  private val pressedRef = new AtomicBoolean(false)

  setOpaque(true)
  setFocusable(true)
  setPreferredSize(preferredSize())
  SwingWindow.setAccessibleNameIfAvailable(this, initialKind.accessibleName)

  def setKind(kind: SwingWindow.ChromeControlKind): Unit =
    kindRef.set(kind)
    SwingWindow.setAccessibleNameIfAvailable(this, kind.accessibleName)
    repaint()

  override def paintComponent(g: Graphics): Unit =
    val palette = chromePaletteRef.get()
    val state = SwingWindow.ChromeControlState(
      hovered = hoverRef.get(),
      pressed = pressedRef.get(),
      focused = hasFocus
    )
    val kind       = kindRef.get()
    val background = SwingWindow.ChromeControlPaint.background(kind, palette, state)
    val foreground = SwingWindow.ChromeControlPaint.foreground(kind, palette, state)
    val g2         = g.create().asInstanceOf[Graphics2D]
    try
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2.setColor(background)
      g2.fillRect(0, 0, getWidth, getHeight)
      g2.setColor(foreground)
      g2.setStroke(new BasicStroke(SwingWindow.ChromeIconGeometry.strokeWidth(getHeight).toFloat))
      SwingWindow.ChromeIconGeometry.lines(kind, getWidth, getHeight).foreach { line =>
        g2.drawLine(line.x1, line.y1, line.x2, line.y2)
      }
      SwingWindow.ChromeControlPaint.focusBorder(palette, state).foreach { border =>
        g2.setColor(border)
        g2.drawRect(1, 1, getWidth - 3, getHeight - 3)
      }
    finally g2.dispose()

  addMouseListener(new MouseAdapter:
    override def mouseEntered(e: MouseEvent): Unit =
      hoverRef.set(true)
      repaint()
    override def mouseExited(e: MouseEvent): Unit =
      hoverRef.set(false)
      repaint()
    override def mousePressed(e: MouseEvent): Unit =
      pressedRef.set(true)
      repaint()
    override def mouseReleased(e: MouseEvent): Unit =
      val wasPressed = pressedRef.getAndSet(false)
      repaint()
      if wasPressed && e.getX >= 0 && e.getX < getWidth && e.getY >= 0 && e.getY < getHeight then activate())

  addKeyListener(
    new KeyAdapter:
      override def keyPressed(e: KeyEvent): Unit =
        if e.getKeyCode == KeyEvent.VK_ENTER || e.getKeyCode == KeyEvent.VK_SPACE then
          pressedRef.set(true)
          repaint()
      override def keyReleased(e: KeyEvent): Unit =
        if e.getKeyCode == KeyEvent.VK_ENTER || e.getKeyCode == KeyEvent.VK_SPACE then
          val wasPressed = pressedRef.getAndSet(false)
          repaint()
          if wasPressed then activate()
  )

  private def activate(): Unit = onActivate(kindRef.get())

/** Builds the custom-chrome title bar: control buttons, spacer, decorative title label, and drag-to-move/
  * double-click-to-maximize wiring. Extracted out of [[SwingWindow]]'s constructor to keep that file within the
  * architecture ratchet's line targets; every piece of window state it touches (palette, metrics, the frame itself,
  * maximize toggling) is passed in explicitly.
  */
final private[terminal] class ChromeTitleBar(
    initialWindowSitter: WindowSitter,
    initialWindowSitterVisible: Boolean,
    chromePaletteRef: AtomicReference[SwingWindow.ChromePalette],
    maximizedRef: AtomicBoolean,
    frame: () => JFrame,
    controlFont: () => Font,
    buttonSize: () => Dimension,
    spacerSize: () => Dimension,
    titleBarSize: () => Dimension,
    onToggleMaximize: () => Unit,
    onActivate: SwingWindow.ChromeControlKind => Unit
):
  private def makeCtrlBtn(kind: SwingWindow.ChromeControlKind): ChromeControlButton =
    new ChromeControlButton(kind, chromePaletteRef, buttonSize, onActivate)

  private val controlLayout = SwingWindow.ChromeControlLayout.current
  private val buttonPairs   = controlLayout.controls.map(kind => kind -> makeCtrlBtn(kind))

  val controlButtons: scala.List[ChromeControlButton] = buttonPairs.map(_._2)
  val maxButton: Option[ChromeControlButton] =
    buttonPairs.collectFirst { case (SwingWindow.ChromeControlKind.Maximize, button) => button }

  val controlPanel: JPanel = new JPanel(new FlowLayout(controlLayout.flowAlignment, 0, 0)):
    setBackground(chromePaletteRef.get().titleBackground)
  buttonPairs.foreach((_, button) => controlPanel.add(button))

  val titleSpacer: JPanel = new JPanel:
    setBackground(chromePaletteRef.get().titleBackground)
    setPreferredSize(spacerSize())

  val titleLabel: SwingWindow.DecorativeTitleLabel =
    new SwingWindow.DecorativeTitleLabel(initialWindowSitter.glyph, initialWindowSitterVisible)
  titleLabel.setForeground(chromePaletteRef.get().titleForeground)
  titleLabel.setFont(controlFont())

  private val dragAdapter = new MouseAdapter:
    final private case class DragAnchor(x: Int, y: Int)
    private val anchorRef = new AtomicReference(DragAnchor(0, 0))

    override def mousePressed(e: MouseEvent): Unit =
      anchorRef.set(DragAnchor(e.getXOnScreen, e.getYOnScreen))

    override def mouseDragged(e: MouseEvent): Unit =
      val anchor = anchorRef.get()
      val decision = SwingWindow.titleBarDragDecision(
        maximized = maximizedRef.get(),
        anchorX = anchor.x,
        anchorY = anchor.y,
        pointerX = e.getXOnScreen,
        pointerY = e.getYOnScreen
      )
      if decision.restoreFirst then
        frame().setExtendedState(frame().getExtendedState & ~Frame.MAXIMIZED_BOTH)
        maximizedRef.set(false)
        anchorRef.set(DragAnchor(e.getXOnScreen, e.getYOnScreen))
      else
        decision.moveDelta.foreach {
          case (dx, dy) =>
            val loc = frame().getLocation
            frame().setLocation(loc.x + dx, loc.y + dy)
            anchorRef.set(DragAnchor(e.getXOnScreen, e.getYOnScreen))
        }

    override def mouseClicked(e: MouseEvent): Unit =
      if e.getClickCount == 2 then onToggleMaximize()

  val panel: JPanel = new JPanel(new BorderLayout):
    setBackground(chromePaletteRef.get().titleBackground)
    setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, chromePaletteRef.get().border))
    setPreferredSize(titleBarSize())
  controlLayout.placement match
    case SwingWindow.ChromeControlPlacement.Left =>
      panel.add(controlPanel, BorderLayout.WEST)
      panel.add(titleSpacer, BorderLayout.EAST)
    case SwingWindow.ChromeControlPlacement.Right =>
      panel.add(titleSpacer, BorderLayout.WEST)
      panel.add(controlPanel, BorderLayout.EAST)
  panel.add(titleLabel, BorderLayout.CENTER)
  panel.addMouseListener(dragAdapter)
  panel.addMouseMotionListener(dragAdapter)
  titleLabel.addMouseListener(dragAdapter)
  titleLabel.addMouseMotionListener(dragAdapter)
