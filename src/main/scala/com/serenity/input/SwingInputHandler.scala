package com.serenity.input

import java.awt.event.*
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicBoolean

import cats.effect.{Concurrent, Sync}
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo, Modifier}
import com.serenity.ui.layout.CellMetrics
import fs2.Stream

/** Bridges AWT keyboard and mouse events on a Swing component to the input pipeline.
  *
  * KEY_TYPED is used for printable characters (correct for keyboard-layout-aware input). KEY_PRESSED is used for
  * navigation/function keys and Ctrl+letter combinations. Mouse clicks are converted from pixel coords to cell coords
  * via CellMetrics.
  */
class SwingInputHandler[F[_] : Sync : Concurrent, E <: Event](
    component: java.awt.Component,
    inputRouter: InputRouter[F, E],
    metrics: () => CellMetrics
) extends InputHandler[F]:

  private val infoQueue    = new LinkedBlockingQueue[Option[KeyStrokeInfo]]()
  private val mouseQueue   = new LinkedBlockingQueue[Option[Event]]()
  private val shutdownFlag = new AtomicBoolean(false)

  private def enqueueInput(info: KeyStrokeInfo): Unit =
    if !shutdownFlag.get() then infoQueue.put(Some(info))

  private def enqueueMouse(event: Event): Unit =
    if !shutdownFlag.get() then mouseQueue.put(Some(event))

  component.addKeyListener(new KeyAdapter:
    override def keyTyped(e: KeyEvent): Unit =
      translateTyped(e).foreach(enqueueInput)
    override def keyPressed(e: KeyEvent): Unit =
      translatePressed(e).foreach(enqueueInput))

  component.addMouseListener(
    new MouseAdapter:
      override def mousePressed(e: MouseEvent): Unit =
        val currentMetrics = metrics()
        enqueueMouse(
          MousePress(
            currentMetrics.toCol(e.getX),
            currentMetrics.toRow(e.getY),
            pixelX = Some(e.getX),
            pixelY = Some(e.getY),
            shiftDown = e.isShiftDown,
            button = mouseButton(e)
          )
        )

      override def mouseClicked(e: MouseEvent): Unit =
        val currentMetrics = metrics()
        enqueueMouse(
          MouseClick(
            currentMetrics.toCol(e.getX),
            currentMetrics.toRow(e.getY),
            pixelX = Some(e.getX),
            pixelY = Some(e.getY),
            clickCount = e.getClickCount,
            shiftDown = e.isShiftDown,
            button = mouseButton(e)
          )
        )
  )

  component.addMouseMotionListener(
    new MouseMotionAdapter:
      override def mouseMoved(e: MouseEvent): Unit =
        val currentMetrics = metrics()
        enqueueMouse(
          MouseMove(
            currentMetrics.toCol(e.getX),
            currentMetrics.toRow(e.getY),
            pixelX = Some(e.getX),
            pixelY = Some(e.getY),
            shiftDown = e.isShiftDown
          )
        )

      override def mouseDragged(e: MouseEvent): Unit =
        val currentMetrics = metrics()
        enqueueMouse(
          MouseDrag(
            currentMetrics.toCol(e.getX),
            currentMetrics.toRow(e.getY),
            pixelX = Some(e.getX),
            pixelY = Some(e.getY),
            shiftDown = e.isShiftDown,
            button = dragButton(e)
          )
        )
  )

  def keyStrokeInfoStream: Stream[F, KeyStrokeInfo] =
    queueStream(infoQueue)

  private def mouseStream: Stream[F, Event] =
    queueStream(mouseQueue)

  def eventStream: Stream[F, Event] =
    inputRouter.eventStream(keyStrokeInfoStream).merge(mouseStream)

  private def queueStream[A](queue: LinkedBlockingQueue[Option[A]]): Stream[F, A] =
    Stream.eval(Sync[F].delay(shutdownFlag.get())).flatMap {
      case true  => Stream.empty
      case false => Stream.repeatEval(Sync[F].blocking(queue.take())).unNoneTerminate
    }

  private def mouseButton(e: MouseEvent): MouseButton =
    e.getButton match
      case MouseEvent.BUTTON1 => MouseButton.Primary
      case MouseEvent.BUTTON2 => MouseButton.Middle
      case MouseEvent.BUTTON3 => MouseButton.Secondary
      case _                  => MouseButton.Other

  private def dragButton(e: MouseEvent): MouseButton =
    val modifiers = e.getModifiersEx
    if (modifiers & InputEvent.BUTTON1_DOWN_MASK) != 0 then MouseButton.Primary
    else if (modifiers & InputEvent.BUTTON2_DOWN_MASK) != 0 then MouseButton.Middle
    else if (modifiers & InputEvent.BUTTON3_DOWN_MASK) != 0 then MouseButton.Secondary
    else MouseButton.Other

  def shutdown: F[Unit] =
    Sync[F].blocking {
      if shutdownFlag.compareAndSet(false, true) then
        val _ = infoQueue.offer(None)
        val _ = mouseQueue.offer(None)
    }

  private def mods(e: KeyEvent): Set[Modifier] =
    Set(
      Option.when(e.isControlDown)(Modifier.Ctrl),
      Option.when(e.isAltDown)(Modifier.Alt),
      Option.when(e.isShiftDown)(Modifier.Shift),
      Option.when(e.isMetaDown)(Modifier.Meta)
    ).flatten

  private def translateTyped(e: KeyEvent): Option[KeyStrokeInfo] =
    val char = e.getKeyChar
    if !e.isControlDown && !e.isMetaDown && char != KeyEvent.CHAR_UNDEFINED && !Character.isISOControl(char) then
      Some(KeyStrokeInfo(InputKey.Character, Some(char), Set.empty))
    else None

  private def translatePressed(e: KeyEvent): Option[KeyStrokeInfo] =
    import KeyEvent.*
    val m = mods(e)
    e.getKeyCode match
      case VK_UP         => Some(KeyStrokeInfo(InputKey.ArrowUp, None, m))
      case VK_DOWN       => Some(KeyStrokeInfo(InputKey.ArrowDown, None, m))
      case VK_LEFT       => Some(KeyStrokeInfo(InputKey.ArrowLeft, None, m))
      case VK_RIGHT      => Some(KeyStrokeInfo(InputKey.ArrowRight, None, m))
      case VK_BACK_SPACE => Some(KeyStrokeInfo(InputKey.Backspace, None, m))
      case VK_DELETE     => Some(KeyStrokeInfo(InputKey.Delete, None, m))
      case VK_ENTER      => Some(KeyStrokeInfo(InputKey.Enter, None, m))
      case VK_TAB        =>
        // Shift is encoded as ReverseTab; strip it from modifiers so translators
        // only see Ctrl/Alt when deciding between NextTab / RunnerPreviousCategory etc.
        val tabMods = m - Modifier.Shift
        if e.isShiftDown then Some(KeyStrokeInfo(InputKey.ReverseTab, None, tabMods))
        else Some(KeyStrokeInfo(InputKey.Tab, None, tabMods))
      case VK_ESCAPE                               => Some(KeyStrokeInfo(InputKey.Escape, None, m))
      case VK_HOME                                 => Some(KeyStrokeInfo(InputKey.Home, None, m))
      case VK_END                                  => Some(KeyStrokeInfo(InputKey.End, None, m))
      case VK_PAGE_UP                              => Some(KeyStrokeInfo(InputKey.PageUp, None, m))
      case VK_PAGE_DOWN                            => Some(KeyStrokeInfo(InputKey.PageDown, None, m))
      case VK_F1                                   => Some(KeyStrokeInfo(InputKey.F1, None, m))
      case VK_F2                                   => Some(KeyStrokeInfo(InputKey.F2, None, m))
      case VK_F3                                   => Some(KeyStrokeInfo(InputKey.F3, None, m))
      case VK_F4                                   => Some(KeyStrokeInfo(InputKey.F4, None, m))
      case VK_F5                                   => Some(KeyStrokeInfo(InputKey.F5, None, m))
      case VK_F6                                   => Some(KeyStrokeInfo(InputKey.F6, None, m))
      case VK_F7                                   => Some(KeyStrokeInfo(InputKey.F7, None, m))
      case VK_F8                                   => Some(KeyStrokeInfo(InputKey.F8, None, m))
      case VK_F9                                   => Some(KeyStrokeInfo(InputKey.F9, None, m))
      case VK_F10                                  => Some(KeyStrokeInfo(InputKey.F10, None, m))
      case VK_F11                                  => Some(KeyStrokeInfo(InputKey.F11, None, m))
      case VK_F12                                  => Some(KeyStrokeInfo(InputKey.F12, None, m))
      case code if e.isControlDown || e.isMetaDown =>
        // Modified letters/digits are represented as character strokes for hotkey matching.
        val ch = code.toChar.toLower
        if (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') then
          Some(KeyStrokeInfo(InputKey.Character, Some(ch), m))
        else None
      case _ => None
