package com.serenity.input

import java.awt.event.*
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import java.util.concurrent.{ConcurrentLinkedQueue, Semaphore}

import cats.effect.Sync
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
class SwingInputHandler[F[_] : Sync, E <: Event](
    component: java.awt.Component,
    inputRouter: InputRouter[F, E],
    metrics: () => CellMetrics,
    uiMetrics: () => CellMetrics
) extends InputHandler[F]:

  def this(component: java.awt.Component, inputRouter: InputRouter[F, E], metrics: () => CellMetrics) =
    this(component, inputRouter, metrics, metrics)

  private enum MovementKind:
    case Move, Drag

  sealed private trait MovementState
  final private case class AvailableMovement(event: Event) extends MovementState
  private case object ClaimedMovement                      extends MovementState

  private class MovementSlot(val kind: MovementKind, event: Event):
    private val state = new AtomicReference[MovementState](AvailableMovement(event))

    def replace(event: Event): Boolean =
      def loop(current: MovementState): Boolean = current match
        case available: AvailableMovement =>
          if state.compareAndSet(available, AvailableMovement(event)) then true
          else loop(state.get())
        case ClaimedMovement => false
      loop(state.get())

    def claim: Option[Event] =
      state.getAndSet(ClaimedMovement) match
        case AvailableMovement(event) => Some(event)
        case ClaimedMovement          => None

  sealed private trait QueuedInput
  final private case class QueuedKey(info: KeyStrokeInfo)     extends QueuedInput
  final private case class QueuedMouse(event: Event)          extends QueuedInput
  final private case class QueuedMovement(slot: MovementSlot) extends QueuedInput
  private case object QueuedShutdown                          extends QueuedInput

  private val inputQueue         = new ConcurrentLinkedQueue[QueuedInput]()
  private val inputAvailable     = new Semaphore(0)
  private val latestMovement     = new AtomicReference[Option[MovementSlot]](None)
  private val enqueuesInFlight   = new AtomicInteger(0)
  private val shutdownFlag       = new AtomicBoolean(false)
  private val pendingModifierTap = new AtomicReference[Option[(Int, Long, Boolean)]](None)

  private val doubleTapWindowMillis = 200L

  private def enqueueInput(info: KeyStrokeInfo): Unit =
    enqueue(QueuedKey(info))

  private def enqueueMouse(event: Event): Unit =
    enqueue(QueuedMouse(event))

  private def enqueue(input: QueuedInput): Unit =
    if !shutdownFlag.get() then
      enqueuesInFlight.incrementAndGet()
      if !shutdownFlag.get() then
        input match
          case QueuedMouse(event: MouseMove) => enqueueMovement(MovementKind.Move, event)
          case QueuedMouse(event: MouseDrag) => enqueueMovement(MovementKind.Drag, event)
          case _                             => enqueueNonMovement(input)
      val _ = enqueuesInFlight.decrementAndGet()

  private def enqueueMovement(kind: MovementKind, event: Event): Unit =
    def loop(): Unit =
      latestMovement.get() match
        case Some(slot) if slot.kind == kind && slot.replace(event) => ()
        case current =>
          val replacement = new MovementSlot(kind, event)
          if latestMovement.compareAndSet(current, Some(replacement)) then
            inputQueue.offer(QueuedMovement(replacement))
            inputAvailable.release()
          else loop()
    loop()

  private def enqueueNonMovement(input: QueuedInput): Unit =
    latestMovement.set(None)
    inputQueue.offer(input)
    inputAvailable.release()

  @annotation.tailrec
  private def awaitEnqueues(): Unit =
    if enqueuesInFlight.get() != 0 then
      Thread.onSpinWait()
      awaitEnqueues()

  component.addKeyListener(new KeyAdapter:
    override def keyTyped(e: KeyEvent): Unit =
      translateTyped(e).foreach(enqueueInput)
    override def keyPressed(e: KeyEvent): Unit =
      translatePressed(e).foreach(enqueueInput)
    override def keyReleased(e: KeyEvent): Unit =
      translateModifierReleased(e))

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
            button = mouseButton(e),
            renderMetrics = Some(MouseRenderMetrics(currentMetrics, uiMetrics()))
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
    orderedInputStream.collect { case QueuedKey(info) => info }

  def eventStream: Stream[F, Event] =
    orderedInputStream.flatMap {
      case QueuedKey(info)      => inputRouter.eventStream(Stream.emit(info))
      case QueuedMouse(event)   => Stream.emit(event)
      case QueuedMovement(slot) => Stream.emits(slot.claim.toList)
      case QueuedShutdown       => Stream.empty
    }

  private def orderedInputStream: Stream[F, QueuedInput] =
    Stream.eval(Sync[F].delay(shutdownFlag.get())).flatMap {
      case true  => Stream.empty
      case false => Stream.repeatEval(takeInput).takeWhile(_ != QueuedShutdown)
    }

  private def takeInput: F[QueuedInput] =
    Sync[F].map(
      Sync[F].blocking {
        inputAvailable.acquire()
        inputQueue.poll()
      }
    )(input => Option(input).getOrElse(QueuedShutdown))

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
        awaitEnqueues()
        latestMovement.set(None)
        inputQueue.offer(QueuedShutdown)
        inputAvailable.release()
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
    translateModifierPressed(e) match
      case Some(info)               => Some(info)
      case None if isModifierKey(e) => None
      case None =>
        pendingModifierTap.set(None)
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

  private def isModifierKey(e: KeyEvent): Boolean =
    import KeyEvent.*
    e.getKeyCode match
      case VK_CONTROL | VK_ALT | VK_SHIFT | VK_META => true
      case _                                        => false

  private def translateModifierPressed(e: KeyEvent): Option[KeyStrokeInfo] =
    import KeyEvent.*
    val keyType = e.getKeyCode match
      case VK_CONTROL => Some(InputKey.Ctrl)
      case VK_ALT     => Some(InputKey.Alt)
      case VK_SHIFT   => Some(InputKey.Shift)
      case VK_META    => Some(InputKey.Meta)
      case _          => None

    keyType.flatMap { inputKey =>
      val timestamp = e.getWhen
      pendingModifierTap.get match
        case Some((keyCode, previousTimestamp, released))
            if keyCode == e.getKeyCode && released && timestamp >= previousTimestamp &&
              timestamp - previousTimestamp <= doubleTapWindowMillis =>
          pendingModifierTap.set(None)
          Some(KeyStrokeInfo(inputKey, None, Set.empty))
        case Some((keyCode, _, false)) if keyCode == e.getKeyCode =>
          None
        case _ =>
          pendingModifierTap.set(Some((e.getKeyCode, timestamp, false)))
          None
    }

  private def translateModifierReleased(e: KeyEvent): Unit =
    if isModifierKey(e) then
      pendingModifierTap.get match
        case Some((keyCode, timestamp, false)) if keyCode == e.getKeyCode =>
          pendingModifierTap.set(Some((keyCode, timestamp, true)))
        case _ => ()
