package com.serenity.input

import cats.effect.{Concurrent, Sync}
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.serenity.keystroke.KeyStrokeInfo
import com.serenity.keystroke.events.Event
import fs2.Stream
import java.awt.event.{KeyAdapter, KeyEvent}
import java.util.concurrent.LinkedBlockingQueue

/** Bridges AWT keyboard events on a Swing component to the Lanterna-keyed input pipeline.
 *
 *  KEY_TYPED is used for printable characters (correct for keyboard-layout-aware input).
 *  KEY_PRESSED is used for navigation/function keys and Ctrl+letter combinations.
 */
class SwingInputHandler[F[_] : Sync : Concurrent, E <: Event](
  component: java.awt.Component,
  inputRouter: InputRouter[F, E]
) extends InputHandler[F]:

  private val queue = new LinkedBlockingQueue[KeyStroke]()

  component.addKeyListener(new KeyAdapter:
    override def keyTyped(e: KeyEvent): Unit =
      translateTyped(e).foreach(queue.put)
    override def keyPressed(e: KeyEvent): Unit =
      translatePressed(e).foreach(queue.put)
  )

  def keyStream: Stream[F, KeyStroke] =
    Stream
      .repeatEval(Sync[F].blocking(queue.take()))
      .filter(ks => ks != null && ks.getKeyType != null)

  def keyStrokeInfoStream: Stream[F, KeyStrokeInfo] =
    keyStream.map(KeyStrokeInfo.fromKeyStroke)

  def eventStream: Stream[F, Event] =
    inputRouter.eventStream(keyStream)

  private def translateTyped(e: KeyEvent): Option[KeyStroke] =
    val char = e.getKeyChar
    if !e.isControlDown && !e.isAltDown && char != KeyEvent.CHAR_UNDEFINED && char >= 32 then
      Some(new KeyStroke(char, false, false, e.isShiftDown))
    else None

  private def translatePressed(e: KeyEvent): Option[KeyStroke] =
    import KeyEvent.*
    val ctrl = e.isControlDown
    val alt  = e.isAltDown
    e.getKeyCode match
      case VK_UP         => Some(new KeyStroke(KeyType.ArrowUp, ctrl, alt))
      case VK_DOWN       => Some(new KeyStroke(KeyType.ArrowDown, ctrl, alt))
      case VK_LEFT       => Some(new KeyStroke(KeyType.ArrowLeft, ctrl, alt))
      case VK_RIGHT      => Some(new KeyStroke(KeyType.ArrowRight, ctrl, alt))
      case VK_BACK_SPACE => Some(new KeyStroke(KeyType.Backspace, ctrl, alt))
      case VK_DELETE     => Some(new KeyStroke(KeyType.Delete, ctrl, alt))
      case VK_ENTER      => Some(new KeyStroke(KeyType.Enter, ctrl, alt))
      case VK_TAB        => Some(new KeyStroke(KeyType.Tab, ctrl, alt))
      case VK_ESCAPE     => Some(new KeyStroke(KeyType.Escape, ctrl, alt))
      case VK_HOME       => Some(new KeyStroke(KeyType.Home, ctrl, alt))
      case VK_END        => Some(new KeyStroke(KeyType.End, ctrl, alt))
      case VK_PAGE_UP    => Some(new KeyStroke(KeyType.PageUp, ctrl, alt))
      case VK_PAGE_DOWN  => Some(new KeyStroke(KeyType.PageDown, ctrl, alt))
      case VK_F1         => Some(new KeyStroke(KeyType.F1, ctrl, alt))
      case VK_F2         => Some(new KeyStroke(KeyType.F2, ctrl, alt))
      case VK_F3         => Some(new KeyStroke(KeyType.F3, ctrl, alt))
      case VK_F4         => Some(new KeyStroke(KeyType.F4, ctrl, alt))
      case VK_F5         => Some(new KeyStroke(KeyType.F5, ctrl, alt))
      case VK_F6         => Some(new KeyStroke(KeyType.F6, ctrl, alt))
      case VK_F7         => Some(new KeyStroke(KeyType.F7, ctrl, alt))
      case VK_F8         => Some(new KeyStroke(KeyType.F8, ctrl, alt))
      case VK_F9         => Some(new KeyStroke(KeyType.F9, ctrl, alt))
      case VK_F10        => Some(new KeyStroke(KeyType.F10, ctrl, alt))
      case VK_F11        => Some(new KeyStroke(KeyType.F11, ctrl, alt))
      case VK_F12        => Some(new KeyStroke(KeyType.F12, ctrl, alt))
      case code if ctrl =>
        // Map Ctrl+letter to a character KeyStroke with ctrlDown=true.
        // VK_A=65, VK_Z=90 — toLower maps to 'a'-'z'.
        val ch = code.toChar.toLower
        if (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') then
          Some(new KeyStroke(ch, true, alt, false))
        else None
      case _ => None
